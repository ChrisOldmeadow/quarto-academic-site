# Load required packages
pacman::p_load(rorcid, dplyr, purrr, tibble, glue, memoise, here, stringr, rcrossref)

# ORCID ID
orcid_id <- "0000-0001-6104-1322"

# Safe fallback operator
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Memoised fetcher using BibTeX parsing
cache_dir <- ".orcid_cache"
fs_cache <- cache_filesystem(cache_dir)

fetch_entry <- memoise::memoise(function(orcid_id, put_code) {
  message(glue("Fetching put-code {put_code}..."))
  data <- rorcid::orcid_works(orcid_id, put_code)[[1]][[1]]
  if (nrow(data) == 0) return(NULL)
  work <- data[1, ]
  
  citation_text <- work[["citation"]][["citation-value"]]
  
  # Pull DOI from structured external-ids instead of citation-text
  external_ids <- work$`external-ids`$`external-id`[[1]]
  doi_val <- NA_character_
  if (!is.null(external_ids)) {
    doi_row <- external_ids[external_ids$`external-id-type` == "doi", ]
    if (nrow(doi_row) > 0) {
      doi_val <- doi_row$`external-id-value`
    }
  }
  
  tibble(
    title   = work$title$title$value,
    journal = work[["journal-title"]][["value"]],
    year    = work[["publication-date"]][["year"]][["value"]],
    type    = work$type,
    authors = paste(work$contributors$contributor[[1]]$`credit-name.value`, collapse = ", "),
    volume  = str_match(citation_text, "volume = \\{(.*?)\\}")[,2],
    issue   = str_match(citation_text, "number = \\{(.*?)\\}")[,2],
    pages   = str_match(citation_text, "pages = \\{(.*?)\\}")[,2],
    doi     = doi_val,
    citation_text = citation_text,
    put_code = put_code
  )
}, cache = fs_cache)

# Get deduplicated work entries
works <- orcid_works(orcid_id)
df <- as.data.frame(works[[1]]$works)

# Deduplicate by title
df_dedup <- df %>%
  mutate(
    title = title.title.value,
    year = `publication-date.year.value`,
    journal = `journal-title.value`,
    source = `source.source-name.value`,
    doi = purrr::map_chr(`external-ids.external-id`, function(x) {
      if (is.null(x)) return(NA_character_)
      doi_row <- x[x$`external-id-type` == "doi", ]
      if (nrow(doi_row) > 0) return(doi_row$`external-id-value`) else return(NA_character_)
    }),
    has_doi = !is.na(doi),
    modified = `last-modified-date.value`,
    type = type
  )

df_unique <- df_dedup %>%
  arrange(title, desc(has_doi), desc(source == "Crossref"), desc(modified)) %>%
  group_by(title) %>%
  slice(1) %>%
  ungroup()

put_codes <- df_unique$`put-code`

# Fetch Bib entries
bib_df <- purrr::map2_dfr(
  put_codes,
  seq_along(put_codes),
  ~ tryCatch({
    message(glue("Fetching {.y} of {length(put_codes)} (put-code: {.x})..."))
    fetch_entry(orcid_id, .x)
  }, error = function(e) {
    message(glue("Error fetching put-code {.x}: {e$message}"))
    NULL
  })
)

# Separate categories
bib_df <- bib_df %>% mutate(year = as.integer(year))
journal_df <- bib_df %>% filter(type == "journal-article", str_to_lower(journal) != "medrxiv")
preprint_df <- bib_df %>% filter(str_to_lower(journal) == "medrxiv")
sap_df <- bib_df %>% filter(str_detect(str_to_lower(title), "statistical analysis plan"))


# get some impact metrics

# journal imapct:
scimago <- read_delim("scimagojr 2024.csv", delim = ";")
scimago_clean <- scimago %>%
  select(journal = `Title`, H_index = `H index`, SJR_Quartile = `SJR Best Quartile`) %>%
  mutate(journal = str_to_lower(str_trim(journal)))



# Safe wrapper around cr_citation_count
safe_citations <- safely(function(doi) {
  result <- cr_citation_count(doi)
  return(result$count)
}, otherwise = NA_integer_)

# Throttled fetcher with progress
get_crossref_citations_throttled <- function(doi, i = NA, total = NA) {
  if (!is.na(i)) {
    message(glue("Fetching citations {i} of {total}: {doi}"))
  }
  result <- safe_citations(doi)
  Sys.sleep(0.1)  # Respectful of API limits
  return(result$result)
}

# Apply to bib_df with progress
bib_df <- bib_df %>%
  mutate(citation_count = map2_int(
    doi, row_number(),
    ~ get_crossref_citations_throttled(.x, .y, n())
  ))


# Apply with progress tracking
total_dois <- nrow(bib_df)
bib_df <- bib_df %>%
  mutate(citation_count = purrr::map2_int(doi, row_number(), ~ get_crossref_citations_throttled(.x, .y, total_dois)))

bib_df <- bib_df %>%
  mutate(journal_clean = str_to_lower(str_trim(journal))) %>%
  left_join(scimago_clean %>%
              rename(SJR_Best_Quartile = SJR_Quartile),
            by = c("journal_clean" = "journal"))


journal_df <- bib_df %>%
  filter(type == "journal-article", str_to_lower(journal) != "medrxiv") %>%
  mutate(citation_count = ifelse(is.na(citation_count), 0, citation_count))

# Citation formatter with citation counts

format_citation <- function(authors, year, title, journal, doi,
                            volume = NA, issue = NA, pages = NA,
                            citation_count = NA,
                            SJR_Best_Quartile = NA, H_index = NA) {
  author_list <- str_split(authors, ",\\s*")[[1]]
  author_display <- if (length(author_list) > 5) {
    paste(paste(author_list[1:5], collapse = ", "), "et al.")
  } else {
    paste(author_list, collapse = ", ")
  }
  
  # Format volume/issue/pages
  details <- ""
  if (!is.na(volume) && nzchar(volume)) {
    details <- volume
    if (!is.na(issue) && nzchar(issue)) {
      details <- glue("{details}({issue})")
    }
  }
  if (!is.na(pages) && nzchar(pages)) {
    details <- if (nzchar(details)) glue("{details}, {pages}") else pages
  }
  
  journal_str <- if (!is.na(journal) && nzchar(journal)) {
    detail_suffix <- if (nzchar(details)) glue(", {details}") else ""
    glue("*{journal}*{detail_suffix}.")
  } else {
    NULL
  }
  
  # Build right-aligned badge
  badge_parts <- c()
  if (!is.na(citation_count) && citation_count > 0) {
    badge_parts <- c(badge_parts, glue("🛈 Cites: {citation_count}"))
  }
  if (!is.na(SJR_Best_Quartile) && nzchar(SJR_Best_Quartile)) {
    badge_parts <- c(badge_parts, glue("🏅 {SJR_Best_Quartile}"))
  }
  if (!is.na(H_index) && H_index > 0) {
    badge_parts <- c(badge_parts, glue("H-Index: {H_index}"))
  }
  badge_html <- if (length(badge_parts) > 0) {
    glue('<span style="float: right; font-size: 0.85em; color: #555;">{paste(badge_parts, collapse = " &nbsp; ")}</span>')
  } else {
    ""
  }
  
  main_content <- c(
    glue("{author_display} ({year})."),
    glue("*{title}*"),
    journal_str,
    if (!is.na(doi) && nzchar(doi)) glue("[https://doi.org/{doi}](https://doi.org/{doi})") else NULL
  )
  
  glue('<div style="overflow: hidden;">{badge_html}<div>{paste(main_content[!sapply(main_content, is.null)], collapse = "<br/>")}</div></div>')
}

# Format journal articles grouped by year
journal_grouped <- journal_df %>%
  arrange(desc(year)) %>%
  mutate(
    formatted = pmap_chr(
      list(authors, year, title, journal, doi, volume, issue, pages,
           citation_count, SJR_Best_Quartile, H_index),
      format_citation
    ),
    pub_number = rev(seq_len(n())),
    numbered = glue("{pub_number}. {formatted}")
  ) %>%
  group_by(year) %>%
  summarise(entries = paste0("- ", numbered, collapse = "\n\n"), .groups = "drop") %>%
  arrange(desc(year)) %>%
  mutate(section = glue("### {year}\n\n{entries}")) %>%
  pull(section) %>%
  paste(collapse = "\n\n")

# Format preprints (not grouped)
preprint_section <- preprint_df %>%
  arrange(desc(year)) %>%
  mutate(formatted = pmap_chr(
    list(authors, year, title, journal, doi, volume, issue, pages),
    format_citation
  )) %>%
  mutate(entry = glue("- {formatted}")) %>%
  pull(entry) %>%
  paste(collapse = "\n\n")

# Format SAPs (not grouped)
sap_section <- sap_df %>%
  arrange(desc(year)) %>%
  mutate(formatted = pmap_chr(
    list(authors, year, title, journal, doi, volume, issue, pages),
    format_citation
  )) %>%
  mutate(entry = glue("- {formatted}")) %>%
  pull(entry) %>%
  paste(collapse = "\n\n")

final_md <- paste(
  "## Journal Articles", journal_grouped,
  "\n\n## Preprints", preprint_section,
  "\n\n## Statistical Analysis Plans", sap_section,
  sep = "\n\n"
)


writeLines(final_md, here("journal_publications.qmd"))


# Function to strip or transliterate Unicode characters
strip_unicode <- function(text) {
  iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
}

# Step 1: BibTeX key generator
make_bibtex_key <- function(authors, year, title) {
  first_author <- strsplit(authors, ",\\s*")[[1]][1]
  cleaned_author <- gsub("[^A-Za-z]", "", tolower(first_author))
  cleaned_title <- gsub("\\W+", "", tolower(substr(title, 1, 5)))
  paste0(cleaned_author, year, cleaned_title)
}

# Step 2: Generate keys and clean citation text
bib_entries <- journal_df %>%
  mutate(
    bib_key = pmap_chr(list(authors, year, title), make_bibtex_key),
    citation_clean = strip_unicode(citation_text),
    fixed_citation = str_replace(citation_clean, "@\\w+\\{.*?,", paste0("@article{", bib_key, ","))
  ) %>%
  pull(fixed_citation)


# Step 3: Write to file

writeLines(bib_entries, "../CV/publications/journal_output.bib")


