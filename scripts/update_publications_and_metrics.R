# Integrated Metrics Pipeline

# Summary:
# Fetches publications from ORCID
# Adds citation counts (Crossref)
# Adds journal metrics (SJR, H-index)
# Writes:
#  journal_output.bib → ../CV/publications/
#  sum_pubs.tex, metrics.tex → ../CV/publications/

# Load required packages
pacman::p_load(rorcid, dplyr, purrr, tibble, glue, memoise, here, stringr, rcrossref, readr, scales, scholar)

# ORCID ID
orcid_id <- "0000-0001-6104-1322"

#token <- rorcid::orcid_auth()
#access_token <- sub("^Bearer\\s+", "", token)

if (Sys.getenv("ORCID_TOKEN") == "") {
  stop("❌ ORCID_TOKEN not set. Please add it to your .Renviron file or export it in your shell.")
}


# Fallback operator
`%||%` <- function(x, y) if (!is.null(x)) x else y

# Unicode stripper
strip_unicode <- function(text) {
  out <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  ifelse(is.na(out), "", out)
}

# Memoised fetcher with BibTeX citation parsing
cache_dir <- here("..", ".orcid_cache")
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

# Deduplicate from ORCID
works <- orcid_works(orcid_id)
df <- as.data.frame(works[[1]]$works)

# Dedup by title
put_codes <- df %>%
  mutate(title = title.title.value) %>%
  group_by(title) %>%
  slice(1) %>%
  ungroup() %>%
  pull(`put-code`)

bib_df <- purrr::map2_dfr(
  put_codes,
  seq_along(put_codes),
  ~ tryCatch(fetch_entry(orcid_id, .x), error = function(e) NULL)
)

bib_df <- bib_df %>% mutate(year = as.integer(year))

# Get citation counts from Crossref
# Safe wrapper around cr_citation_count
safe_citations <- safely(function(doi) {
  result <- cr_citation_count(doi)
  return(result$count)
}, otherwise = NA_integer_)

# Throttled fetcher with progress
get_crossref_citations_throttled <- function(doi, i = NA, total = NA) {
  if (is.na(doi) || doi == "") return(NA_integer_)  # <- handle missing DOIs
  if (!is.na(i)) {
    message(glue("Fetching citations {i} of {total}: {doi}"))
  }
  result <- safe_citations(doi)
  Sys.sleep(0.1)  # be polite to Crossref API
  return(result$result)
}
bib_df <- bib_df %>%
  mutate(citation_count = map2_int(doi, row_number(), ~ get_crossref_citations_throttled(.x, .y, n())))

# Separate categories
bib_df <- bib_df %>% mutate(year = as.integer(year))
journal_df <- bib_df %>% filter(type == "journal-article", str_to_lower(journal) != "medrxiv")
preprint_df <- bib_df %>% filter(str_to_lower(journal) == "medrxiv")
sap_df <- bib_df %>% filter(str_detect(str_to_lower(title), "statistical analysis plan"))



# Merge Scimago
scimago <- read_delim(here("data", "scimagojr 2024.csv"), delim = ";", show_col_types = FALSE)
scimago_clean <- scimago %>%
  select(journal = `Title`, H_index = `H index`, SJR_Best_Quartile = `SJR Best Quartile`) %>%
  mutate(journal = str_to_lower(str_trim(journal)))

bib_df <- bib_df %>%
  mutate(journal_clean = str_to_lower(str_trim(journal))) %>%
  left_join(scimago_clean, by = c("journal_clean" = "journal"))


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


writeLines(final_md, here("data", "journal_publications.qmd"))




# Function to strip or transliterate Unicode characters
strip_unicode <- function(text) {
  iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
}

# BibTeX key generator
make_bibtex_key <- function(authors, year, title) {
  first_author <- strsplit(authors, ",\\s*")[[1]][1]
  cleaned_author <- gsub("[^A-Za-z]", "", tolower(first_author))
  cleaned_title <- gsub("\\W+", "", tolower(substr(title, 1, 5)))
  paste0(cleaned_author, year, cleaned_title)
}

# Generate keys and clean citation text
bib_entries <- journal_df %>%
  mutate(
    bib_key = pmap_chr(list(authors, year, title), make_bibtex_key),
    citation_clean = strip_unicode(citation_text),
    fixed_citation = str_replace(citation_clean, "@\\w+\\{.*?,", paste0("@article{", bib_key, ","))
  ) %>%
  pull(fixed_citation)


writeLines(bib_entries, here("../CV/publications/journal_output.bib"))

# Write publication summary for LaTeX
cat(glue("\\def\\numpubs{{{nrow(bib_df)}}}"), file = here("../CV/publications/sum_pubs.tex"))

# Extract scholar metrics
author_info <- scholar::get_profile("Lo70aGIAAAAJ&")
cat(glue("\\def\\hindex{{{author_info$h_index}}}\n",
         "\\def\\i10index{{{author_info$i10_index}}}\n",
         "\\def\\totalcites{{{author_info$total_cites}}}"),
    file = here("../CV/publications/metrics.tex"))


