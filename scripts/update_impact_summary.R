pacman::p_load(readr, glue, here, scales)

# Load previously written LaTeX macros
metrics_tex <- readLines(here("..", "CV", "publications" , "metrics.tex"))
grants_tex <- readLines(here("..", "CV" , "grants" , "sum_amount.tex"))

# Extract numeric values from LaTeX definitions
extract_macro <- function(lines, name) {
  pattern <- glue("\\\\def\\\\{name}\\{{(.*?)\\}}")
  match <- stringr::str_match(lines, pattern)
  na.omit(match[, 2])[1]
}

h_index <- extract_macro(metrics_tex, "hindex")
i10_index <- extract_macro(metrics_tex, "i10index")
total_cites <- extract_macro(metrics_tex, "totalcites")

sumamount <- extract_macro(grants_tex, "sumamount")
sumamountCI <- extract_macro(grants_tex, "sumamountCI")
sumamountAI <- extract_macro(grants_tex, "sumamountAI")

# Count publications from bib file
bib_file <- here("..", "CV", "publications", "journal_output.bib")
n_pubs <- length(readLines(bib_file)[grepl("^@article\\{", readLines(bib_file))])

strip_tex_dollar <- function(x) {
  sub("^\\\\textdollar", "\\$", x)
}

fmt_number <- function(x) scales::comma(x, accuracy = 1)


# Format output
metrics <- data.frame(
  Metric = c(
    "📚 Publications",
    "📈 Citations (GS)",
    "🔬 h-index",
    "🎯 i10-index",
    "💰 Grants (Total)",
    "💼 CI Funding",
    "🤝 AI Funding"
  ),
  Value = c(
    paste(n_pubs, "peer-reviewed articles"),
    fmt_number(as.numeric(total_cites)),
    h_index,
    i10_index,
    strip_tex_dollar(sumamount),
    strip_tex_dollar(sumamountCI),
    strip_tex_dollar(sumamountAI)
  )
)

# Write markdown table
output_file <- here("data", "impact_summary.md")
cat("## Research Impact Summary\n\n", file = output_file)
cat("| Metric             | Value                    |\n", file = output_file, append = TRUE)
cat("|--------------------|--------------------------|\n", file = output_file, append = TRUE)

for (i in 1:nrow(metrics)) {
  cat(sprintf("| %s | %s |\n", metrics$Metric[i], metrics$Value[i]), file = output_file, append = TRUE)
}

message("✅ impact_summary.md updated.")
