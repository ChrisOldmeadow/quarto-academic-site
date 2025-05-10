
pacman::p_load(scholar, bibtex)

# Pull metrics


author_id <- "Lo70aGIAAAAJ"

# Get profile
author_info <- get_profile(author_id)

# Extract metrics
h_index <- author_info$h_index
i10_index <- author_info$i10_index
total_cites <- author_info$total_cites

pubs <- read.bib("../scopus_clean.bib")
n_pubs <- length(pubs)  # number of publications

# Load grant data
grants <- read.csv("../grant_successfull.csv")

# Clean and convert 'amount' column
grants$amount <- as.numeric(gsub("[^0-9.]", "", as.character(grants$amount)))

# Total funding
total_funding <- sum(grants$amount, na.rm = TRUE)

# By role
ci_funding <- sum(grants$amount[grants$role == "CI"], na.rm = TRUE)
ai_funding <- sum(grants$amount[grants$role == "AI"], na.rm = TRUE)

# Format to dollars
fmt_dollar <- function(x) paste0("$", format(round(x), big.mark = ","))


# Assemble metrics with icons
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
    format(total_cites, big.mark = ","),
    h_index,
    i10_index,
    fmt_dollar(total_funding),
    fmt_dollar(ci_funding),
    fmt_dollar(ai_funding)
  )
)

# Write markdown table
output_file <- "impact_summary.md"
cat("## Research Impact Summary\n\n", file = output_file)
cat("| Metric             | Value                    |\n", file = output_file, append = TRUE)
cat("|--------------------|--------------------------|\n", file = output_file, append = TRUE)

for (i in 1:nrow(metrics)) {
  cat(sprintf("| %s | %s |\n", metrics$Metric[i], metrics$Value[i]), file = output_file, append = TRUE)
}

message("✅ impact_summary.md updated.")
