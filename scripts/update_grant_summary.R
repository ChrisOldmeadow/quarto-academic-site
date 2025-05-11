# Grant Summary for LaTeX Macros

pacman::p_load(dplyr, stringr, readr, scales, here)

csv_file <- here("grants", "grant_successfull.csv")
tex_file <- here("..", "CV", "grants", "sum_amount.tex")

# Only recompute if CSV is newer or .tex doesn't exist
csv_exists <- file.exists(csv_file)
tex_exists <- file.exists(tex_file)

if (!tex_exists || (csv_exists && file.mtime(csv_file) > file.mtime(tex_file))) {
  message("Grant CSV modified – recomputing grant summary...")
  
  grants_df <- read_csv(csv_file, show_col_types = FALSE) %>%
    mutate(
      amount_clean = str_remove_all(as.character(amount), "[^0-9.]"),
      amount_numeric = as.numeric(amount_clean),
      role_clean = str_to_upper(str_trim(as.character(role)))
    )
  
  sum_total <- sum(grants_df$amount_numeric, na.rm = TRUE)
  sum_ci <- sum(grants_df$amount_numeric[str_detect(grants_df$role_clean, "CI")], na.rm = TRUE)
  sum_ai <- sum(grants_df$amount_numeric[str_detect(grants_df$role_clean, "AI")], na.rm = TRUE)
  
  format_amount <- function(x) paste0("\\textdollar", comma(x, accuracy = 0.01))
  
  writeLines(c(
    glue::glue("\\def\\sumamount{{{format_amount(sum_total)}}}"),
    glue::glue("\\def\\sumamountCI{{{format_amount(sum_ci)}}}"),
    glue::glue("\\def\\sumamountAI{{{format_amount(sum_ai)}}}"),
    ""  # newline at end
  ), tex_file, sep = "\n")
  
  message("Grant summary written to ../CV/grants/sum_amount.tex")
} else {
  message("Grant CSV not modified — skipping regeneration.")
}
