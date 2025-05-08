library(bibtex)
library(stringr)

# Replace Unicode curly apostrophes/smart quotes with ASCII equivalents
replace_smart_quotes <- function(text) {
  text <- gsub("[\u2018\u2019]", "'", text)  # curly single quotes → '
  text <- gsub("[\u201C\u201D]", '"', text)  # curly double quotes → "
  return(text)
}

input <- "scopus.bib"
output <- "scopus_clean.bib"

# Step 1: Check parseability using bibtex (helpful for debugging)
bib <- tryCatch(read.bib(input), error = function(e) NULL)

if (!is.null(bib)) {
  message("✅ BibTeX structure is parseable by R. Now cleaning for Quarto...")
  
  # Step 2: Read raw lines for manual cleaning
  lines <- readLines(input)
  
  # --- CLEANING STEPS ---
  
  # 1. Remove spaces from BibTeX entry keys
  # e.g., @ARTICLE{Da SilvaSena2022, → @ARTICLE{DaSilvaSena2022,
  lines <- str_replace_all(lines, 
                           "^(@\\w+\\{)([^,]+),", 
                           function(m) {
                             prefix <- sub("^(@\\w+\\{)([^,]+),", "\\1", m)
                             key    <- sub("^(@\\w+\\{)([^,]+),", "\\2", m)
                             key_clean <- gsub(" ", "", key)
                             paste0(prefix, key_clean, ",")
                           }
  )
  
  # 2. Remove problematic fields
  lines <- lines[!grepl("^\\s*(type|publication_stage|source)\\s*=", lines)]
  
  # 3. Remove long/malformed notes
  lines <- gsub("^\\s*note\\s*=\\s*\\{.*?\\},?\\s*$", "", lines)
  
  # 4. Wrap unbraced journal names in curly braces
  lines <- gsub(
    "^(\\s*journal\\s*=\\s*)([^\"{][^,]+)(,?)$",
    "\\1{\\2}\\3",
    lines,
    perl = TRUE
  )
  
  # 5. Escape LaTeX-sensitive characters
  lines <- gsub("([&%_#])", "\\\\\\1", lines)
  lines <- gsub("\\\\(?![a-zA-Z])", "\\\\\\\\", lines, perl = TRUE)
  lines <- gsub("\\$", "\\\\$", lines)
  lines <- replace_smart_quotes(lines)
  
  # Step 3: Write cleaned file
  writeLines(lines, output)
  message("✅ Cleaned BibTeX saved to ", output)
} else {
  stop("❌ Even R could not parse the BibTeX file. Please fix manually.")
}
