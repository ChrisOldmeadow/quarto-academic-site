# Scripts Directory

This folder contains scripts used to automatically update publication data, citation metrics, and grant summaries for both the academic website and the LaTeX CV.

## Scripts

### `update_publications_and_metrics.R`

This R script performs the following tasks:

- Fetches publication data from ORCID using the `rorcid` package.
- Deduplicates entries and extracts metadata (title, journal, authors, etc.).
- Retrieves Crossref citation counts for articles with DOIs.
- Merges Scimago Journal Rank (SJR) and H-index data for journals.
- Generates:
  - A formatted markdown publication list (`journal_publications.qmd`) for use in Quarto sites.
  - A BibTeX file (`journal_output.bib`) used in the LaTeX CV.
  - Summary LaTeX macros with total publication count, h-index, i10-index, and total citation count for use in the CV (`metrics.tex`, `sum_pubs.tex`).

These outputs are written into the `../CV/publications/` directory.

### `update_grant_summary.R`

This R script:

- Reads a manually maintained CSV file at `grants/grant_successfull.csv`.
- Cleans and parses grant funding amounts.
- Separates Chief Investigator (CI) and Associate Investigator (AI) roles.
- Computes total awarded amounts overall and by role.
- Outputs a LaTeX macro file (`sum_amount.tex`) with the formatted amounts to `../CV/grants/`.

The script only re-runs if the CSV has been modified more recently than the output `.tex` file.

## Notes

- These scripts are invoked by the project `Makefile`.
- They use local caching where appropriate to avoid repeated API calls.
- External files:
  - `scimagojr 2024.csv` (for journal impact metrics) should be placed in the project root.
  - `grant_successfull.csv` should be located in the `grants/` subfolder.
