.PHONY: all update_pubs update_grants update_impact build_cv build_site deploy_site

all: update_pubs update_grants update_impact build_cv build_site

update_pubs:
	Rscript scripts/update_publications_and_metrics.R

update_grants:
	Rscript scripts/update_grant_summary.R

update_impact:
	Rscript scripts/update_impact_summary.R

build_cv:
	cd ../CV && latexmk -pdf -bibtex -quiet -f CV-Oldmeadow.tex

build_site:
	quarto render

deploy_site:
	NETLIFY_AUTH_TOKEN=$$NETLIFY_AUTH_TOKEN quarto publish netlify --no-browser
