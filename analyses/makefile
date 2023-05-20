SCRIPTDIR := scripts
FIGDIR := figs
DATADIR := data

$(DATADIR)/compact_data_with_citation_keys_cleaned.rds : \
$(SCRIPTDIR)/orv_models_review_data_cleaning.R \
$(DATADIR)/orv_review_compact_data.csv \
$(DATADIR)/included_studies.bib
	Rscript $^ $@

