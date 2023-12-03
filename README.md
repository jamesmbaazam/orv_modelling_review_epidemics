# Modelling outbreak response impact in human vaccine-preventable diseases: A systematic review of differences in practices between collaboration types before COVID-19

This repo contains the data and scripts, and manuscript for a systematic review of differences in modelling practices between academic and mixed modelling groups that developed mechanistic models for evaluating the impact of outbreak response of human vaccine-preventable diseases and foot and mouth disease in livestock during 1970-2019.

See the final published version [here](https://www.sciencedirect.com/science/article/pii/S1755436523000567). 

# How to produce the results

Run the following scripts in that order:

1. `analyses/scripts/orv_models_review_data_cleaning.R`: generates the data needed.
2. `analyses/scripts/S2_File.Rmd`: generates the raw analyses reported in the Results section.
3. `analyses/scripts/S4_Table.Rmd`: generates the tables of the database of included studies and extracted data.
