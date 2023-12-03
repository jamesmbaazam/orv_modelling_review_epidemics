# Modelling outbreak response impact in human vaccine-preventable diseases: A systematic review of differences in practices between collaboration types before COVID-19

This repo contains the data and scripts, and manuscript for a systematic review of differences in modelling practices between academic and mixed modelling groups that developed mechanistic models for evaluating the impact of outbreak response of human vaccine-preventable diseases and foot and mouth disease in livestock during 1970-2019.

This manuscript is published in Epidemics. See [here](https://www.sciencedirect.com/science/article/pii/S1755436523000567). 

## Abstract

### Background

Outbreak response modelling often involves collaboration among academics, and experts from governmental and non-governmental organizations. We conducted a systematic review of modelling studies on human vaccine-preventable disease (VPD) outbreaks to identify patterns in modelling practices between two collaboration types. We complemented this with a mini comparison of foot-and-mouth disease (FMD), a veterinary disease that is controllable by vaccination.

### Methods

We searched three databases for modelling studies that assessed the impact of an outbreak response. We extracted data on author affiliation type (academic institution, governmental, and non-governmental organizations), location studied, and whether at least one author was affiliated to the studied location. We also extracted the outcomes and interventions studied, and model characteristics. Included studies were grouped into two collaboration types: purely academic (papers with only academic affiliations), and mixed (all other combinations) to help investigate differences in modelling patterns between collaboration types in the human disease literature and overall differences with FMD collaboration practices.

### Results

Human VPDs formed 227 of 252 included studies. Purely academic collaborations dominated the human disease studies (56%). Notably, mixed collaborations increased in the last seven years (2013–2019). Most studies had an author affiliated to an institution in the country studied (75.2%) but this was more likely among the mixed collaborations. Contrasted to the human VPDs, mixed collaborations dominated the FMD literature (56%). Furthermore, FMD studies more often had an author with an affiliation to the country studied (92%) and used complex model design, including stochasticity, and model parametrization and validation.

### Conclusion

The increase in mixed collaboration studies over the past seven years could suggest an increase in the uptake of modelling for outbreak response decision-making. We encourage more mixed collaborations between academic and non-academic institutions and the involvement of locally affiliated authors to help ensure that the studies suit local contexts.

## How to produce the results

Run the following scripts in that order:

1. `analyses/scripts/orv_models_review_data_cleaning.R`: generates the data needed.
2. `analyses/scripts/S2_File.Rmd`: generates the raw analyses reported in the Results section.
3. `analyses/scripts/S4_Table.Rmd`: generates the tables of the database of included studies and extracted data.
