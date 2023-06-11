# I/O paths
data_dir <- "./data"

.args <- if (interactive()) {
  c(
    here::here(data_dir, "orv_review_compact_data.csv"),
    here::here(data_dir, "included_studies.bib"),
    here::here(data_dir, "compact_data_with_citation_keys_cleaned.rds")
  )
} else {
  commandArgs(trailingOnly = TRUE)
}


suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(bib2df)
  library(xlsx)
  library(here)
})

# load the review data extraction results and remove extraneous variables
review_data_compact <- read_delim(
  here(.args[1]),
  delim = ";",
  na = c("", "NA")
)

#' Further papers to be removed
#' 1. On the Regional Control of a Reaction–Diffusion System SIR:
    #' Reason: about Ebola in Gorillas
#' 2. Analysis of a tb model with treatment interruptions:
  #' Reason: Duplicate of "A Mathematical Study of a TB Model with Treatment
  #' Interruptions and Two Latent Periods"
exclusions <- c("On the Regional Control of a Reaction–Diffusion System SIR",
              "Analysis of a TB model with treatment interruptions"
              )
review_data_compact <- review_data_compact %>%
  filter(!paper_title %in% exclusions)

# load the citation data for merging
citation_data <- bib2df(here(.args[2]),
  separate_names = FALSE
) %>%
  mutate(year = as.numeric(YEAR))

############################################
# Data cleaning
############################################




# Step 1
#' Remove extraneous variables and clean the column names
#' the compact data is the same as above except that there are multiple
#' entries per cell;
#' because that is how KoboToolbox saves multiple answer questions
review_data_compact_cleaning_step1 <- review_data_compact %>%
  # remove 2020 because the search was
  #' done in Jan 2020 so it gives the illusion that there were few papers
  filter(publication_year < 2020) %>%
  #' in 2020.
  #' select(-'_index') %>%
  clean_names() %>%
  remove_empty("cols")

#' Step 2
#' i. change column names to lower case
#' ii. Rename/shorten entries
#' iii. "Unite" some columns
#' iv. replace 'NA' entries with 'not_applicable'
review_data_compact_cleaning_step2 <- review_data_compact_cleaning_step1 %>%
  # Replace spaces in entries with underscore
  mutate(
    country_studied_other =
      str_replace_all(country_studied_other, " ", "_")
  ) %>%
  #' make titles lower case
  mutate(
    title = str_to_lower(paper_title),
    disease = str_to_lower(disease),
    country_studied_other = str_to_lower(country_studied_other)
  ) %>%
  #' contract long entries
  mutate(
    objectives = case_when(
      objectives == "assess_impact_future" ~ "future",
      objectives == "assess_impact_past" ~ "past",
      objectives == "assess_impact_past assess_impact_future" ~ "both"
    )
  ) %>%
  # if entry is NA, change it to 'not_applicable' for easy analysis in R
  mutate(
    author_in_country_studied =
      if_else(is.na(author_in_country_studied),
        "not_applicable",
        author_in_country_studied
      ),
    is_vax_effective =
      if_else(
        is.na(is_vax_effective),
        "not_applicable",
        is_vax_effective
      ),
    data_available =
      if_else(
        is.na(data_available),
        "not_applicable",
        data_available
      )
  ) %>%
  mutate(
    country_studied = case_when(
      country_studied == "none" ~ "none",
      country_studied == "multiple" ~ country_studied_multiple,
      country_studied == "other" ~ country_studied_other,
      country_studied != "none" &
        country_studied != "multiple" &
        country_studied != "other" ~ country_studied
    )
  ) %>%
  mutate(
    intervention_modelled = str_replace_all(
      intervention_modelled,
      " ",
      ","
    ),
    outcome_measured = str_replace_all(
      outcome_measured,
      " ",
      ","
    )
  ) %>%
  # make some columns factors
  mutate(across(
    .cols = c(
      author_in_country_studied,
      country_studied,
      is_vax_effective,
      data_available
    ),
    .fns = ~ as.factor(.x)
  ))


#' step 2a. Convert string columns to factor

# View(review_data_compact_cleaning_step2)


#' Step 3
#' Combine the intervention and outcome columns, replace the commas with
#' space, and remove the "other" variables
review_data_compact_cleaning_step3 <- review_data_compact_cleaning_step2 %>%
  # trim/remove white space from both ends of the variables
  mutate(
    intervention_modelled_other = str_trim(intervention_modelled_other,
      side = "both"
      ),
    outcome_measured_other = str_trim(outcome_measured_other,
      side = "both"
      )
  ) %>%
  # make lower case
  mutate(
    intervention_modelled_other = str_to_lower(intervention_modelled_other),
    outcome_measured_other = str_to_lower(outcome_measured_other)
    ) %>%
  #remove the spaces after "," before next operation
  mutate(
    intervention_modelled_other = str_replace_all(
      intervention_modelled_other,
      ", ", ","
    ),
    outcome_measured_other = str_replace_all(
      outcome_measured_other,
      ", ", ","
    )
  ) %>%
  # replace spaces between words with an underscore
  mutate(
    intervention_modelled_other = str_replace_all(
      intervention_modelled_other,
      " ", "_"
    ),
    outcome_measured_other = str_replace_all(
      outcome_measured_other,
      " ", "_"
    )
  ) %>%
  # reconstruct the intervention_modelled and outcomes_measured columns to include those recorded in the "other" column. The "other" columns will be removed later
  mutate(
    intervention_modelled = case_when(
      is.na(intervention_modelled_other) ~ intervention_modelled,
      !is.na(intervention_modelled_other) ~ str_to_lower(
        paste(intervention_modelled,
          intervention_modelled_other,
          sep = ","
        )
      )
    ),
    outcome_measured = case_when(
      is.na(outcome_measured_other) ~ outcome_measured,
      !is.na(outcome_measured_other) ~ str_to_lower(
        paste0(outcome_measured, ",", outcome_measured_other)
      )
    )
  )

#' Step 3A
# Remove the "other" and "multiple" columns
review_data_compact_cleaned <- review_data_compact_cleaning_step3 %>%
  select(
    -contains("othe"), #' one column was miss-labelled as "_othe"
    #' instead of "other"
    -c(paper_title, country_studied_multiple)
  )

# save the cleaned compact dataset
saveRDS(review_data_compact_cleaned,
  file = here(data_dir, "review_data_compact_cleaned.rds")
)



#' Step 4
#' Separate columns with multiple entries into single rows and
#' remove unwanted rows
review_data_wide_to_long <- review_data_compact_cleaned %>%
  separate_rows(author_affiliation_type, sep = " ") %>%
  separate_rows(disease, sep = " ") %>%
  separate_rows(model_structure, sep = " ") %>%
  separate_rows(parametrization, sep = " ") %>%
  separate_rows(validation, sep = " ") %>%
  separate_rows(country_studied, sep = " ") %>%
  separate_rows(intervention_modelled, sep = ",") %>%
  separate_rows(outcome_measured, sep = ",") %>%
  filter(outcome_measured != "outcome_other") %>%
  filter(intervention_modelled != "intervention_other")

# save the cleaned data
saveRDS(review_data_wide_to_long,
  file = here(data_dir, "review_data_long_cleaned.rds")
)

# Clean the citation database
citation_data_cleaned <- citation_data %>%
  remove_empty(which = c("rows", "cols")) %>%
  clean_names() %>%
  select(-c(
    "address",
    "abstract",
    "annote",
    "booktitle",
    "type",
    "isbn",
    "issn",
    "file",
    "keywords",
    "mendeley_tags",
    "pmid",
    "url",
    "archiveprefix",
    "arxivid",
    "eprint",
    "year_2"
  )) %>%
  mutate(
    category = str_to_lower(category),
    title = str_to_lower(title),
    year = as.numeric(year)
  )

# Combine the citation database and cleaned dataset
compact_data_with_citation_keys <- left_join(review_data_compact_cleaned,
  citation_data_cleaned,
  by = c("title")
)


# Clean individual variables
compact_data_with_citation_keys <- compact_data_with_citation_keys %>%
  arrange(publication_year) %>%
  mutate(vax_modelled = ifelse(
    str_detect(intervention_modelled, 'vaccination'), 'yes', 'no')) %>%
  mutate(
    intervention_type = case_when(
    # When vax was not modelled at all
    vax_modelled == 'no' & vax_modelled_with_non_vax == 'no' ~ 'no_vax',
    # When vax was modelled but not for comparison as a single intervention
    vax_modelled == 'yes' & vax_modelled_with_non_vax == 'yes' ~ 'vax_combination_with_others',
    # When vax was modelled as a single intervention to be compared with others
    vax_modelled == 'yes' & vax_modelled_with_non_vax == 'no' ~ 'vax_single'
    )
    ) %>%
  mutate(
    author_affiliation_type = str_replace_all(author_affiliation_type, " ",  " + ")
    ) %>%
  mutate(
    author_in_country_studied = if_else(author_in_country_studied == 'NA',
                                        'not_applicable',
                                        author_in_country_studied
                                        )
    ) %>%
  #Aggregate years before 2006 into one group because of the low number of papers
  mutate(year_aggreg = as.numeric(
    ifelse(publication_year < 2006, 2005, publication_year)
    )
    ) %>%
  mutate(
  collab_type = as_factor(
    if_else(author_affiliation_type == 'academic_institutions',
            'purely_academic',
            'mixed')
    ),
  purely_academic_collab = if_else(
    author_affiliation_type == 'academic_institutions', TRUE, FALSE)
  ) %>%
  relocate(purely_academic_collab, .after = author_affiliation_type)

# Clean descriptions of parametrization and validation with shorter labels

#' First create dictionary of parametrization methods descriptions and shorter
#' replacements

parametrization_dict <- c('values_from_literature' = 'literature',
                          'values_from_guess_and_expert_opion' = 'expert_opinion',
                          'values_fitted_to_ts' = 'fitted',
                          ' ' = ' and '
                          )

# Dictionary of validation methods descriptions
validation_dict <- c('compared_with_data' = 'data',
                     'compared_with_other_model_output' = 'another_model',
                     'model_not_validated' = 'none',
                     ' ' = '_and_'
                     )

compact_data_with_citation_keys <- compact_data_with_citation_keys %>%
  mutate(parametrization = str_replace_all(parametrization,
                                           parametrization_dict
                                           )
         ) %>%
  mutate(validation = str_replace_all(validation, validation_dict)) %>%
  mutate(model_structure = str_replace(model_structure,
                                       'deterministic stochastic',
                                       'both'
                                       )
         )


#Rearrange the columns
compact_data_with_citation_keys <- compact_data_with_citation_keys %>%
  relocate(c(title, bibtexkey), .before = publication_year) %>%
  relocate(publication_year, .before = bibtexkey) %>%
  relocate(c(x0_reviewer, category, author:language),
           .after = collab_type
  ) %>%
  arrange(title)



# save the cleaned data
saveRDS(compact_data_with_citation_keys, tail(.args, 1))


