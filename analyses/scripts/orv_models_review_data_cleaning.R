suppressPackageStartupMessages({
  library("tidyverse")
  library("ggthemes")
  library("scales")
  library("janitor")
  library("bib2df")
  library("xlsx")
})

# Define global variables
data_dir <- "./data"


# load the review data extraction results and remove extraneous variables
review_data_compact <- read_delim(
  file.path(
    data_dir,
    "orv_review_compact_data.csv"
  ),
  delim = ";",
  na = c("", "NA")
)

# load the citation data for merging
citation_data <- bib2df(file.path(data_dir, "included_studies.bib"),
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
  ) |>
  #' make titles lower case
  mutate(
    title = str_to_lower(paper_title),
    disease = str_to_lower(disease),
    country_studied_other = str_to_lower(country_studied_other)
  ) |>
  #' contract long entries
  mutate(
    objectives = case_when(
      objectives == "assess_impact_future" ~ "future",
      objectives == "assess_impact_past" ~ "past",
      objectives == "assess_impact_past assess_impact_future" ~ "both"
    )
  ) |>
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
  ) |>
  mutate(
    country_studied = case_when(
      country_studied == "none" ~ "none",
      country_studied == "multiple" ~ country_studied_multiple,
      country_studied == "other" ~ country_studied_other,
      country_studied != "none" &
        country_studied != "multiple" &
        country_studied != "other" ~ country_studied
    )
  ) |>
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
  ) |>
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
        paste(outcome_measured,
          outcome_measured_other,
          sep = ","
        )
      )
    )
  )

# View(review_data_compact_cleaning_step3)



# View(review_data_compact_cleaning_step3)

#' Step 3A
# Remove the "other" and "multiple" columns
review_data_compact_cleaned <- review_data_compact_cleaning_step3 %>%
  select(
    -contains("othe"), #' one column was miss-labelled as "_othe"
    #' instead of "other"
    -c(paper_title, country_studied_multiple)
  )
# View(review_data_compact_cleaned)

# save the cleaned compact dataset
saveRDS(review_data_compact_cleaned,
  file = file.path(data_dir, "review_data_compact_cleaned.rds")
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
  file = file.path(data_dir, "review_data_long_cleaned.rds")
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

# COmbine the citation database and cleaned dataset
compact_data_with_citation_keys <- left_join(review_data_compact_cleaned,
  citation_data_cleaned,
  by = c("title")
)



# save the cleaned data
saveRDS(compact_data_with_citation_keys,
  file = file.path(data_dir, "compact_data_with_citation_keys_cleaned.rds")
)

openxlsx::write.xlsx(x = compact_data_with_citation_keys,
                     file = file.path(data_dir, "included_studies_database.xlsx")
                     )
