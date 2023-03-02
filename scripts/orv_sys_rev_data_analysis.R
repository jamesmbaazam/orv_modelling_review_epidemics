library('tidyverse')
library('ggthemes')
library('scales')
library('janitor')
library('forcats')
library('stringr')

#load the data and remove the Kobotoolbox meaningless variables 
review_data_wide <- readxl::read_excel(normalizePath('./data/final_data/2021_02_25_wide.xlsx')) 
review_data_compact <- readxl::read_excel(normalizePath('./data/final_data/2021_02_25_compact.xlsx')) 


############################################
#Data cleaning
############################################


review_data_wide_cleaned <- review_data_wide %>% 
    filter(publication_year < 2020) %>% #remove 2020 because the search was done in Jan 2020 so it gives the illusion that there were few papers in 2020.
    select(-c('start':'phonenumber'),
           -c('__version__':'_notes')
           ) %>% 
    clean_names() %>% 
    remove_empty('cols')

review_data_compact_cleaned <- review_data_compact %>% 
  filter(publication_year < 2020) %>% #remove 2020 because the search was done in Jan 2020 so it gives the illusion that there were few papers in 2020.
  select(-c('start':'phonenumber'),
         -c('__version__':'_notes')
         ) %>% 
  clean_names() %>% 
  remove_empty('cols')

#' Primary objectives 
#' 
#' 
#' 1. Is modelling being used to inform policy? NB: pm = policy making

# 1A. Get relevant variables

pm_data <- review_data_compact_cleaned %>%
  select(
    paper_title,
    publication_year,
    starts_with("objectives"),
    starts_with("author_affiliation_type"),
    starts_with('disease'),
    author_in_country_studied,
    outbreak_type,
    modelling_timing,
    data_used
  )

# 1B. Create the author affiliation types and reword the string NA
pm_data_mutated <- pm_data %>%
  arrange(publication_year) %>%
  mutate(
    author_affiliation_type = str_replace_all(author_affiliation_type, ' ', ' + '),
    author_in_country_studied = if_else(author_in_country_studied == 'NA', 'not_applicable',
                                        author_in_country_studied
                                        )
    )



#' Count by affiliation type and make a barplot
pm_data_affiliation_aggregated <- pm_data_mutated %>% 
    mutate(author_affiliation_type = as_factor(author_affiliation_type)) %>% 
    count(author_affiliation_type) %>% 
    mutate(perc = round(n/sum(n)*100, 1)) %>% 
    arrange(desc(n))
 
    
affiliation_type_aggregated_barplot <- pm_data_affiliation_aggregated %>%
    ggplot() + geom_bar(aes(x = reorder(author_affiliation_type, perc),
                            y = perc,
                            fill = factor(author_affiliation_type)
                            ),
                        stat = 'identity',
                        color = 'black'
                        ) +
    scale_y_continuous(breaks = seq(0, 60, 5),
                       labels = seq(0, 60, 5)) +
    labs(title = 'Aggregated number of studies per author affiliation type', 
         x = 'Author affiliation type', 
         y = 'Percent') + 
 #   coord_polar(theta = "x", direction = -1) +
    scale_color_tableau()

plot(affiliation_type_aggregated_barplot)


#' Save the plot    
ggsave(filename = './figs/affiliation_type_aggregated_barplot.jpg', 
       plot = affiliation_type_aggregated_barplot,
       width = 10.5,
       height = 6.5,
       units = 'in'
)

#' alternatively, make a treemap

library(treemapify)
affilition_type_aggregated_treemap <- ggplot(pm_data_affiliation_aggregated,
                                                    aes(area = n, 
                                                        label = perc,
                                                        fill = author_affiliation_type
                                                        ),
                                                    color = 'black',
                                                    stroke = 5,
                                                    stat = 'identity',
                                                    color = 'black'
                                                    ) + 
  geom_treemap() +
  geom_treemap_text(place = 'centre') +
  labs(title = 'Aggregated number of studies per author affiliation type') +
  scale_color_tableau()

        

plot(affilition_type_aggregated_treemap)


#' Save the plot    
ggsave(filename = './figs/affilition_type_aggregated_treemap.jpg', 
       plot = affilition_type_aggregated_treemap,
       width = 10.5,
       height = 6.5,
       units = 'in'
)


#' Count the number of each affiliation type per year for only FMD papers
pm_data_affiliation_type_FMD_only_by_year <- pm_data_mutated %>% 
  arrange(publication_year) %>% 
  filter(disease == 'FMD') %>% 
  group_by(publication_year, author_affiliation_type) %>% 
  count(publication_year, author_affiliation_type)


pm_data_affiliation_type_wFMD_by_year <- pm_data_mutated %>% 
    arrange(publication_year) %>% 
    group_by(publication_year, author_affiliation_type) %>% 
    count(publication_year, author_affiliation_type)


#' Count the number of each affiliation type per year (excluding FMD)
pm_data_affiliation_type_no_FMD_by_year <- pm_data_mutated %>% 
  arrange(publication_year) %>% 
  filter(disease != 'FMD') %>% 
  group_by(publication_year, author_affiliation_type) %>% 
  count(publication_year, author_affiliation_type)

#Plot the affiliation type over time (with FMD)
affiliation_type_FMD_only_by_year_barplot <- ggplot(data = pm_data_affiliation_type_FMD_only_by_year) + 
    geom_bar(aes(x = publication_year, 
                 y = n, 
                 fill = author_affiliation_type
                 ),
             color = 'black',
             stat = 'identity',
             position = 'stack'
             ) + 
    scale_x_continuous(limits = c(min(pm_data_affiliation_type_wFMD_by_year$publication_year), 
                                  max(pm_data_affiliation_type_wFMD_by_year$publication_year)
                                  ),
                       breaks = seq(min(pm_data_affiliation_type_wFMD_by_year$publication_year), 
                                    max(pm_data_affiliation_type_wFMD_by_year$publication_year),
                                    5
                                    ),
                       labels = seq(min(pm_data_affiliation_type_wFMD_by_year$publication_year), 
                                  max(pm_data_affiliation_type_wFMD_by_year$publication_year),
                                  5
                                  )
                       ) +
  scale_y_continuous(limits = c(0, 
                                max(pm_data_affiliation_type_wFMD_by_year$n)
                                ), 
                     breaks = seq(0, 
                                  max(pm_data_affiliation_type_wFMD_by_year$n), 
                                  1
                                  ), 
                     labels = seq(0, 
                                  max(pm_data_affiliation_type_wFMD_by_year$n), 
                                  1
                                  )
                     ) +
    labs(title = 'Author affiliation composition over time (FMD only)',
         x = 'Year',
         y = 'Number of publications') +
    scale_color_tableau()

plot(affiliation_type_FMD_only_by_year_barplot)

#' save the plot
ggsave(filename = './figs/affiliation_type_FMD_only_by_year_barplot.jpg', 
       plot = affiliation_type_FMD_only_by_year_barplot,
       width = 10.5,
       height = 6.5,
       units = 'in'
       )


#' Count the number of each author affiliation type per year (including FMD)
pm_data_affiliation_type_wFMD_by_year <- pm_data_mutated %>% 
  arrange(publication_year) %>% 
  group_by(publication_year, author_affiliation_type) %>% 
  count(publication_year, author_affiliation_type)


#Plot the affiliation type over time (with FMD)

ggplot(data = pm_data_affiliation_type_wFMD_by_year, 
       aes(x = publication_year)) + 
  geom_histogram(aes(color = author_affiliation_type), binwidth = 5) 

affiliation_type_by_year_wFMD_barplot <- ggplot(data = pm_data_affiliation_type_wFMD_by_year) + 
  geom_histogram(aes(x = publication_year, 
               y = n, 
               fill = author_affiliation_type
  ),
  color = 'black',
  stat = 'identity',
  position = 'stack'
  ) + 
  labs(title = 'Author affiliation composition per year (with FMD)',
       x = 'Year',
       y = 'Number of publications') +
  scale_color_tableau()

plot(affiliation_type_by_year_wFMD_barplot)

#' save the plot
ggsave(filename = './figs/affiliation_type_by_year_with_FMD_barplot.jpg', 
       plot = affiliation_type_by_year_wFMD_barplot,
       width = 10.5,
       height = 6.5,
       units = 'in'
)


#' Count the number of each author affiliation type per year (excluding FMD)
# pm_data_affiliation_type_no_FMD_no_academic_aff <- pm_data_mutated %>% 
#   arrange(publication_year) %>% 
#   filter(disease != 'FMD', author_affiliation_type !='academic_institutions') %>% 
#   group_by(publication_year, author_affiliation_type) %>% 
#   count(publication_year, author_affiliation_type)


#Plot the affiliation type over time (with FMD)
#' pm_data_affiliation_type_no_FMD_no_academic_aff_plot <- ggplot(data = pm_data_affiliation_type_no_FMD_no_academic_aff) + 
#'   # geom_point(aes(x = publication_year, 
#'   #              y = n, 
#'   #              shape = author_affiliation_type,
#'   #              color = author_affiliation_type,
#'   #              size = 1.5
#'   # )
#'   # ) + 
#'   geom_line(data = pm_data_affiliation_type_no_FMD_by_year %>% 
#'               filter(author_affiliation_type !='academic_institutions'), 
#'             aes(x = publication_year, 
#'                  y = n, 
#'                  color = author_affiliation_type
#'   ),
#'   size = 1
#'   ) +
#'   scale_x_continuous(expand = c(0, 0), limits = c(min(pm_data_affiliation_type_no_FMD_no_academic_aff$publication_year), 2020)) + 
#'   scale_y_continuous(expand = expansion(mult = c(0, 0.1)), limits = c(0, max(pm_data_affiliation_type_no_FMD_no_academic_aff$n))) +
#'   labs(title = 'Author affiliation composition per year (without FMD)',
#'        x = 'Year',
#'        y = 'Number of publications') +
#'   scale_color_tableau()
#' 
#' plot(pm_data_affiliation_type_no_FMD_no_academic_aff_plot)
#' 
#' #' save the plot
#' ggsave(filename = './figs/affiliation_type_by_year_no_FMD_barplot.jpg', 
#'        plot = affiliation_type_by_year_no_FMD_barplot,
#'        width = 10.5,
#'        height = 6.5,
#'        units = 'in'
#'        )




#' Split the collaborations into purely academic and hybrid
#' Count the number of each author affiliation type per year 
#' Final figure should be justaposed with information on outbreaks from https://www.cfr.org/timeline/major-epidemics-modern-era

pm_data_collab_type <- pm_data_mutated %>% 
  arrange(publication_year) %>%
  mutate(collab_type = as_factor(if_else(author_affiliation_type == 'academic_institutions', 
                                         'academic', 
                                         'Other collabs')
  )
  ) %>% 
  count(publication_year, collab_type, name = 'total_publications')

#plot
collab_type_trend <- ggplot(data = pm_data_collab_type) + 
  geom_bar(aes(x = publication_year,
               y = total_publications,
               fill = collab_type
               ),
           color = 'black',
           stat = 'identity',
           position =   position_fill()  
             ) +
  scale_x_continuous(breaks = seq(1971, 2020, 2),
                     labels = seq(1971, 2020, 2)
                     ) +
  # scale_y_continuous(breaks = seq(0, 50, 2),
  #                    labels = seq(0, 50, 2)) +
  labs(title = 'Publications by collaboration type',
       x = 'Year',
       y = 'Proportion of publications',
       fill = 'Collaboration type' 
         ) +
  theme_minimal()

plot(collab_type_trend)


 #Plot the data with and without FMD
library(patchwork)

collab_type_trend <- plot(affiliation_type_by_year_no_FMD_barplot/affiliation_type_by_year_wFMD_barplot)

#' save the plot
ggsave(filename = './figs/affiliation_type_by_year_with_and_without_FMD_barplot.jpg', 
       plot = affiliation_type_by_year_with_and_without_FMD_barplot,
       width = 10.5,
       height = 6.5,
       units = 'in'
)


#' 2. Is vaccination the most effective when compared to other single interventions?
#' the wide data
vax_vars_wide_data <- review_data_wide_cleaned %>% 
    select(paper_title, 
           publication_year,
           starts_with('disease_'),
           outbreak_type,
           starts_with('intervention_modelled_'),
           vax_modelled_with_non_vax,
           is_vax_effective
           ) 

#the long data
vax_vars_data_no_FMD_compact <- review_data_compact_cleaned %>% 
  select(paper_title, 
         publication_year,
         starts_with('disease'),
         outbreak_type,
         starts_with('intervention_modelled'),
         vax_modelled_with_non_vax,
         is_vax_effective
  ) %>% 
  filter(vax_modelled_with_non_vax == 'yes', disease != 'FMD')

#count the impact categories
vax_impact_categories <- vax_vars_data_no_FMD_compact %>% 
  count(is_vax_effective) %>% 
  mutate(perc = n/sum(n)*100) %>% 
  arrange(n)


vax_impact_categories


#Plot the aggregated vaccine impact results

vax_impact_dot_plot <- ggplot(data = vax_impact_categories) + 
  geom_bar(
    aes(x = is_vax_effective, 
        y = n,
        fill = is_vax_effective
        ),
    stat = 'identity',
    width = 0.25
    ) +
  scale_y_continuous(breaks = seq(0, 
                                  max(vax_impact_categories$n),
                                  2
                                  ),
                     labels = seq(0, 
                                max(vax_impact_categories$n),
                                2
                                )
                     ) +
  scale_x_discrete(breaks = c('no', 
                              'yes', 
                              'the_outcomes_were_inconclusive'
                              ), 
                   labels = c('no' = 'no', 
                              'yes' = 'yes', 
                              'the_outcomes_were_inconclusive' = 'inconclusive'
                              )
                   ) +
  scale_fill_discrete(name = 'Vaccine impact',
                      labels = c('no', 
                                 'inconclusive',
                                 'yes'
                      )
                      ) +
  labs(x = 'Vaccine is most impactful single intervention',
       y = 'Number of studies'
       ) +
  scale_color_colorblind()

plot(vax_impact_dot_plot)



#' Clean the data by renaming some columns
vax_vars_wide_data_cleaned <- vax_vars_wide_data %>%
    arrange(publication_year) %>%
    rename_at(
        vars(starts_with("disease_")),
        funs(str_replace(
            .,
            "disease_",
            ""
        ))
    ) %>%
    rename_at(
        vars(starts_with("intervention_modelled_")),
        funs(str_replace(
            .,
            "intervention_modelled_",
            ""
        ))
    )


#vaccination impact data
data_vax_impact <- data_disease_intervention_vax_vars_clean %>% 
  filter(vax_modelled_with_non_vax == 'yes') %>% 
  count(is_vax_effective) %>% 
  mutate(perc = n/sum(n)*100) 

#vaccination impact plot
data_vax_impact_barplot <- data_vax_impact %>% 
  ggplot(aes(x = is_vax_effective,
             y = perc,
             fill = is_vax_effective
             )
         ) + 
  geom_bar(stat = 'identity') +
  scale_color_tableau()
  
plot(data_vax_impact_barplot)

#' save the plot
ggsave(filename = './figs/data_vax_impact_barplot.jpg', 
       plot = data_vax_impact_barplot,
       width = 10.5,
       height = 6.5,
       units = 'in'
)

#' Secondary objectives

#' 1. Number of ORV publications by year
#' FMD included
pubs_per_year_with_FMD_bar_plot <- review_data_compact_cleaned %>% 
  count(publication_year) %>% 
    ggplot() + 
    geom_bar(aes(x = publication_year, 
                 y = n
    ), 
    stat = 'identity',
    color = 'black',
    fill = 'tomato3') +
    scale_x_continuous(breaks = seq(1970, 2020, 5),
                       labels = seq(1970, 2020, 5)
    ) +
    scale_y_continuous(breaks = seq(0, 26, 2),
                       labels = seq(0, 26, 2)
    )

plot(pubs_per_year_with_FMD_bar_plot)

#' FMD excluded
pubs_per_year_no_FMD_bar_plot <- review_data_compact_cleaned %>% 
  filter(disease != 'FMD') %>% 
  count(publication_year) %>% 
  ggplot() + 
  geom_bar(aes(x = publication_year, 
               y = n
  ), 
  stat = 'identity',
  color = 'black',
  fill = 'tomato3') +
  scale_x_continuous(breaks = seq(1970, 2020, 5),
                     labels = seq(1970, 2020, 5)
  ) +
  scale_y_continuous(breaks = seq(0, 26, 2),
                     labels = seq(0, 26, 2)
  )

plot(pubs_per_year_no_FMD_bar_plot)


#' Year with highest number of ORV publications
max_pub_by_year <- data_cleaned %>% 
    count(publication_year) %>% 
    filter(n == max(n)) 

max_pub_by_year


#' Disease and intervention(s) modelled
data_disease_and_intervention <- review_data_wide_cleaned %>% 
    select(publication_year,
           starts_with('disease_'),
           starts_with('intervention_modelled_'),
           vax_modelled_with_non_vax,
           is_vax_effective
           ) 

#' Clean up the variable names
data_disease_and_intervention_cleaned <- data_disease_and_intervention %>% 
    rename_at(vars(starts_with('intervention_modelled_')), 
              funs(str_replace(., 
                               'intervention_modelled_', 
                               '')
                   )
              ) %>% 
    rename_at(vars(starts_with('disease_')), 
              funs(str_replace(., 
                               'disease_', 
                               '')
              )
    )



#'Data and analysis code availability





