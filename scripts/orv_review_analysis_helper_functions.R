library('dplyr')

count_variable <- function(long_data, grouping_var_1, grouping_var_2, var_to_count){
    long_data %>%
        group_by({{grouping_var_1}}) %>% 
        distinct() %>% 
        ungroup() %>% 
        group_by({{grouping_var_2}}) %>% 
        count({{grouping_var_2}}, {{var_to_count}})
}

count_variable(long_data = long_data_no_fmd, 
               grouping_var_1 = title, 
               grouping_var_2 = objectives, 
               var_to_count = outbreak_type
               )
