library(dplyr)
library(tidyr)

########## FUNCTION TO CALCULATE SUMMARY STATISTICS #####-----
summary_stats_num <- function(df,numeric_valid_columns) {
  df %>%
  select(-kobo_farmer_id) %>%  # Exclude 'id' column
  select(all_of(numeric_valid_columns))%>%
  summarise(across(everything(), list(
    mean = ~mean(. , na.rm = TRUE),
    sd = ~sd(. , na.rm = TRUE),
    min = ~min(. , na.rm = TRUE),
    max = ~max(. , na.rm = TRUE),
    missing_perc = ~sum(is.na(.)) / length(.) * 100
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = everything(), 
               names_to = "full_name", 
               values_to = "value") %>%
  mutate(
    variable = case_when(
      str_detect(full_name, "_missing_perc$") ~ str_remove(full_name, "_missing_perc$"),  # Keep "missing_perc" together
      TRUE ~ str_extract(full_name, ".*(?=_[^_]+$)")),  # Extract everything before last "_"
    statistic = case_when(
      str_detect(full_name, "_missing_perc$") ~ "missing_perc",  # Assign "missing_perc" as a single statistic
      TRUE ~ str_extract(full_name, "[^_]+$"))) %>% # Extract everything after last "_"
  select(variable, statistic, value)%>%  # Reorder columns for clarity
  pivot_wider(names_from = "statistic", values_from = "value")
}


summary_stats_factor <- function(df,factor_valid_columns) {
  df %>%
    select(-kobo_farmer_id) %>%  # Exclude 'id' column
    select(all_of(factor_valid_columns))%>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Category") %>%
    group_by(Variable, Category) %>%
    summarise(Count = n(), .groups = "drop") %>%
    ungroup() %>%
    group_by(Variable) %>%
    mutate(Total = sum(Count)) %>%
    ungroup()%>%
    mutate(Percentage = (Count / Total) * 100)
}
    
    
########## SUMMARY STATISTICS #####-----
### For numeric variables
per_valid_columns_numeric <- intersect(factors_list$column_name_new[factors_list$metric_type == "continuous"], colnames(per_data_clean))
print(per_numeric_valid_columns)  # Check if it holds expected values
per_summary_numerical <- summary_stats_num(per_data_clean,per_valid_columns_numeric)


### For factor and binary variables
per_valid_columns_factor <- intersect(factors_list$column_name_new[factors_list$metric_type %in%c("categorical","binary")], colnames(per_data_clean))
print(per_factor_valid_columns)  # Check if it holds expected values
per_summary_categorical <- summary_stats_factor(per_data_clean,per_valid_columns_factor)




