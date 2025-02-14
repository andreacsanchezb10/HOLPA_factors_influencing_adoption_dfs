library(dplyr)
library(tidyr)

global_survey <- read_excel("factors_list.xlsx",sheet = "holpa_survey")%>%
  #select only the necessary columns
  select("type", "name","label::English ((en))")%>%
  #rename columns names
  rename("label_question" = "label::English ((en))")%>%
  rename("name_question" = "name")%>%
  #remove rows without questions
  filter(!type%in%c("begin_group","begin_repeat","end_repeat","end_group","start","end"))%>%
  #separate question type components
  mutate(type_question = ifelse(substr(type,1,10)=="select_one","select_one",
                                ifelse(substr(type,1,10)=="select_mul","select_multiple",type)))%>%
  #create column with list_name codes matching the choices worksheet
  mutate(list_name = if_else(type_question== "select_one"|type_question== "select_multiple", 
                             str_replace(.$type, paste0(".*", .$type_question), ""),NA))%>%
  mutate(list_name = str_replace_all(list_name, " ", ""))  #%>% mutate(global_r_list_name =  sub('*_', "", name_question)) %>%mutate(global_r_list_name = ifelse(grepl("_", global_r_list_name, fixed = TRUE)==TRUE,global_r_list_name,""))

global_choices <- read_excel("factors_list.xlsx",sheet = "holpa_choices")%>%
  select("list_name","name","label::English ((en))","name_new")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")

categorical_choices<-read_excel("factors_list.xlsx",sheet = "factors_list")%>%
  left_join(global_survey,by=c("column_name_old"="name_question"))%>%
  filter(!is.na(column_name_new))%>%
  left_join(global_choices,by="list_name")%>%
  select(column_name_new,name_choice,label_choice)%>%
  filter(!is.na(name_choice))%>%
  mutate(variable_category=paste0(column_name_new,"_",name_choice))


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


summary_stats_factor <- function(df,factor_valid_columns,categorical_choices) {
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
    mutate(Percentage = (Count / Total) * 100)%>%
    mutate(variable_category=paste0(Variable,"_",Category))%>%
    left_join(categorical_choices,by= "variable_category")

    
}
    
    
########## SUMMARY STATISTICS #####-----
### For numeric variables
columns_numeric <- intersect(factors_list$column_name_new[factors_list$metric_type == "continuous"], colnames(per_data_clean))
print(columns_numeric)  # Check if it holds expected values
per_summary_numerical <- summary_stats_num(per_data_clean,columns_numeric)
per_summary_numerical

### For factor and binary variables
columns_factor <- intersect(factors_list$column_name_new[factors_list$metric_type %in%c("categorical","binary")], colnames(per_data_clean))
print(columns_factor)  # Check if it holds expected values
per_summary_categorical <- summary_stats_factor(per_data_clean,columns_factor,categorical_choices)
per_summary_categorical



