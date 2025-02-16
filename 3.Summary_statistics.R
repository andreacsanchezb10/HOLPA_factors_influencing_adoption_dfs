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


factors_list <- read_excel("factors_list.xlsx",sheet = "factors_list")





#############################################################
########## FUNCTIONS TO CALCULATE SUMMARY STATISTICS #####-----
#############################################################
summary_stats_num <- function(df,numeric_valid_columns) {
  df %>%
  select(-kobo_farmer_id) %>%  # Exclude 'id' column
  select(all_of(numeric_valid_columns))%>%
  summarise(across(everything(), list(
    mean = ~mean(. , na.rm = TRUE),
    sd = ~sd(. , na.rm = TRUE),
    min = ~min(. , na.rm = TRUE),
    max = ~max(. , na.rm = TRUE),
    missing_perc = ~sum(is.na(.)) / length(.) * 100,
    n = ~sum(!is.na(.))
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(cols = everything(), 
               names_to = "full_name", 
               values_to = "value") %>%
  mutate(total=length(.),
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
    mutate(across(all_of(factor_valid_columns),as.factor))%>%
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Category") %>%
    group_by(Variable, Category) %>%
    summarise(Count = n(), .groups = "drop") %>%
    ungroup() %>%
    group_by(Variable) %>%
    mutate(Total = sum(Count)) %>%
    ungroup()%>%
    mutate(Percentage = (Count / Total) * 100)%>%
    mutate(variable_category=paste0(Variable,"_",Category))%>%
    left_join(categorical_choices,by= "variable_category")%>%
    mutate(column_name_new= if_else(is.na(column_name_new),Variable,column_name_new))%>%
    select(-variable_category,-type_question,-column_name_new2,-name_choice)%>%
    mutate(label_choice= case_when(
      is.na(label_choice) & Category=="0" ~ "No",
      is.na(label_choice) & Category=="1" ~ "Yes",
      TRUE ~ label_choice))
}
    
#############################################################    
########## SUMMARY STATISTICS #####-----
#############################################################


### For numeric variables
columns_numeric <- intersect(factors_list$column_name_new[factors_list$metric_type == "continuous"], colnames(per_data_clean))
print(columns_numeric)  # Check if it holds expected values
per_summary_numerical <- summary_stats_num(per_data_clean,columns_numeric)
per_summary_numerical

names(factors_list)

names(per_categorical_choices)
names(per_global_choices)


### For factor and binary variables
per_categorical_choices<-factors_list%>%
  left_join(global_survey,by=c("column_name_old"="name_question"))%>%
  filter(!is.na(column_name_new))%>%
  left_join(per_global_choices  %>% select("list_name",    "name_choice",  "label_choice", "name_new"),by=c("list_name"))%>%
  distinct(column_name_new,list_name,    name_choice,  label_choice,.keep_all = TRUE)%>%
  mutate(name_new=as.character(name_new),
         name_choice= if_else(is.na(name_new),name_choice,name_new))%>%
  select(column_name_new,name_choice,label_choice,type_question)%>%
  filter(!is.na(name_choice))%>%
  mutate(variable_category=paste0(column_name_new,"_",name_choice))%>%
  mutate(column_name_new2=if_else(type_question=="select_multiple",paste0(column_name_new,"/",name_choice),column_name_new))
  select(-column_name_new)

### For factor and binary variables
#(select_one)
columns_factor_so <- intersect(factors_list$column_name_new[factors_list$metric_type %in%c("categorical","binary")], colnames(per_data_clean))
print(columns_factor_so)  # Check if it holds expected values
#(select_multiple)
columns_factor_sm <- intersect(per_categorical_choices$column_name_new2[per_categorical_choices$type_question %in%c("select_multiple")], colnames(per_data_clean))
print(columns_factor_sm)  # Check if it holds expected values

columns_factor<-c(columns_factor_so, columns_factor_sm)
print(columns_factor)  # Check if it holds expected values

per_summary_categorical <- summary_stats_factor(per_data_clean,columns_factor,per_categorical_choices)
per_summary_categorical

