ibrary(dplyr)
library(readxl)
#############################################################    
########## UPLOAD DATA #####-----
#############################################################
factors_list_analysis<-read_excel("factors_list.prueba.xlsx",sheet = "factors_list_analysis")

per_measurement_model_afterAssess<- read_excel("factors_list.prueba.xlsx",sheet = "measurement_model_afterAssess")%>%
  select(category_1,path,constructs, column_name_new,constructs,factor, constructs_type,weights,country)%>%
  filter(country=="peru")

per_data_analysis<-  read.csv("per_data_Binary.csv",sep=",")
rownames(per_data_analysis) <- per_data_analysis$X

per_data_analysis<- per_data_analysis%>%
  dplyr::select(-X)%>%
  select(all_of(per_measurement_model_afterAssess$column_name_new),)%>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))%>%
  mutate(crop_type= case_when(
    crop_type.cacao==1~"cacao",
    crop_type.camucamu==1~"camucamu",
    TRUE~"fruittrees"  ))

table(per_data_analysis$crop_type)
#cacao   camucamu fruittrees 
#70         70         60

table(per_data_analysis$dfs_adoption_binary)
#0   1 
#144  56

table(per_data_analysis$crop_type,per_data_analysis$dfs_adoption_binary)
#0  1
#cacao      20 50
#camucamu   69  1
#fruittrees 55  5

library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork) # For combining multiple plots
library(caret)
library(AppliedPredictiveModeling)
library(readxl)
library(ggplot2)
library(reshape2)
library(psych)

global_survey<-read.csv("h_global_survey.csv",sep=",")
per_global_choices<-read.csv("per_global_choices.csv",sep=",")


#############################################################
########## FUNCTIONS TO CALCULATE SUMMARY STATISTICS #####-----
#############################################################
summary_stats_num <- function(df,numeric_valid_columns,factors_list) {
  df %>%
    #select(-kobo_farmer_id) %>%  # Exclude 'id' column
    select(all_of(numeric_valid_columns), crop_type,dfs_adoption_binary)%>%
    group_by(crop_type,dfs_adoption_binary)%>%
    group_modify(~ psych::describe(.x %>% select(all_of(numeric_valid_columns)), fast = TRUE) %>%
                   as.data.frame() %>%
                   mutate(variable = rownames(.))) %>%
    ungroup()%>%
    tibble::rownames_to_column(c("variable","crop_type","dfs_adoption_binary")) %>%
    left_join(factors_list%>%select(category_1,factor, metric, metric_type,column_name_new,
                                    description),by=c("variable"="column_name_new"))
}

summary_stats_factor <- function(df,factor_valid_columns,categorical_choices,factors_list) {
  df %>%
    #select(-kobo_farmer_id) %>%  # Exclude 'id' column
    select(all_of(factor_valid_columns))%>%
    mutate(across(all_of(factor_valid_columns),as.factor))%>%
    #describe(.)%>%
    #tibble::rownames_to_column("column_name_new") %>%
    pivot_longer(cols = everything(), names_to = "column_name_new2", values_to = "name_choice") %>%
    group_by(column_name_new2, name_choice) %>%
    summarise(Count = n(), .groups = "drop") %>%
    ungroup() %>%
    group_by(column_name_new2) %>%
    mutate(Total = sum(Count)) %>%
    ungroup()%>%
    mutate(Percentage = (Count / Total) * 100)%>%
    left_join(categorical_choices%>%select(column_name_new2,type_question,name_choice),by= c("column_name_new2","name_choice"))%>%
    mutate(variable_category = case_when( type_question == "select_one" ~ paste0(column_name_new2, "_", name_choice), TRUE ~ column_name_new2))%>%
    select(-type_question)%>%
    left_join(categorical_choices%>%select(-name_choice),by= c("variable_category","column_name_new2"))%>%
    mutate(column_name_new = case_when(is.na(column_name_new) ~ column_name_new2,  TRUE ~ column_name_new))%>%
    select(-variable_category,-type_question)%>%
    mutate(label_choice= case_when(
      is.na(label_choice) & name_choice=="0" ~ "No",
      is.na(label_choice) & name_choice=="1" ~ "Yes",
      column_name_new %in%c("livestock_health","livestock_source","livestock_exotic_local","livestock_antibiotics",
                            "livestock_feed","livestock_injury","livestock_vaccinations","fair_price_livestock") &is.na(name_choice)~ "Farmers without livestock production",
      column_name_new %in%c("fair_price_crops") &is.na(name_choice)~ "Farmers without crop production",
      column_name_new2=="ethnicity"~name_choice,
      TRUE ~ label_choice))%>%
    left_join(factors_list%>%select(category_1,sub_category,category_3,
                                    factor, metric, metric_type,categorical_type,column_name_new,description),
              by=("variable"=="column_name_new"))%>%
    filter(!is.na(metric_type))%>%
    distinct(column_name_new2, name_choice, Count, .keep_all = TRUE)
}

#############################################################    
########## SUMMARY STATISTICS #####-----
#############################################################
###### --- NUMERICAL VARIABLES -----
### Summary statistics
columns_numeric <- intersect(factors_list_analysis$column_name_new[factors_list_analysis$metric_type == "continuous"], colnames(per_data_analysis))
print(columns_numeric)  # Check if it holds expected values

per_summary_numerical <- summary_stats_num(per_data_analysis,columns_numeric,factors_list_analysis)
sort(unique(per_summary_numerical$factor))

write.csv(per_summary_numerical,"results/per/per_summary_selected_numerical.csv",row.names=FALSE)
