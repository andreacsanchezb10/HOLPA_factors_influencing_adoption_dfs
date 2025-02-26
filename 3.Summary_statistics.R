library(dplyr)
library(tidyr)
library(stringr)


per_data_clean<- read.csv("per_data_clean.csv",sep=",")
global_survey<-read.csv("global_survey.csv",sep=",")
per_global_choices<-read.csv("per_global_choices.csv",sep=",")
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
         column_name_new = case_when(
      str_detect(full_name, "_missing_perc$") ~ str_remove(full_name, "_missing_perc$"),  # Keep "missing_perc" together
      TRUE ~ str_extract(full_name, ".*(?=_[^_]+$)")),  # Extract everything before last "_"
    statistic = case_when(
      str_detect(full_name, "_missing_perc$") ~ "missing_perc",  # Assign "missing_perc" as a single statistic
      TRUE ~ str_extract(full_name, "[^_]+$"))) %>% # Extract everything after last "_"
  select(column_name_new, statistic, value)%>%  # Reorder columns for clarity
  pivot_wider(names_from = "statistic", values_from = "value")%>%
    left_join(factors_list%>%select(category_1,category_2,category_3,factor, metric, metric_type,column_name_new),by="column_name_new")
}


summary_stats_factor <- function(df,factor_valid_columns,categorical_choices,factors_list) {
  df %>%
    select(-kobo_farmer_id) %>%  # Exclude 'id' column
    select(all_of(factor_valid_columns))%>%
    mutate(across(all_of(factor_valid_columns),as.factor))%>%
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
    mutate(column_name_new = case_when(
      is.na(column_name_new) ~ sub("\\..*", "", column_name_new2),  TRUE ~ column_name_new))%>%
    select(-variable_category,-type_question)%>%
    mutate(label_choice= case_when(
      is.na(label_choice) & name_choice=="0" ~ "No",
      is.na(label_choice) & name_choice=="1" ~ "Yes",
      column_name_new %in%c("livestock_health","livestock_source","livestock_exotic_local","livestock_antibiotics",
                            "livestock_feed","livestock_injury","livestock_vaccinations","fair_price_livestock") &is.na(name_choice)~ "Farmers without livestock production",
      column_name_new %in%c("fair_price_crops") &is.na(name_choice)~ "Farmers without crop production",
      column_name_new2=="ethnicity"~name_choice,
      TRUE ~ label_choice))%>%
    left_join(factors_list%>%select(category_1,category_2,category_3,factor, metric, metric_type,categorical_type,column_name_new),by="column_name_new")%>%
    filter(!is.na(metric_type))
}
    
#############################################################    
########## SUMMARY STATISTICS #####-----
#############################################################

### For numeric variables
columns_numeric <- intersect(factors_list$column_name_new[factors_list$metric_type == "continuous"], colnames(per_data_clean))
print(columns_numeric)  # Check if it holds expected values
per_summary_numerical <- summary_stats_num(per_data_clean,columns_numeric)
per_summary_numerical

write.csv(per_summary_numerical,"per_summary_numerical.csv",row.names=FALSE)


### For factor and binary variables
per_categorical_choices<-per_global_choices%>%
  mutate(name_new=as.character(name_new),
         name_choice= if_else(is.na(name_new),name_choice,name_new))%>%
  select(column_name_new,name_choice,label_choice,type_question)%>%
  filter(!is.na(name_choice))%>%
  mutate(variable_category=if_else(type_question=="select_one",paste0(column_name_new,"_",name_choice),column_name_new))%>%
  mutate(column_name_new2=if_else(type_question=="select_multiple",paste0(column_name_new,".",name_choice),column_name_new))

### For factor and binary variables
#(select_one)
columns_factor_so <- intersect(factors_list$column_name_new[factors_list$metric_type %in%c("categorical","binary")], colnames(per_data_clean))
print(columns_factor_so)  # Check if it holds expected values

#(select_multiple)
columns_factor_sm <- intersect(per_categorical_choices$column_name_new2[per_categorical_choices$type_question %in%c("select_multiple")], colnames(per_data_clean))
print(columns_factor_sm)  # Check if it holds expected values

columns_factor<-c(columns_factor_so, columns_factor_sm)
print(columns_factor)  # Check if it holds expected values

per_summary_categorical <- summary_stats_factor(per_data_clean,columns_factor,per_categorical_choices,factors_list)
per_summary_categorical
print(per_summary_categorical)  # Check if it holds expected values

write.csv(per_summary_categorical,"per_summary_categorical.csv",row.names=FALSE)

#############################################################    
########## GRAPHS #####-----
#############################################################

library(ggplot2)
library(patchwork) # For combining multiple plots

# Example: Load your data
# df <- read.csv("your_data.csv")

# Select only numeric columns
columns_numeric <- intersect(factors_list$column_name_new[factors_list$metric_type == "continuous"], colnames(per_data_clean))

columns_numeric 

# Create histogram plots for each numeric column
plot_list <- lapply(columns_numeric, function(col) {
  ggplot(per_data_clean, aes(x = .data[[col]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    labs(title = col) +
    theme_minimal()
})

# Arrange all histograms in a grid
histogram_plot <- wrap_plots(plot_list) + plot_annotation(title = "Histograms of Numeric Variables")

# Display
print(histogram_plot)


