library(dplyr)
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

zwe_data_clean<- read.csv("zwe_data_clean.csv",sep=",")
global_survey<-read.csv("h_global_survey.csv",sep=",")
zwe_global_choices<-read.csv("zwe_global_choices.csv",sep=",")
factors_list <- read_excel("factors_list.xlsx",sheet = "factors_list")

#############################################################
########## FUNCTIONS TO CALCULATE SUMMARY STATISTICS #####-----
#############################################################
summary_stats_num <- function(df,numeric_valid_columns) {
  df %>%
  select(-kobo_farmer_id) %>%  # Exclude 'id' column
  select(all_of(numeric_valid_columns))%>%
    describe(.)%>%
    tibble::rownames_to_column("column_name_new") %>%
    left_join(factors_list%>%select(category_1,sub_category,constructs,constructs_type,weights,factor, metric, metric_type,column_name_new,description),by="column_name_new")
}


summary_stats_factor <- function(df,factor_valid_columns,categorical_choices,factors_list) {
  df %>%
    select(-kobo_farmer_id) %>%  # Exclude 'id' column
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
    left_join(factors_list%>%select(category_1,sub_category,category_3,factor, metric, metric_type,categorical_type,column_name_new,description),by="column_name_new")%>%
    filter(!is.na(metric_type))%>%
    distinct(column_name_new2, name_choice, Count, .keep_all = TRUE)
}
    
#############################################################    
########## DATA VISUALIZATION #####-----
#############################################################

###### --- NUMERICAL VARIABLES -----
### Summary statistics
columns_numeric <- intersect(factors_list$column_name_new[factors_list$metric_type == "continuous"], colnames(zwe_data_clean))
print(columns_numeric)  # Check if it holds expected values

zwe_summary_numerical <- summary_stats_num(zwe_data_clean,columns_numeric)
sort(unique(zwe_summary_numerical$factor))

write.csv(zwe_summary_numerical,"zwe_summary_numerical.csv",row.names=FALSE)

### Histogram plots for each numerical variable
plot_list <- lapply(columns_numeric, function(col) {
  ggplot(zwe_data_clean, aes(x = .data[[col]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    labs(title = col) +
    theme_minimal()
})

histogram_plot <- wrap_plots(plot_list) + plot_annotation(title = "Histograms of Numeric Variables")
print(histogram_plot)

### Scatterplot Matrix
long_data <- melt(zwe_data_clean, id.vars = "dfs_adoption_binary", measure.vars = columns_numeric)

ggplot(long_data, aes(x = dfs_adoption_binary, y =  value, color = dfs_adoption_binary)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~variable, scales = "free") +
  theme_minimal()

### Boxplot Matrix 
featurePlot(x = zwe_data_clean[,columns_numeric], 
            y = as.factor(zwe_data_clean$dfs_adoption_binary), 
            plot = "box", 
            ## Pass in options to bwplot() 
            scales = list(y = list(relation="free"),
                          x = list(rot = 90)),  
            layout = c(6,6 ))


###### --- CATEGORICAL AND BINARY VARIABLES -----
### Summary statistics
zwe_categorical_choices<-zwe_global_choices%>%
  mutate(name_new=as.character(name_new),
         name_choice= if_else(is.na(name_new),name_choice,name_new))%>%
  select(column_name_new,name_choice,label_choice,type_question)%>%
  filter(!is.na(name_choice))%>%
  mutate(variable_category=if_else(type_question=="select_one",paste0(column_name_new,"_",name_choice),column_name_new))%>%
  mutate(column_name_new2=if_else(type_question=="select_multiple",paste0(column_name_new,".",name_choice),column_name_new))

### For factor and binary variables
#(select_one)
columns_factor_so <- intersect(factors_list$column_name_new[factors_list$metric_type %in%c("categorical","binary")], colnames(zwe_data_clean))
print(columns_factor_so)  # Check if it holds expected values

#(select_multiple)
columns_factor_sm <- intersect(zwe_categorical_choices$column_name_new2[zwe_categorical_choices$type_question %in%c("select_multiple")], colnames(zwe_data_clean))
print(columns_factor_sm)  # Check if it holds expected values

columns_factor<-c(columns_factor_so, columns_factor_sm)
print(columns_factor)  # Check if it holds expected values

zwe_summary_categorical <- summary_stats_factor(zwe_data_clean,columns_factor,zwe_categorical_choices,factors_list)
sort(unique(zwe_summary_categorical$column_name_new2))

print(zwe_summary_categorical)  # Check if it holds expected values
sort(unique(zwe_summary_categorical$factor))
write.csv(zwe_summary_categorical,"zwe_summary_categorical.csv",row.names=FALSE)

### Boxplot Matrix
# Define the number of variables per plot
num_zwe_plot <- 36  # Adjust as needed

# Split categorical variables into chunks
split_vars <- split(columns_factor, ceiling(seq_along(columns_factor) / num_zwe_plot))

# Loop over chunks and generate separate plots
for (i in seq_along(split_vars)) {
  subset_vars <- split_vars[[i]]
  
  # Convert data to long format for the subset
  long_data_subset <- melt(zwe_data_clean, id.vars = "dfs_adoption_binary", measure.vars = subset_vars)
  
  p <- ggplot(long_data_subset) +
    geom_bar(position =  "stack", aes(x = value, fill = dfs_adoption_binary)) +
    facet_wrap(~variable, scales = "free", ncol = 6) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(paste("Categorical Variables - Batch", i))
  
  print(p)  # Display the plot
  
  # Save each plot separately
  #ggsave(filename = paste0("categorical_variables_batch_", i, ".png"), plot = p, width = 12, height = 8)
}
print(p)  # Display the plot



library(ggplot2)
library(reshape2)

# Define the number of variables per plot
num_per_plot <- 36  # Adjust as needed

# Convert categorical variables to factors (if not already)
zwe_data_clean[, columns_factor] <- lapply(zwe_data_clean[, columns_factor], as.factor)

# Split categorical variables into chunks
split_vars <- split(columns_factor, ceiling(seq_along(columns_factor) / num_per_plot))

# Loop over chunks and generate separate plots
for (i in seq_along(split_vars)) {
  subset_vars <- split_vars[[i]]
  
  # Convert data to long format for the subset
  long_data_subset <- melt(zwe_data_clean, id.vars = "dfs_adoption_binary", measure.vars = subset_vars)
  
  # Ensure dfs_adoption_binary is a factor
  long_data_subset$dfs_adoption_binary <- as.factor(long_data_subset$dfs_adoption_binary)
  
  # Create the plot
  p <- ggplot(long_data_subset, aes(x = value, fill = dfs_adoption_binary)) +
    geom_bar(position = "dodge") +
    facet_wrap(~variable, scales = "free", ncol = 6) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    ggtitle(paste("Categorical Variables - Batch", i))
  
  # Print the plot explicitly
  print(p)
  
  # Save each plot separately
  #ggsave(filename = paste0("categorical_variables_batch_", i, ".png"), plot = p, width = 12, height = 8)
  
  # Pause for a moment to allow plots to render in some environments
  Sys.sleep(1)
}

