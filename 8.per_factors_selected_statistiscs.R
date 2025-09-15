library(dplyr)
library(readxl)
#############################################################    
########## UPLOAD DATA #####-----
#############################################################
factors_list_analysis<-read_excel("factors_list.pruebaNEW.xlsx",sheet = "factors_list_analysis")

per_measurement_model_afterAssess<- read_excel("factors_list.pruebaNEW.xlsx",sheet = "measurement_model_afterAssess")%>%
  select(category_1,path,constructs, column_name_new,constructs,factor, constructs_type,weights,country)%>%
  filter(country=="peru")

per_data_analysis<-  read.csv("per_data_Binary.csv",sep=",")%>%
  filter(crop_type.camucamu==0)
rownames(per_data_analysis) <- per_data_analysis$X

per_data_analysis<- per_data_analysis%>%
  dplyr::select(-X)%>%
  select(all_of(per_measurement_model_afterAssess$column_name_new),crop_type.cacao)%>%#,
         #age,
         #nr_management_opinion,perception_associations_effectiveness,sales_channel_crops.cooperative)%>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))%>%
  mutate(crop_type= case_when(
    crop_type.cacao==1~"cacao",
    TRUE~"fruittrees"  ))

table(per_data_analysis$crop_type.cacao)
#cacao    fruittrees 
#70         60

table(per_data_analysis$dfs_adoption_binary)
#0   1 
#75  55

table(per_data_analysis$crop_type,per_data_analysis$dfs_adoption_binary)
#0  1
#cacao      20 50
#fruittrees 55  5

library(tidyr)
library(stringr)
library(patchwork) # For combining multiple plots
library(AppliedPredictiveModeling)
library(reshape2)
library(psych)
library(purrr)


#############################################################
########## FUNCTIONS TO CALCULATE SUMMARY STATISTICS #####-----
#############################################################
create_continuous_summary_table <- function(df, continuous_vars, crop_col = "crop_type", adoption_col = "dfs_adoption_binary") {
  df <- df %>%
    select(all_of(continuous_vars), !!sym(crop_col), !!sym(adoption_col))
  
  crop_types <- unique(df[[crop_col]])
  
  format_stats <- function(x) {
    m <- round(mean(x, na.rm = TRUE), 2)
    s <- round(sd(x, na.rm = TRUE), 2)
    minv <- round(min(x, na.rm = TRUE), 2)
    maxv <- round(max(x, na.rm = TRUE), 2)
    paste0(m, " (", s, ") [", minv, ", ", maxv, "]")
  }
  
  build_summary <- function(var) {
    tmp <- df %>%
      group_by(!!sym(crop_col), !!sym(adoption_col)) %>%
      summarise(stat = format_stats(.data[[var]]), .groups = "drop") %>%
      mutate(group = paste0(!!sym(crop_col), "_", ifelse(!!sym(adoption_col) == 1, "adopters", "non_adopters")))
    
    # Pivot to wide
    wide <- tmp %>%
      select(group, stat) %>%
      pivot_wider(names_from = group, values_from = stat)
    
    # Add per-crop total
    for (crop in crop_types) {
      adopt_vals <- df %>% filter(!!sym(crop_col) == crop, !!sym(var) %>% is.finite()) %>% pull(!!sym(var))
      total_stat <- format_stats(adopt_vals)
      wide[[paste0(crop, "_Total")]] <- total_stat
    }
    
    # Add total across all 200 farmers
    total_stat_adopters <- df %>% filter(!!sym(adoption_col) == 1, !!sym(var) %>% is.finite()) %>% pull(!!sym(var)) %>% format_stats()
    total_stat_non_adopters <- df %>% filter(!!sym(adoption_col) == 0, !!sym(var) %>% is.finite()) %>% pull(!!sym(var)) %>% format_stats()
    total_stat_all <- df %>% filter(!!sym(var) %>% is.finite()) %>% pull(!!sym(var)) %>% format_stats()
    
    wide$total_adopters <- total_stat_adopters
    wide$total_non_adopters <- total_stat_non_adopters
    wide$total_total <- total_stat_all
    
    wide <- wide %>%
      mutate(variable = var) %>%
      select(variable, everything())
    
    return(wide)
  }
  
  result <- map_dfr(continuous_vars, build_summary)
  return(result)
}

create_custom_summary_table <- function(df, vars, crop_col = "crop_type", adoption_col = "dfs_adoption_binary") {
  df <- df %>%
    select(all_of(vars), !!sym(crop_col), !!sym(adoption_col)) %>%
    mutate(across(all_of(vars), as.factor))
  
  crop_types <- unique(df[[crop_col]])
  
  build_summary <- function(var) {
    # 1. Group totals by crop x adoption
    group_totals <- df %>%
      count(!!sym(crop_col), !!sym(adoption_col), name = "group_total")
    
    # 2. Counts per level x crop x adoption
    tmp <- df %>%
      count(!!sym(var), !!sym(crop_col), !!sym(adoption_col), name = "n") %>%
      left_join(group_totals, by = c(crop_col, adoption_col)) %>%
      mutate(
        pct = round(100 * n / group_total, 1),
        label = paste0(n, " (", pct, "%)"),
        group = paste0(!!sym(crop_col), "_", ifelse(!!sym(adoption_col) == 1, "adopters", "non_adopters"))
      )
    
    wide <- tmp %>%
      select(!!sym(var), group, label) %>%
      pivot_wider(names_from = group, values_from = label, values_fill = "0 (0%)")
    
    # Rename variable column
    colnames(wide)[1] <- "value"
    
    # Add per-crop totals
    for (crop in crop_types) {
      adopt_col <- paste0(crop, "_adopters")
      non_col <- paste0(crop, "_non_adopters")
      total_col <- paste0(crop, "_Total")
      
      n_adopt <- as.integer(str_extract(wide[[adopt_col]], "\\d+"))
      n_non <- as.integer(str_extract(wide[[non_col]], "\\d+"))
      crop_total <- n_adopt + n_non
      total_pct <- round(100 * crop_total / sum(crop_total), 1)
      
      wide[[total_col]] <- paste0(crop_total, " (", total_pct, "%)")
    }
    
    # Compute overall totals from original df
    totals <- df %>%
      count(!!sym(var), !!sym(adoption_col)) %>%
      pivot_wider(names_from = !!sym(adoption_col), values_from = n, values_fill = 0) %>%
      rename(non_adopters = `0`, adopters = `1`) %>%
      mutate(
        total = adopters + non_adopters,
        total_adopters = paste0(adopters, " (", round(100 * adopters / sum(adopters + non_adopters), 1), "%)"),
        total_non_adopters = paste0(non_adopters, " (", round(100 * non_adopters / sum(adopters + non_adopters), 1), "%)"),
        total_total = paste0(total, " (", round(100 * total / sum(total), 1), "%)")
      )
    
    # Rename join column
    names(totals)[1] <- "value"
    
    # Join back to wide
    wide <- wide %>%
      left_join(totals %>% select(value, total_adopters, total_non_adopters, total_total), by = "value") %>%
      mutate(variable = var) %>%
      select(variable, everything())
    
    return(wide)
  }
  
  result <- purrr::map_dfr(vars, build_summary)
  return(result)
}

#############################################################    
########## SUMMARY STATISTICS #####-----
#############################################################
###### --- NUMERICAL VARIABLES -----
### Summary statistics
columns_continuous <- intersect(factors_list_analysis$column_name_new[factors_list_analysis$metric_type == "continuous"], colnames(per_data_analysis))
print(columns_continuous)  # Check if it holds expected values

columns_categorical_ordinal <- intersect(factors_list_analysis$column_name_new[factors_list_analysis$metric_type == "categorical"&factors_list_analysis$categorical_type=="ordinal"], colnames(per_data_analysis))
print(columns_categorical_ordinal)  # Check if it holds expected values

columns_numeric<-c(columns_continuous, columns_categorical_ordinal)
                   #"age")
print(columns_numeric)

per_summary_numerical <- create_continuous_summary_table(per_data_analysis, columns_numeric)%>%
  left_join(factors_list_analysis%>%select(category_1,factor, metric, metric_type,column_name_new,
                                           description),by=c("variable"="column_name_new"))%>%
  select(category_1,factor,description,"variable",    metric,  
         "cacao_adopters" ,"cacao_non_adopters",      "cacao_Total",
         
         "fruittrees_adopters" ,"fruittrees_non_adopters","fruittrees_Total" , 
         "total_adopters" ,"total_non_adopters","total_total")

print(per_summary_numerical)

###### --- CATEGORICAL AND BINARY VARIABLES -----
### Summary statistics
columns_binary <- intersect(factors_list_analysis$column_name_new[factors_list_analysis$metric_type %in%c("binary")&factors_list_analysis$category_1!="outcome"], colnames(per_data_analysis))
print(columns_binary)  # Check if it holds expected values

columns_categorical_nominal <- intersect(factors_list_analysis$column_name_new[factors_list_analysis$metric_type %in%c("categorical")&factors_list_analysis$categorical_type=="nominal"], colnames(per_data_analysis))
print(columns_categorical_nominal)  # Check if it holds expected values

columns_factor<-c(columns_binary, columns_categorical_nominal)
print(columns_factor)  # Check if it holds expected values

per_summary_categorical <- create_custom_summary_table(per_data_analysis, columns_factor)%>%
  left_join(factors_list_analysis%>%select(category_1,factor,column_name_new,
                                           description),by=c("variable"="column_name_new"))%>%
  select(category_1,factor,description,"variable", "value" ,     
         "cacao_adopters" ,"cacao_non_adopters",      "cacao_Total",
         "fruittrees_adopters" ,"fruittrees_non_adopters","fruittrees_Total" , 
         "total_adopters" ,"total_non_adopters","total_total")%>%
  mutate(value= case_when(
    value==0~"0= No",
    value==1~"1= Yes",
    TRUE~NA))%>%
  rename("metric"="value")

print(per_summary_categorical)  # Check if it holds expected values

names(per_summary_categorical)
names(per_summary_numerical)

per_summary_selected<-bind_rows(per_summary_categorical,per_summary_numerical)
  
  
write.csv(per_summary_selected,"results/per/per_summary_selected_factors.csv",row.names=FALSE)

