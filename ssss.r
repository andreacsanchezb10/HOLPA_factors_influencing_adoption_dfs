#####################################
########## DATA TYPE CONVERSION -----
#####################################
factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")
  
#### categorical and binary data as factor 
columns_factor <- intersect(factors_list$column_name_new[factors_list$metric_type %in%c("categorical","binary")], colnames(per_data_clean))
print(columns_factor)  # Check if it holds expected values

per_data_analysis<- per_data_clean%>%
  mutate(across(all_of(columns_factor), as.numeric))


[1] "gender"                     "marital_status"             "read_write"                 "education_level"            "project_participation"     
[6] "training_best_practices"    "training_agribusiness"      ""             "agroecology_knowledge"      "land_tenure_security"      
[11] "farm_products"              "participation_nr_frequency" ""     "nr_management_opinion"      "dfs_adoption_binary"       
[16] "ecol_practices"             "ecol_practices_other" 

str(per_data_analysis$gender)
str(per_data_analysis$training_other)
str(per_data_analysis$influence_nr_frequency)
str(per_data_analysis$nr_management_opinion)
str(per_data_analysis$dfs_adoption_binary)
str(per_data_analysis$gender)
str(per_data_analysis$ecol_practices)
