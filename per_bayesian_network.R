factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")


per_data_clean<- read.csv("per_data_clean.csv",sep=",")
per_summary_categorical<-read.csv("per_summary_categorical.csv",sep=",")
per_summary_numerical<-read.csv("per_summary_numerical.csv",sep=",")




#########################################################################################################################################################################################
############# DATA TYPE CONVERSION
# data standardization I need to implement this first-----
#########################################################################################################################################################################################
#### Convert categorical nominal to factor
columns_categorical_nominal <- intersect(per_summary_categorical$column_name_new2[per_summary_categorical$categorical_type %in%c("nominal")], colnames(per_data_clean))
print(columns_categorical_nominal)  # Check if it holds expected values

per_data_clean<- per_data_clean%>%
  mutate(across(all_of(columns_categorical_nominal), as.factor))

#### Convert categorical ordinal and binary variables to numeric
columns_categorical_ordinal <- intersect(per_summary_categorical$column_name_new2[per_summary_categorical$categorical_type %in%c("ordinal",NA)], colnames(per_data_clean))
print(columns_categorical_ordinal)  # Check if it holds expected values

per_data_clean<- per_data_clean%>%
  mutate(across(all_of(columns_categorical_ordinal), as.numeric))

#### Convert continuous variables to numeric
columns_continuous <- intersect(per_summary_numerical$column_name_new, colnames(per_data_clean))
print(columns_continuous)  # Check if it holds expected values

per_data_clean<- per_data_clean%>%
  mutate(across(all_of(columns_continuous), as.numeric))

##########################################################################
############# SELECT VARIABLE FOR ANALYSIS -----
##########################################################################

per_variables_list<-c(unique(per_summary_categorical$column_name_new2),unique(per_summary_numerical$column_name_new))
per_variables_list

per_data_analysis<- per_data_clean%>%
  select(all_of(per_variables_list))

summary(per_data_analysis)

select(gender,age, n_people,year_birth,n_visits_extensionist,dfs_total_area)



#https://github.com/drkowal/BayesSubsets?utm_source=chatgpt.com