library(dplyr)
library(readxl)
library(caret)



factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")
per_data_clean<- read.csv("per_data_clean.csv",sep=",")
per_summary_categorical<-read.csv("per_summary_categorical.csv",sep=",")
per_summary_numerical<-read.csv("per_summary_numerical.csv",sep=",")

sort(unique(per_data_clean$temperature_change_perception))


#### Select the factors that were listed as important for adoption according to:
# - data availability
# - systematic evidence map

per_variables_list<-c(unique(per_summary_categorical$column_name_new2))#,unique(per_summary_numerical$column_name_new))
per_variables_list

per_data_analysis<- per_data_clean%>%
  dplyr::select(all_of(per_variables_list))

dim(per_data_analysis) #[1] 200 111 #200 farmers; 111 variables evaluated


#############################################################    
########## DATA TYPE CONVERSION #####-----
#############################################################

###### --- CATEGORICAL AND BINARY VARIABLES -----
#### Convert categorical and binary to factor
columns_categorical_nominal <- intersect(per_summary_categorical$column_name_new2, colnames(per_data_analysis))

print(columns_categorical_nominal)  # Check if it holds expected values

per_data_analysis<- per_data_analysis%>%
  mutate(across(all_of(columns_categorical_nominal), as.factor))
sort(unique(per_data_analysis$dfs_adoption_binary))

###### --- NUMERICAL VARIABLES -----
#### Convert continuous variables to numeric
columns_continuous <- intersect(per_summary_numerical$column_name_new, colnames(per_data_analysis))
print(columns_continuous)  # Check if it holds expected values

per_data_analysis<- per_data_analysis%>%
  mutate(across(all_of(columns_continuous), as.numeric))

#############################################################    
############# DISCRETIZATION -----
#############################################################

#############################################################    
############# FEATURE SELECTION -----
#############################################################

###### --- ZERO AND NEAR ZERO VARIANCE PREDICTORS -----
#In some situations, the data generating mechanism can create predictors that only have a 
#single unique value (i.e. a “zero-variance predictor”). For many models (excluding tree-based models),
#this may cause the model to crash or the fit to be unstable.
#Similarly, predictors might have only a handful of unique values that occur with very low frequencies.
## frequency ratio: would be near one for well-behaved predictors and very large for highly-unbalanced data.
## percent of unique values: is the number of unique values divided by the total number of samples (times 100)
#that approaches zero as the granularity of the data increases

nzv <- nearZeroVar(per_data_analysis, saveMetrics= TRUE) 
nzv # the variables with nzv== TRUE should be remove

nzv_list <- nearZeroVar(per_data_analysis)

long_data <- melt(per_data_analysis, id.vars = "dfs_adoption_binary", measure.vars = nzv_list)

# Create the ggplot
ggplot(long_data, aes(x = value, fill = dfs_adoption_binary)) +
  geom_bar(position = "stack") +
  facet_wrap(~variable, scales = "free", ncol = 6) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#### TO CHECK IN THE CALCULATION STEP, THE ENERGY TYPE, FISH FAIR PRICE IS WRONG!
problems<- per_data_analysis[, nzv_list]

#### Remove nzv variables from data
per_data_analysis_Filterednzv<- per_data_analysis[, -nzv_list]

dim(per_data_analysis_Filterednzv) #[1] 200 77 #200 farmers; 77 variables retained

###### only for continuous variables...
###### --- identify correlated predictors

descrCor <-  cor(per_data_analysis_Filterednzv)

###### ---  identify linear dependencies
x<-findLinearCombos(per_data_analysis_Filterednzv)

###### --- IMPUTING MISSING DATA
na_columns <- colnames(per_data_analysis_Filterednzv)[colSums(is.na(per_data_analysis_Filterednzv)) > 0]
print(na_columns)

#[1] "energy_cooking_type" FIRST CHECH WHAT IS THE PROBLEM HERE

problems2<- per_data_analysis_Filterednzv%>%
  select(all_of(na_columns))





per_data_analysis <- per_data_analysis%>%
  dplyr::select(!all_of(na_columns))








## CHECK remove the outcomes different than dfs_adoption_binary, 
excluded_for_binary<- c ( "dfs_agroforestry_adoption"  ,                    
                          "dfs_cover_crops_adoption" ,                       "dfs_crop_rotation_adoption" ,                    
                          "dfs_fallow_adoption"   ,                          "dfs_hedgerows_adoption" ,                        
                          "dfs_homegarden_adoption",                         "dfs_intercropping_adoption",                     
                          "dfs_strip_vegetation_adoption"  )



"dfs_agroforestry_area" ,"dfs_agroforestry_area" ,
"dfs_cover_crops_area", "dfs_homegarden_area" ,   

"dfs_crop_rotation_area" ,
"dfs_intercropping_area"    ,                      "dfs_fallow_area",                                
"dfs_strip_vegetation_area",                       "dfs_hedgerows_area",
)

## CHECK remove the outcomes different than "dfs_total_area"
excluded_for_continuous<- c ( "dfs_agroforestry_area" ,"dfs_agroforestry_area" ,
                              "dfs_cover_crops_area", "dfs_homegarden_area" ,   
                              "dfs_adoption_binary",                                 
                              "dfs_crop_rotation_area" ,
                              "dfs_intercropping_area"    ,                      "dfs_fallow_area",                                
                              "dfs_strip_vegetation_area",                       "dfs_hedgerows_area",
                              "dfs_agroforestry_adoption"  ,                    
                              "dfs_cover_crops_adoption" ,                       "dfs_crop_rotation_adoption" ,                    
                              "dfs_fallow_adoption"   ,                          "dfs_hedgerows_adoption" ,                        
                              "dfs_homegarden_adoption",                         "dfs_intercropping_adoption",                     
                              "dfs_strip_vegetation_adoption",
                              "education_level","ethnicity","marital_status",
                              "rainfall_timing_change_perception.nochange",     
                              "rainfall_timing_change_perception.notsure" ,     
                              "rainfall_timing_change_perception.startearlier",
                              "rainfall_timing_change_perception.startlater" ,  
                              "rainfall_timing_change_perception.stopearlier" , 
                              "rainfall_timing_change_perception.stoplater" ,   
                              "rainfall_timing_change_perception.unpredictable",
                              "temperature_change_perception")


per_data_analysis.binary<- per_data_analysis%>%
  dplyr::select(!all_of(excluded_for_binary))%>%
  dplyr::select(where(~ !(is.factor(.) && nlevels(.) < 2)))%>%
  dplyr::select(-energy_type,-fair_price_livestock,-human_wellbeing_5 )

write.csv(per_data_analysis.binary,"per_data_analysis.binary.csv",row.names=FALSE)




###### --- DISCRETIZATION OF CONTINUOUS VARIABLES -----




