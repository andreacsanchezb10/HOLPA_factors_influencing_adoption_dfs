library(matrixStats)
library(dplyr)
library(readr)  # for parse_number()

per_data_clean<- read.csv("per_data.csv",sep=",")
global_survey<-read.csv("global_survey.csv",sep=",")
per_global_choices<-read.csv("per_global_choices.csv",sep=",")

names(per_data_clean)
#####################################
########## DATA TYPE CONVERSION -----
#####################################
### Continuous data as numeric ----
names(global_survey)
sort(unique(global_survey$type_question))
per_columns_numeric <- intersect(global_survey$column_name_new[global_survey$type_question %in% c( "decimal", "integer")], colnames(per_data_clean))
print(per_columns_numeric)  # Check if it holds expected values

per_data_clean<- per_data_clean%>%
  mutate(across(all_of(per_columns_numeric), as.numeric))%>%
  mutate(across(starts_with("nonhired_labour_"), ~ replace_na(.x, 0)))%>%
  mutate(across(starts_with("hired_labour_"), ~ replace_na(.x, 0)))


#### categorical and binary data as factor 
sort(unique(global_survey$type_question))
per_columns_numeric <- intersect(global_survey$column_name_new[global_survey$type_question %in% c( "decimal", "integer")], colnames(per_data_clean))

per_columns_factor <- intersect(global_survey$column_name_new[global_survey$type_question %in%c("calculate","select_multiple", "select_one","text" )], colnames(per_data_clean))
print(per_columns_factor)  # Check if it holds expected values

per_data_clean<- per_data_clean%>%
  mutate(across(all_of(per_columns_factor), as.factor))

str(per_data_clean$marital_status)
str(per_data_clean$gender)
names(per_data_clean)

#############################################################
########## VARIABLES CALCULATION #####-----
#############################################################

##### FACTORS ----
### HUMAN CAPITAL ----
per_data_clean<-per_data_clean%>%
  mutate(
    #Household head age
    age = 2025-year_birth,
    #Total adults (18-65 years old) in household
    num_adults_wa = num_adults_wa_male+num_adults_wa_female,
    #Total adults (>65 years old) in household
    num_adults_old= num_adults_old_male+num_adults_old_female,
    #Total children in household
    num_children= num_children_male+num_children_female ,
    #Total adults in household (18->65 years old) 
    num_adults_total= num_adults_wa+num_adults_old ,
    #Total number of people in household
    num_people = num_adults_total+num_children,
    #Total number of permanent hired labour
    num_hlabour_permanent_total= rowSums(across(starts_with("num_hlabour_permanent.")), na.rm = TRUE),
    #Total number of seasonal hired labour
    num_hlabour_seasonal_total= rowSums(across(starts_with("num_hlabour_seasonal.")), na.rm = TRUE),
    #Total number of permanent household labour
    num_nhlabour_permanent_total=rowSums(across(starts_with("num_nhlabour_permanent.")), na.rm = TRUE),
    #Total number of seasonal household labour
    num_nhlabour_seasonal_total=rowSums(across(starts_with("num_nhlabour_seasonal.")), na.rm = TRUE),
    #Number of secondary occupations
    across(starts_with("occupation_secondary_list"), ~ as.numeric(as.character(.))),  
    num_occupation_secondary_list = rowSums(as.matrix(select(., starts_with("occupation_secondary_list"))), na.rm = TRUE),
    #Farmer as a primary occupation
    "occupation_primary_farmer" = ifelse(occupation_primary=="1", "1","0"))

x<-per_data_clean %>%
  select(starts_with("human_wellbeing_"),starts_with("agroecol_perspective_"))


### FARMER BEHAVIOUR ----
per_data_clean<- per_data_clean %>%
  #Human well being score
  mutate(across(starts_with("human_wellbeing_"), ~ as.numeric(as.factor(.))))%>%
  mutate(human_wellbeing_median = rowMedians(as.matrix(select(., starts_with("human_wellbeing_"))), na.rm = TRUE))%>%
  #Perspective on agroecology score
  mutate(across(starts_with("agroecol_perspective_"), ~ as.numeric(as.factor(.))))%>%
  mutate(agroecol_perspective_median = rowMedians(as.matrix(select(., starts_with("agroecol_perspective_"))), na.rm = TRUE))

str(per_data_clean$livestock_diseases_management.1)
### FARM MANAGEMENT CHARACTERISTICS ----
per_data_clean <- per_data_clean %>%
  mutate(
    #Number of ecological practices use on cropland to improve soil quality and health
    across(starts_with("soil_fertility_ecol_practices."), ~ as.numeric(as.character(.))),
    num_soil_fertility_ecol_practices = rowSums(across(starts_with("soil_fertility_ecol_practices.")), na.rm = TRUE),
    #Number of farm products type in the last 12 months
    across(starts_with("farm_products."), ~ as.numeric(as.character(.))),
    num_farm_products = rowSums(across(starts_with("farm_products.")), na.rm = TRUE),
    #Number of practices implemented to keep animals on the farm healthy and happy?
    across(starts_with("livestock_health_practice."), ~ as.numeric(as.character(.))),
    num_livestock_health_practice = rowSums(across(starts_with("livestock_health_practice.")), na.rm = TRUE),
    #Number of management practices used to manage livestock diseases in the last 12 months
    across(starts_with("livestock_diseases_management."), ~ as.numeric(as.character(.))),
    num_livestock_diseases_management= rowSums(across(starts_with("livestock_diseases_management.")), na.rm = TRUE),
    #Number of ORGANIC management practices used to manage livestock diseases in the last 12 months
    num_livestock_diseases_management_organic = rowSums(select(., c("livestock_diseases_management.3",
                                                                  "livestock_diseases_management.4",
                                                                  "livestock_diseases_management.5",
                                                                  "livestock_diseases_management.6")),na.rm = TRUE),
    #Number of CHEMICAL management practices used to manage livestock diseases in the last 12 months
    num_livestock_diseases_management_organic = rowSums(select(., c("livestock_diseases_management.1",
                                                                  "livestock_diseases_management.2")),na.rm = TRUE))

### NATURAL CAPITAL ----

per_data_clean<-per_data_clean%>%
  # Farm size
  mutate(farm_size= land_tenure_own_area+land_tenure_lease_area+land_tenure_hold_area)
         
        
### POLITICAL AND INSTITUTIONAL CONTEXT: Land tenure ----
per_data_clean<-per_data_clean%>%
  mutate(
    #Land tenure status of hosehold: Own
    land_status_own= ifelse(per_data_clean$land_tenure_own_area>0, "1","0"),
    #Land tenure status of hosehold: Lease
    land_status_lease = ifelse(land_tenure_lease_area>0, "1","0"),
    #Land tenure status of hosehold:  HOLDS USE RIGHTS, either alone or jointly with someone else
    land_status_hold = ifelse(land_tenure_hold_area>0, "1","0"))

### POLITICAL AND INSTITUTIONAL CONTEXT: Knowledge ----

per_data_clean<-per_data_clean%>%
  mutate(
    #Number of training topics
    across(starts_with("training_"), ~ as.numeric(as.character(.))),
    across(starts_with("training_"), ~ ifelse(. == 2, NA, .)), # REMOVE ANSWER I don't know
    num_training_topics= rowSums(across(starts_with("training_")), na.rm = TRUE),
    #Participation in training (training in innovative or best management agricultural practices, training in agribusiness management and value addition, or other)
    training_participation= as.factor(ifelse(num_training_topics>0, "1","0")))%>%
  mutate(
    #Exchange information with: Extensionists
    access_info_exchange_extension= ifelse(num_info_exchange_extension>0, "1","0"),
    #Exchange information with: Researchers
    access_info_exchange_researchers= ifelse(num_info_exchange_researchers>0, "1","0"),
    #Exchange information with: Farmers
    access_info_exchange_farmers= ifelse(num_info_exchange_farmers>0, "1","0"),
    #Exchange information with: Government
    access_info_exchange_government= ifelse(num_info_exchange_government>0, "1","0"),
    #Exchange information with: NGOs
    access_info_exchange_ngo= ifelse(num_info_exchange_ngo>0, "1","0"),
    #Exchange information with: consumers
    access_info_exchange_consumers= ifelse(num_info_exchange_consumers>0, "1","0"),
    #Exchange information with: Food traders
    access_info_exchange_traders= ifelse(num_info_exchange_traders>0, "1","0"),
    #Number of information sources
    across(starts_with("access_info_exchange_"), ~ as.numeric(as.character(.))),
    num_info_exchange_sources= rowSums(across(starts_with("access_info_exchange_")), na.rm = TRUE))
    

### OUTCOMES ----
### Potential outcomes ----
per_data_clean <- per_data_clean %>%
  mutate(
    # total area (ha) of cropland under diversified farming systems
    across(starts_with("dfs_") & ends_with("_area"), ~ as.numeric(as.character(.))),
    dfs_total_area = rowSums(select(., starts_with("dfs_") & ends_with("_area")) %>% mutate(across(everything(), as.numeric)), na.rm = TRUE),
    # adoption of diversified farming systems binary (1=yes,0=no)
    dfs_adoption_binary = as.factor(ifelse(dfs_total_area > 0, "1","0")),
    dfs_crop_rotation_adoption= as.factor(ifelse(dfs_crop_rotation_area > 0, "1","0")),
    
    dfs_agroforestry_adoption= as.factor(ifelse(dfs_agroforestry_area > 0, "1","0")),
    dfs_cover_crops_adoption= as.factor(ifelse(dfs_cover_crops_area > 0, "1","0")),
    dfs_homegarden_adoption= as.factor(ifelse(dfs_homegarden_area > 0, "1","0")),
    dfs_intercropping_adoption= as.factor(ifelse(dfs_intercropping_area > 0, "1","0")),
    dfs_fallow_adoption= as.factor(ifelse(dfs_fallow_area > 0, "1","0")),
    dfs_strip_vegetation_adoption= as.factor(ifelse(dfs_strip_vegetation_area > 0, "1","0")),
    dfs_hedgerows_adoption= as.factor(ifelse(dfs_hedgerows_area > 0, "1","0")))
    
    
    
write.csv(per_data_clean,"per_data_clean.csv",row.names=FALSE)

    
    
    
    
    
    
    

## to check: controlar si hay otras preguntas donde se citen dfs, ver las practicas de livestock

names(per_data_clean)

