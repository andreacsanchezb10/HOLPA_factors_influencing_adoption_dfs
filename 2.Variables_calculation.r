library(matrixStats)
library(dplyr)


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
    n_adults_wa = n_adults_wa_male+n_adults_wa_female,
    #Total adults (>65 years old) in household
    n_adults_old= n_adults_old_male+n_adults_old_female,
    #Total children in household
    n_children= n_children_male+n_children_female ,
    #Total adults in household (18->65 years old) 
    n_adults_total= n_adults_wa+n_adults_old ,
    #Total number of people in household
    n_people = n_adults_total+n_children,
    #Total number of permanent hired labour
    n_hlabour_permanent_total= rowSums(across(starts_with("n_hlabour_permanent/")), na.rm = TRUE),
    #Total number of seasonal hired labour
    n_hlabour_seasonal_total= rowSums(across(starts_with("n_hlabour_seasonal/")), na.rm = TRUE),
    #Total number of permanent household labour
    n_nhlabour_permanent_total=rowSums(across(starts_with("n_nhlabour_permanent/")), na.rm = TRUE),
    #Total number of seasonal household labour
    n_nhlabour_seasonal_total=rowSums(across(starts_with("n_nhlabour_seasonal/")), na.rm = TRUE),
    #Number of secondary occupations
    across(starts_with("occupation_secondary_list"), ~ as.numeric(as.character(.))),  
    n_occupation_secondary_list = rowSums(as.matrix(select(., starts_with("occupation_secondary_list"))), na.rm = TRUE),
    #Farmer as a primary occupation
    "occupation_primary_farmer" = ifelse(occupation_primary=="1", "1","0"))

### FARMER BEHAVIOUR ----
per_data_clean <- per_data_clean %>%
  mutate(
    #Human well being score
    across(starts_with("human_wellbeing_"), ~ as.numeric(as.character(.))),  
    human_wellbeing_median = rowMedians(as.matrix(select(., starts_with("human_wellbeing_"))), na.rm = TRUE),
    #Perspective on agroecology score
    across(starts_with("agroecol_perspective_"), ~ as.numeric(as.character(.))),  
    agroecol_perspective_median = rowMedians(as.matrix(select(., starts_with("agroecol_perspective_"))), na.rm = TRUE))


### FARM MANAGEMENT CHARACTERISTICS ----
per_data_clean <- per_data_clean %>%
  mutate(
    #Number of ecological practices use on cropland to improve soil quality and health
    across(starts_with("soil_fertility_ecol_practices/"), ~ as.numeric(as.character(.))),
    n_soil_fertility_ecol_practices = rowSums(across(starts_with("soil_fertility_ecol_practices/")), na.rm = TRUE),
    #Number of farm products type in the last 12 months
    across(starts_with("farm_products/"), ~ as.numeric(as.character(.))),
    n_farm_products = rowSums(across(starts_with("farm_products/")), na.rm = TRUE),
    #Number of practices implemented to keep animals on the farm healthy and happy?
    across(starts_with("livestock_health_practice/"), ~ as.numeric(as.character(.))),
    n_livestock_health_practice = rowSums(across(starts_with("livestock_health_practice/")), na.rm = TRUE),
    #Number of management practices used to manage livestock diseases in the last 12 months
    across(starts_with("livestock_diseases_management/"), ~ as.numeric(as.character(.))),
    n_livestock_diseases_management= rowSums(across(starts_with("livestock_diseases_management/")), na.rm = TRUE),
    #Number of ORGANIC management practices used to manage livestock diseases in the last 12 months
    n_livestock_diseases_management_organic = rowSums(select(., c("livestock_diseases_management/3",
                                                                  "livestock_diseases_management/4",
                                                                  "livestock_diseases_management/5",
                                                                  "livestock_diseases_management/6")),na.rm = TRUE),
    #Number of CHEMICAL management practices used to manage livestock diseases in the last 12 months
    n_livestock_diseases_management_organic = rowSums(select(., c("livestock_diseases_management/1",
                                                                  "livestock_diseases_management/2")),na.rm = TRUE))

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
    #Participation in training (training in innovative or best management agricultural practices, training in agribusiness management and value addition, or other)
    training_participation<- if_else(training_best_practices=="1", "1",
                                     if_else(training_agribusiness=="1", "1",
                                             if_else(training_other=="1", "1","0"))))

### OUTCOMES ----
### Potential outcomes ----
per_data_clean <- per_data_clean %>%
  mutate(
    # total area (ha) of cropland under diversified farming systems
    across(starts_with("dfs_") & ends_with("_area"), ~ as.numeric(as.character(.))),
    dfs_total_area = rowSums(select(., starts_with("dfs_") & ends_with("_area")) %>% mutate(across(everything(), as.numeric)), na.rm = TRUE),
    # adoption of diversified farming systems binary (1=yes,0=no)
    dfs_adoption_binary = as.factor(ifelse(dfs_total_area > 0, "1","0")))

## to check: controlar si hay otras preguntas donde se citen dfs, ver las practicas de livestock

names(per_data_clean)

