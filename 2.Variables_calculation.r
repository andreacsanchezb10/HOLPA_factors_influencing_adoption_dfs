library(matrixStats)
library(dplyr)


#############################################################
########## VARIABLES CALCULATION #####-----
#############################################################

##### FACTORS ----
### HUMAN CAPITAL ----

#Household head age
per_data_clean$age <- 2025-per_data_clean$year_birth 

#Total adults (18-65 years old) in household
per_data_clean$n_adults_wa <- per_data_clean$n_adults_wa_male+per_data_clean$n_adults_wa_female 

#Total adults (>65 years old) in household
per_data_clean$n_adults_old <- per_data_clean$n_adults_old_male+per_data_clean$n_adults_old_female 

#Total children in household
per_data_clean$n_children <- per_data_clean$n_children_male+per_data_clean$n_children_female 

#Total adults in household (18->65 years old) 
per_data_clean$n_adults_total <- per_data_clean$n_adults_wa+per_data_clean$n_adults_old 

#Total number of people in household
per_data_clean$n_people <- per_data_clean$n_adults_total+per_data_clean$n_children 

per_data_clean<-per_data_clean%>%
  mutate(
    #Total number of permanent hired labour
    n_hlabour_permanent_total= rowSums(across(starts_with("n_hlabour_permanent/")), na.rm = TRUE),
    #Total number of seasonal hired labour
    n_hlabour_seasonal_total= rowSums(across(starts_with("n_hlabour_seasonal/")), na.rm = TRUE),
    #Total number of permanent household labour
    n_nhlabour_permanent_total=rowSums(across(starts_with("n_nhlabour_permanent/")), na.rm = TRUE),
    #Total number of seasonal household labour
    n_nhlabour_seasonal_total=rowSums(across(starts_with("n_nhlabour_seasonal/")), na.rm = TRUE))

### FARMER BEHAVIOUR ----

library(dplyr)
library(matrixStats)

per_data_clean <- per_data_clean %>%
  mutate(
    #Human well being score
    across(starts_with("human_wellbeing_"), ~ as.numeric(as.character(.))),  
    human_wellbeing_median = rowMedians(as.matrix(select(., starts_with("human_wellbeing_"))), na.rm = TRUE),
    #Perspective on agroecology score
    across(starts_with("agroecol_perspective_"), ~ as.numeric(as.character(.))),  
    agroecol_perspective_median = rowMedians(as.matrix(select(., starts_with("agroecol_perspective_"))), na.rm = TRUE))


    x<-per_data_clean%>%
      select(kobo_farmer_id,starts_with("human_wellbeing_"))
    
### FARM MANAGEMENT CHARACTERISTICS ----
per_data_clean <- per_data_clean %>%
  mutate(
    #Number of ecological practices use on cropland to improve soil quality and health
    across(starts_with("ecol_practices/"), ~ as.numeric(as.character(.))),
    n_ecol_practices = rowSums(across(starts_with("ecol_practices/")), na.rm = TRUE),
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

# Farm size
per_data_clean$farm_size <- per_data_clean$land_tenure_own_area+per_data_clean$land_tenure_lease_area+per_data_clean$land_tenure_hold_area

sort(unique(per_data_clean$farm_size ))

### POLITICAL AND INSTITUTIONAL CONTEXT: Land tenure ----
#Land tenure status of hosehold: Own
per_data_clean$land_status_own <- ifelse(per_data_clean$land_tenure_own_area>0, "1","0")
#Land tenure status of hosehold: Lease
per_data_clean$land_status_lease <- ifelse(per_data_clean$land_tenure_lease_area>0, "1","0")
#Land tenure status of hosehold:  HOLDS USE RIGHTS, either alone or jointly with someone else
per_data_clean$land_status_hold <- ifelse(per_data_clean$land_tenure_hold_area>0, "1","0")



### Potential outcomes ----

### OUTCOMES ----
# total area (ha) of cropland under diversified farming systems
per_data_clean$dfs_total_area <- rowSums(select(per_data_clean, starts_with("dfs_") & ends_with("_area")), na.rm = TRUE)

# adoption of diversified farming systems binary (1=yes,0=no)
per_data_clean$dfs_adoption_binary <- as.factor(ifelse(per_data_clean$dfs_total_area > 0, "1","0"))

## to check: controlar si hay otras preguntas donde se citen dfs, ver las practicas de livestock

names(per_data_clean)

