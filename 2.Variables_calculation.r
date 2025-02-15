######################################
########## VARIABLES CALCULATION #####-----
######################################

##### FACTORS ----
### HUMAN CAPITAL ----

# household head age
per_data_clean$age <- 2025-per_data_clean$year_birth 

#total adults (18-65 years old) in household
per_data_clean$n_adults_wa <- per_data_clean$n_adults_wa_male+per_data_clean$n_adults_wa_female 

#total adults (>65 years old) in household
per_data_clean$n_adults_old <- per_data_clean$n_adults_old_male+per_data_clean$n_adults_old_female 

# total children in household
per_data_clean$n_children <- per_data_clean$n_children_male+per_data_clean$n_children_female 

#total adults in household (18->65 years old) 
per_data_clean$n_adults_total <- per_data_clean$n_adults_wa+per_data_clean$n_adults_old 

# total number of people in household
per_data_clean$n_people <- per_data_clean$n_adults_total+per_data_clean$n_children 


### FARM MANAGEMENT CHARACTERISTICS ----

# total ecological practices used on cropland to improve soil quality and health
per_data_clean <- per_data_clean %>%
  mutate(across(starts_with("ecol_practices"), ~ as.numeric(as.character(.))))%>%
  mutate(n_ecol_practices = rowSums(across(starts_with("ecol_practices")), na.rm = TRUE)) %>% # Sum the columns
  mutate(across(starts_with("ecol_practices"), as.factor))




### Potential outcomes ----

### OUTCOMES ----
# total area (ha) of cropland under diversified farming systems
per_data_clean$dfs_total_area <- rowSums(select(per_data_clean, starts_with("dfs_") & ends_with("_area")), na.rm = TRUE)

# adoption of diversified farming systems binary (1=yes,0=no)
per_data_clean$dfs_adoption_binary <- as.factor(ifelse(per_data_clean$dfs_total_area > 0, "1","0"))

## to check: controlar si hay otras preguntas donde se citen dfs, ver las practicas de livestock

names(per_data_clean)

