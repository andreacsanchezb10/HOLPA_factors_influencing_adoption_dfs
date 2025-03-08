library(matrixStats)
library(dplyr)
library(readr)  # for parse_number()
library(stringr)

per_data<- read.csv("per_data.csv",sep=",")
global_survey<-read.csv("global_survey.csv",sep=",")
per_global_choices<-read.csv("per_global_choices.csv",sep=",")

names(per_data)
#############################################################    
########## DATA TYPE CONVERSION #####-----
#############################################################

###### --- NUMERICAL VARIABLES -----
#### Convert continuous variables to numeric
per_columns_numeric <- intersect(global_survey$column_name_new[global_survey$type_question %in% c( "decimal", "integer")], colnames(per_data))
print(per_columns_numeric)  # Check if it holds expected values

per_data_clean<- per_data%>%
  mutate(across(all_of(per_columns_numeric), as.numeric))%>%
  mutate(across(starts_with("nonhired_labour_"), ~ replace_na(.x, 0)))%>%
  mutate(across(starts_with("hired_labour_"), ~ replace_na(.x, 0)))

###### --- CATEGORICAL AND BINARY VARIABLES -----
#### Convert categorical and binary to factor
per_columns_numeric <- intersect(global_survey$column_name_new[global_survey$type_question %in% c( "decimal", "integer")], colnames(per_data))

per_columns_factor <- intersect(global_survey$column_name_new[global_survey$type_question %in%c("calculate","select_multiple", "select_one","text" )], colnames(per_data_clean))
print(per_columns_factor)  # Check if it holds expected values

per_data_clean<- per_data_clean%>%
  mutate(across(all_of(per_columns_factor), as.factor))

str(per_data_clean$marital_status)
str(per_data_clean$gender)
names(per_data_clean)

#############################################################
########## OUTCOMES CALCULATION #####-----
#############################################################

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

#############################################################
########## FACTORS CALCULATION #####-----
#############################################################



### BIOPHYSICAL CONTEXT ----
per_data_clean<-per_data_clean%>%
  #Soil depth
  mutate(across(starts_with("soil_depth_"), ~str_extract(.x, "(?<=depth_).*")))%>%
  mutate(across(starts_with("soil_depth_"), ~ as.numeric(as.character(.))))%>%
  mutate(soil_depth = rowSums(as.matrix(select(., starts_with("soil_depth_"))), na.rm = TRUE)/3)%>%
  mutate(soil_depth= round(soil_depth, digits=0))%>%
  mutate(soil_depth=paste0("depth_",soil_depth))%>%
  #Year of assessment
  mutate(year_assessment= str_extract(end_time, "^\\d{4}"))%>%
  #Rainfall timing change
  mutate(rainfall_timing_change_perception= case_when(
    rainfall_timing_change_perception.startearlier=="1"~"1",
    rainfall_timing_change_perception.startlater=="1"~"1",
    rainfall_timing_change_perception.unpredictable=="1"~"1",
    rainfall_timing_change_perception.stopearlier=="1"~"1",
    rainfall_timing_change_perception.stoplater=="1"~"1",
    TRUE~"0"))


### FINANCIAL CAPITAL ----
per_data_clean<-per_data_clean%>%
  #Income sources
  mutate(across(starts_with("income_sources."), ~ as.numeric(as.character(.))))%>%
  mutate(num_income_sources = rowSums(as.matrix(select(., starts_with("income_sources."))), na.rm = TRUE))%>%
  mutate(income_sources= case_when(
    num_income_sources>2~ "5",
    num_income_sources==2~ "2",
    num_income_sources==1~ "0",
    TRUE~ NA))%>%
  #Availability of non-farm income
  mutate(income_access_nonfarm = rowSums(as.matrix(select(., c(income_sources.casual_labour,
                                                               income_sources.transfers,
                                                               income_sources.other,
                                                               income_sources.subsidy,
                                                               income_sources.other_business,
                                                               income_sources.leasing))), na.rm = TRUE))%>%
  mutate(income_access_nonfarm = ifelse(income_access_nonfarm>0, "1","0"))%>%
  #On-farm income
  mutate(income_amount_onfarm= rowSums(as.matrix(select(., c(income_amount_crop,
                                                             income_amount_livestock,
                                                             income_amount_fish))), na.rm = TRUE))%>%
  #Non-farm income amount
  mutate(income_amount_nonfarm= rowSums(as.matrix(select(., c(income_amount_family_business,
                                                              income_amount_casual_labour,
                                                              income_amount_formal_labour,
                                                              income_amount_transfers,
                                                              income_amount_leasing_land,
                                                              income_amount_subsidy,
                                                              income_amount_other))), na.rm = TRUE))%>%
  #Total income amount
  mutate(income_amount_total= income_amount_onfarm+income_amount_nonfarm)%>%
  #Access to credit
  mutate(credit_access= ifelse(credit=="2", "1","0"))%>%
  #Access to credit is a constraint
  mutate(credit_access_constraint= ifelse(credit=="1", "1","0"))%>%
  mutate(across(starts_with("assets_"), ~ ifelse(. == "9999"|is.na(.),"0", .))) %>%  # Replace 9999 with 0
  mutate(across(starts_with("assets_"), ~ as.numeric(as.character(.))))%>%
  #Number of assets
  mutate(assets_count =rowSums(across(starts_with("assets_"), ~ .>0)))%>%
  #Livestock count
  #Reference: Marc BENOIT, Patrick VEYSSET Livestock unit calculation: a method based on energy requirements to refine the study of livestock farming systems
  mutate(livestock_count_tlu= (livestock_main_animal_number_Cattle*1)+ #Cattle TLU=1
           (livestock_main_animal_number_Pigs*0.2)+ #Pigs 0.2
           (livestock_main_animal_number_Chickens*0.01)+ #Poultry 0.01
           (livestock_main_animal_number_Ducks*0.01)+ #Poultry 0.01
           (livestock_main_animal_number_Turkeys*0.01)+ #Poultry 0.01
           (livestock_main_animal_number_Cuyes*0.01)+ #Cuyes 0.01
           (livestock_main_animal_number_rabbits*0.02))%>% #Rabbits 0.01
  #Household held a debt
  mutate(household_held_debt=case_when(credit_payment_full=="0"~ "1",TRUE~"0"))%>%
  #High-cost roof materials
  mutate(high_cost_roof_material= case_when(
    roof_material.Galvanized_iron_or_aluminum_or_other_metal_sheets=="1"~"1",
    roof_material._brick=="1"~"1",
    roof_material.Concrete=="1"~"1",
    roof_material._stone=="1"~"1",
    TRUE~"0"))%>%
  #High-cost walls materials
    mutate(high_cost_walls_material= case_when(
    walls_material.Bricks=="1"~"1",
    walls_material.Stones=="1"~"1",
    walls_material.Iron_sheet=="1"~"1",
    TRUE~"0"))
  
  

### HUMAN CAPITAL ----
per_data_clean<-per_data_clean%>%
    #Household head age
  mutate(age = 2025-year_birth)%>%
    #Ethnicity
    mutate( ethnicity= case_when(ethnicity %in% c("Ashaninka","Quechua")~ "Ashaninka or Quechua", TRUE~ ethnicity))%>%
  #Level of education farmer
  mutate(education_level_finished= case_when(
    education_level%in%c("1","4")~"1",
    education_level%in%c("2","11")~"0",
    education_level%in%c("3","6","8")~"2",
    education_level%in%c("5","7","9","10")  ~"3",
    TRUE~NA))%>%
  #Level of education of most male household members
  mutate(education_level_male_finished= case_when(
    education_level_male%in%c("1","4")~"1",
    education_level_male%in%c("2","11")~"0",
    education_level_male%in%c("3","6","8")~"2",
    education_level_male%in%c("5","7","9","10")  ~"3",
    TRUE~NA))%>%
  #Level of education of most female household members
  mutate(education_level_female_finished= case_when(
      education_level_female%in%c("1","4")~"1",
      education_level_female%in%c("2","11")~"0",
      education_level_female%in%c("3","6","8")~"2",
      education_level_female%in%c("5","7","9","10")  ~"3",
      TRUE~NA))%>%
  #Level of education of most household members
  mutate(education_level_household_finished= pmax(education_level_finished, education_level_male_finished, education_level_female_finished, na.rm = TRUE))%>%
    #Total adults (18-65 years old) in household
  mutate(num_adults_wa = num_adults_wa_male+num_adults_wa_female,
    #Total adults (>65 years old) in household
    num_adults_old= num_adults_old_male+num_adults_old_female,
    #Total children in household
    num_children= num_children_male+num_children_female ,
    #Total adults in household (18->65 years old) 
    num_adults_total= num_adults_wa+num_adults_old ,
    #Total number of people in household
    num_people = num_adults_total+num_children,
    #Total number of permanent hired labour
    num_hlabour_permanent_total= rowSums(across(starts_with("num_hlabour_permanent_")), na.rm = TRUE),
    #Total number of seasonal hired labour
    num_hlabour_seasonal_total= rowSums(across(starts_with("num_hlabour_seasonal_")), na.rm = TRUE),
    #Total number of permanent household labour
    num_nhlabour_permanent_total=rowSums(across(starts_with("num_nhlabour_permanent_")), na.rm = TRUE),
    #Total number of seasonal household labour
    num_nhlabour_seasonal_total=rowSums(across(starts_with("num_nhlabour_seasonal_")), na.rm = TRUE),
    #Farm has hired/free/exchange labour
    hlabour = num_hlabour_permanent_total+num_hlabour_seasonal_total,
    hlabour= ifelse(hlabour>0, "1","0"),
    #Number of secondary occupations
    across(starts_with("occupation_secondary_list"), ~ as.numeric(as.character(.))),  
    num_occupation_secondary_list = rowSums(as.matrix(select(., starts_with("occupation_secondary_list"))), na.rm = TRUE),
    #Farmer as a primary occupation
    occupation_primary_farmer = ifelse(occupation_primary=="1", "1","0"))%>%
  mutate(full_time_farmer= case_when(occupation_primary_farmer=="1"&occupation_secondary=="0"~"1",TRUE~"0"))%>%
select(occupation_primary_farmer,occupation_secondary,full_time_farmer)




### PHYSICAL CAPITAL ----
# Function to classify energy type dynamically
classify_energy_type <- function(df, prefix, renewable_keywords, nonrenewable_keywords, new_col_name) {
  renewable_sources <- grep(paste0("^", prefix, ".*(", paste(renewable_keywords, collapse = "|"), ")$"), colnames(df), value = TRUE)
  nonrenewable_sources <- grep(paste0("^", prefix, ".*(", paste(nonrenewable_keywords, collapse = "|"), ")$"), colnames(df), value = TRUE)
  
  df %>%
    rowwise() %>%
    mutate(
      renewable_used = sum(c_across(any_of(renewable_sources)), na.rm = TRUE) > 0,
      nonrenewable_used = sum(c_across(any_of(nonrenewable_sources)), na.rm = TRUE) > 0,
      !!new_col_name := case_when(
        renewable_used & !nonrenewable_used ~ 5,
        renewable_used & nonrenewable_used ~ 3,
        !renewable_used & nonrenewable_used ~ 1,
        TRUE ~ NA_real_
      )
    ) %>%
    ungroup() %>%
    select(-renewable_used, -nonrenewable_used)  # Remove temp columns
}

# Define keywords to classify renewable and non-renewable energy sources
renewable_keywords <- c("Wind_turbine", "Solar_panel", "Burning_plant_materials", "Cow_dung_cakes","Animal_traction","Human_power.by_hand_only","Biogas")
nonrenewable_keywords <- c("Electricity", "Gas", "Coal", "Petrol_or_diesel","LPG","Oil")

# Type of energy used for: irrigation
per_data_clean <- classify_energy_type(per_data_clean, "energy_irrigation", renewable_keywords, nonrenewable_keywords, "energy_irrigation_type")%>%
  mutate(energy_irrigation_type= ifelse(is.na(energy_irrigation_type), 0,energy_irrigation_type))
#Type of energy used for: Tillage, sowing or harvesting
per_data_clean <- classify_energy_type(per_data_clean, "energy_tillage_haverst", renewable_keywords, nonrenewable_keywords, "energy_tillage_haverst_type")
#Type of energy used for: Cooking
per_data_clean <- classify_energy_type(per_data_clean, "energy_cooking", renewable_keywords, nonrenewable_keywords, "energy_cooking_type")
#Type of energy used for: Cleaning, processing or transporting harvested food
per_data_clean <- classify_energy_type(per_data_clean, "energy_cleaning_transporting", renewable_keywords, nonrenewable_keywords, "energy_cleaning_transporting_type")
#Type of energy used for: General
per_data_clean<-per_data_clean%>%
  mutate(energy_type = rowMedians(as.matrix(select(., c(energy_irrigation_type,
                                                        energy_tillage_haverst_type,
                                                        energy_cooking_type,
                                                        energy_cleaning_transporting_type))), na.rm = TRUE))

per_data_clean<-per_data_clean%>%
  #Access to basic services
  mutate(across(starts_with("accessibility_"), ~ as.numeric(as.character(.))))%>%
  mutate(num_accessibility_basic_services = rowSums(select(., starts_with("accessibility_")), na.rm = TRUE))%>%
  mutate(accessibility_basic_services= case_when(
    num_accessibility_basic_services==0 ~ "0",
    num_accessibility_basic_services>0 & num_accessibility_basic_services<3~ "2",
    num_accessibility_basic_services>2 ~ "5",
    TRUE~ NA))%>%
  #Access to communication services
  mutate(accessibility_communication= case_when(
    accessibility_phone_reception==1 &accessibility_internet==1~ "5",
    accessibility_phone_reception==0 &accessibility_internet==1~ "2",
    accessibility_phone_reception==1 &accessibility_internet==0~ "2",
    accessibility_phone_reception==0 &accessibility_internet==0~ "0",
    TRUE~ NA))%>%
  #Access to sanitation services
  mutate(accessibility_sanitation= case_when(
    accessibility_piped_sewer==1 &accessibility_waste_collection==1~ "5",
    accessibility_piped_sewer==0 &accessibility_waste_collection==1~ "2",
    accessibility_piped_sewer==1 &accessibility_waste_collection==0~ "2",
    accessibility_piped_sewer==0 &accessibility_waste_collection==0~ "0",
    TRUE~ NA)) %>%
#Distances to services
  mutate(metric_distance_closest_farmland= case_when(country=="peru"~ "minutes",TRUE~"NA"),
         metric_distance_water_source=case_when(country=="peru"~ "minutes",TRUE~"NA"),
         metric_distance_primary_school=case_when(country=="peru"~ "minutes",TRUE~"NA"),
         metric_distance_hospital=case_when(country=="peru"~ "minutes",TRUE~"NA"),
         metric_distance_livestock_market=case_when(country=="peru"~ "minutes",TRUE~"NA"),
         metric_distance_crop_market=case_when(country=="peru"~ "minutes",TRUE~"NA"),
         metric_distance_publich_transport=case_when(country=="peru"~ "minutes",TRUE~"NA"),
         metric_distance_main_road=case_when(country=="peru"~ "minutes",TRUE~"NA"))%>%
  #Distance to the closest farmland
  mutate(distance_closest_farmland=across(starts_with("distance_"), ~ as.numeric(as.character(.))))%>%
  mutate(distance_closest_farmland = case_when(
    mode_distance_closest_farmland %in% c("walking")& metric_distance_closest_farmland %in% c("minutes")~distance_closest_farmland,
    mode_distance_closest_farmland %in% c("motobike","motorbike", "car")& metric_distance_closest_farmland %in% c("minutes")~distance_closest_farmland*10, # assumes car speed of 50km/h and walking speed of 5 km/h  
    mode_distance_closest_farmland %in% c("cycling")& metric_distance_closest_farmland %in% c("minutes")~distance_closest_farmland*3, # assumes bike speed of 15km/h and walking speed of 5 km/h
    mode_distance_closest_farmland %in% c("horse","donkey")& metric_distance_closest_farmland %in% c("minutes")~distance_closest_farmland*2, # assumes horse speed of 10km/h and walking speed of 5 km/h
    TRUE~distance_closest_farmland))%>%
  #Distance to the closest fresh water source
  mutate(distance_water_source = case_when(
    mode_distance_water_source%in% c("walking")& metric_distance_water_source %in% c("minutes")~distance_water_source,
    mode_distance_water_source %in% c("motobike","motorbike", "car")& metric_distance_water_source %in% c("minutes")~distance_water_source*10, # assumes car speed of 50km/h and walking speed of 5 km/h  
    mode_distance_water_source %in% c("cycling")& metric_distance_water_source %in% c("minutes")~distance_water_source*3, # assumes bike speed of 15km/h and walking speed of 5 km/h
    mode_distance_water_source %in% c("horse","donkey")& metric_distance_water_source %in% c("minutes")~distance_water_source*2, # assumes horse speed of 10km/h and walking speed of 5 km/h
    TRUE~distance_water_source))%>%
  #Distance to closest primary school
  mutate(distance_primary_school = case_when(
    mode_distance_primary_school%in% c("walking")& metric_distance_primary_school %in% c("minutes")~distance_primary_school,
    mode_distance_primary_school %in% c("motobike","motorbike", "car")& metric_distance_primary_school %in% c("minutes")~distance_primary_school*10, # assumes car speed of 50km/h and walking speed of 5 km/h  
    mode_distance_primary_school %in% c("cycling")& metric_distance_primary_school %in% c("minutes")~distance_primary_school*3, # assumes bike speed of 15km/h and walking speed of 5 km/h
    mode_distance_primary_school %in% c("horse","donkey")& metric_distance_primary_school %in% c("minutes")~distance_primary_school*2, # assumes horse speed of 10km/h and walking speed of 5 km/h
    TRUE~distance_primary_school))%>%
  #Distance to hospital
  mutate(distance_hospital = case_when(
    mode_distance_hospital%in% c("walking")& metric_distance_hospital %in% c("minutes")~distance_hospital,
    mode_distance_hospital %in% c("motobike","motorbike", "car")& metric_distance_hospital %in% c("minutes")~distance_hospital*10, # assumes car speed of 50km/h and walking speed of 5 km/h  
    mode_distance_hospital %in% c("cycling")& metric_distance_hospital %in% c("minutes")~distance_hospital*3, # assumes bike speed of 15km/h and walking speed of 5 km/h
    mode_distance_hospital %in% c("horse","donkey")& metric_distance_hospital %in% c("minutes")~distance_hospital*2, # assumes horse speed of 10km/h and walking speed of 5 km/h
    TRUE~distance_hospital))%>%
  #Distance to livestock market
  mutate(distance_livestock_market = case_when(
    mode_distance_livestock_market%in% c("walking")& metric_distance_livestock_market %in% c("minutes")~distance_livestock_market,
    mode_distance_livestock_market%in% c("motobike","motorbike", "car")& metric_distance_livestock_market %in% c("minutes")~distance_livestock_market*10, # assumes car speed of 50km/h and walking speed of 5 km/h  
    mode_distance_livestock_market %in% c("cycling")& metric_distance_livestock_market %in% c("minutes")~distance_livestock_market*3, # assumes bike speed of 15km/h and walking speed of 5 km/h
    mode_distance_livestock_market %in% c("horse","donkey")& metric_distance_livestock_market %in% c("minutes")~distance_livestock_market*2, # assumes horse speed of 10km/h and walking speed of 5 km/h
    TRUE~distance_livestock_market))%>%
  #Distance to crop market
  mutate(distance_crop_market = case_when(
    mode_distance_crop_market%in% c("walking")& metric_distance_crop_market %in% c("minutes")~distance_crop_market,
    mode_distance_crop_market%in% c("motobike","motorbike", "car")& metric_distance_crop_market %in% c("minutes")~distance_crop_market*10, # assumes car speed of 50km/h and walking speed of 5 km/h  
    mode_distance_crop_market %in% c("cycling")& metric_distance_crop_market %in% c("minutes")~distance_crop_market*3, # assumes bike speed of 15km/h and walking speed of 5 km/h
    mode_distance_crop_market %in% c("horse","donkey")& metric_distance_crop_market %in% c("minutes")~distance_crop_market*2, # assumes horse speed of 10km/h and walking speed of 5 km/h
    TRUE~distance_crop_market))%>%
  #Distance to public transport
  mutate(distance_publich_transport = case_when(
    mode_distance_publich_transport%in% c("walking")& metric_distance_publich_transport %in% c("minutes")~distance_publich_transport,
    mode_distance_publich_transport%in% c("motobike","motorbike", "car")& metric_distance_publich_transport %in% c("minutes")~distance_publich_transport*10, # assumes car speed of 50km/h and walking speed of 5 km/h  
    mode_distance_publich_transport %in% c("cycling")& metric_distance_publich_transport %in% c("minutes")~distance_publich_transport*3, # assumes bike speed of 15km/h and walking speed of 5 km/h
    mode_distance_publich_transport %in% c("horse","donkey")& metric_distance_publich_transport %in% c("minutes")~distance_publich_transport*2, # assumes horse speed of 10km/h and walking speed of 5 km/h
    TRUE~distance_publich_transport))%>%
  #Distance to main road
  mutate(distance_main_road = case_when(
    mode_distance_main_road%in% c("walking")& metric_distance_main_road %in% c("minutes")~distance_main_road,
    mode_distance_main_road%in% c("motobike","motorbike", "car")& metric_distance_main_road %in% c("minutes")~distance_main_road*10, # assumes car speed of 50km/h and walking speed of 5 km/h  
    mode_distance_main_road %in% c("cycling")& metric_distance_main_road %in% c("minutes")~distance_main_road*3, # assumes bike speed of 15km/h and walking speed of 5 km/h
    mode_distance_main_road %in% c("horse","donkey")& metric_distance_main_road %in% c("minutes")~distance_main_road*2, # assumes horse speed of 10km/h and walking speed of 5 km/h
    TRUE~distance_main_road))
  
### FARMER BEHAVIOUR ----
per_data_clean<- per_data_clean %>%
  #Human well being score
  mutate(across(starts_with("human_wellbeing_"), ~ as.numeric(as.factor(.))))%>%
  mutate(human_wellbeing = rowMedians(as.matrix(select(., starts_with("human_wellbeing_"))), na.rm = TRUE))%>%
  mutate(human_wellbeing= round(human_wellbeing, digits=0))%>%
  #Household decision-making agency
  mutate(across(c(farmer_agency_1, farmer_agency_3), ~ replace_na(as.numeric(as.character(.)), 0)))%>%
  mutate(farmer_agency_1_3 = case_when(
    farmer_agency_1==0 &farmer_agency_3!=0 ~ farmer_agency_3,
    farmer_agency_1!=0 &farmer_agency_3==0 ~ farmer_agency_1,
    farmer_agency_1!=0 &farmer_agency_3!=0 ~ rowMedians(as.matrix(select(., c(farmer_agency_1, farmer_agency_3))), na.rm = TRUE),
    TRUE~ NA))%>%
  mutate(farmer_agency_1_3= round(farmer_agency_1_3, digits=0))%>%
  #Perceived credit repayment confidence
  mutate(credit_payment_hability= case_when(
    credit_payment_full== "1"~ "5",
    is.na(credit_payment_full)~ "0",
    TRUE~credit_payment_hability))

#Perspective on agroecology score
mutate(across(starts_with("agroecol_perspective_"), ~ as.numeric(as.factor(.))))%>%
  mutate(agroecol_perspective_median = rowMedians(as.matrix(select(., starts_with("agroecol_perspective_"))), na.rm = TRUE))


### VULNERABILITY CONTEXT ----
per_data_clean<-per_data_clean%>%
  #Perceived pests as risk/shock
  mutate(crop_damage_cause = str_extract(crop_damage_cause, "(?<=//).*"))%>%
  mutate(perceived_shock_pest= case_when(
    str_detect(crop_damage_cause, "pest")~ "1",
    household_shock.8=="1"~ "1", #Pest or disease outbreaks
    TRUE~"0"))%>%
  #Perceived climatic conditions as risk/shock
  mutate(perceived_shock_climate= case_when(
    str_detect(crop_damage_cause, "climate_change")~ "1",
    str_detect(crop_damage_cause,"drought")~ "1",
    str_detect(crop_damage_cause,"flood")~ "1",
    str_detect(crop_damage_cause,"temperature" )~ "1",
    str_detect(crop_damage_cause,"rain")~ "1",
    household_shock.1 =="1"~ "1", #Extreme weather events (e.g. cyclones, dust storm, excess rainfall, insuficient rainfall, frost, high temperatures, high winds)
    TRUE~"0"))%>%
  #Perceived market characteristics as risk/shock
  mutate(perceived_shock_market= case_when(
    str_detect(crop_damage_cause, "lack of market")~ "1",
    str_detect(crop_damage_cause,"low prices")~ "1",
    household_shock.5 =="1"~ "1", #Market disruptions
    household_shock.10 =="1"~ "1", #Price fluctuations in market
    TRUE~"0"))%>%
  #Perceived indebtedness as risk/shock
  mutate(household_shock.9= NA)%>%
  mutate(perceived_shock_indebtedness= case_when(
    credit_payment_hability=="1"~ "1",
    household_shock.4=="1"~ "1", #Indebtedness
    TRUE~"0"))%>%
  #Perceived political change as risk/shock
  mutate(perceived_shock_political_change= case_when(
    household_shock.3=="1"~ "1", #Government change
    household_shock.9=="1"~ "1", #Policy changes
    TRUE~"0"))%>%
  #Household shock count
  mutate(across(starts_with("household_shock."), ~ as.numeric(as.character(.))))%>%
  mutate(perceived_shock_count = rowSums(as.matrix(select(., starts_with("household_shock."))), na.rm = TRUE))%>%
  mutate(perceived_shock_count=case_when(household_shock.none==1~ perceived_shock_count-1,TRUE~perceived_shock_count))%>%
  #Absorptive strategies to cope with shock
  mutate(household_shock_recover_activities.5 = NA)%>%
  mutate(household_shock_strategy_absorptive= case_when(
    household_shock_recover_activities.5=="1"~ "1", #Reduced area under cultivation
    household_shock_recover_activities.6=="1"~ "1", #Reduced food consumption
    household_shock_recover_activities.7=="1"~ "1", #Reduced household expenditure
    household_shock_recover_activities.9=="1"~ "1", #Sold assets
    household_shock_recover_activities.12=="1"~ "1", #Taken loans
    TRUE~"0"))%>%
  #Adaptive strategies to cope with shock
  mutate(household_shock_strategy_adaptive= case_when(
    household_shock_recover_activities.2=="1"~ "1", #Diversified on-farm income sources of income
    household_shock_recover_activities.3=="1"~ "1", #Engage in off-farm income sources
    household_shock_recover_activities.10=="1"~ "1", #Switched from chemical to organic farming
    household_shock_recover_activities.11=="1"~ "1", #Switched from organic to chemical farming
    TRUE~"0"))%>%
  #Transformative strategies to cope with shock
  mutate(household_shock_recover_activities.4 = NA)%>%
  mutate(household_shock_strategy_transformative= case_when(
    household_shock_recover_activities.1=="1"~ "1", #Accessed insurance or risk management mechanisms
    household_shock_recover_activities.4=="1"~ "1", #Migrated
    household_shock_recover_activities.8=="1"~ "1", #Relied on institutional support
    TRUE~"0"))%>%
  #Number of activities to cope with shocks
  mutate(across(starts_with("household_shock_recover_activities."), ~ as.numeric(as.character(.))))%>%
  mutate(household_shock_strategy_count = rowSums(as.matrix(select(., starts_with("household_shock_recover_activities."))), na.rm = TRUE))%>%
  mutate(household_shock_strategy_count=case_when(household_shock_recover_activities.none==1~ household_shock_strategy_count-1,TRUE~household_shock_strategy_count))%>%
  #Household applied shock coping strategy
  mutate(household_shock_strategy= case_when(household_shock_strategy_count>0~ "1", TRUE~"0")) 
  
  
  
  

  




 


### FARM MANAGEMENT CHARACTERISTICS ----
per_data_clean<-per_data_clean%>%
    rename("soil_fertility_management_chemical"="soil_fertility_management.1")%>%
    rename("soil_fertility_management_organic"="soil_fertility_management.2")%>%
    rename("soil_fertility_management_ecol_practices"="soil_fertility_management.3")
    
x <- per_data_clean %>%
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
         

### POLITICAL AND INSTITUTIONAL CONTEXT: Value chain ----
per_data_clean<-per_data_clean%>%
  #Perception of price fairness: crops
    mutate(fair_price_crops= case_when(
    farm_products.Crops =="0"~ "6", #does not produce 
    use_percentage_crops_sales=="0"~ "6", #does not produce 
    TRUE~ fair_price_crops))%>%
  #Perception of price fairness: livestock
    mutate(fair_price_livestock= case_when(
    farm_products.Livestock =="0"~ "6", #does not produce 
    use_percentage_livestock_sales=="0"~ "6", #does not produce 
    TRUE~ fair_price_livestock))%>%
  #Perception of price fairness: fish
    mutate(fair_price_fish= case_when(
    farm_products.Fish =="0"~ "6", #does not produce 
    use_percentage_fish_sales=="0"~ "6", #does not produce 
    TRUE~ fair_price_fish))%>%
  #Perception of price fairness: wood
  mutate(fair_price_wood= case_when(
    farm_products.Trees =="0"~ "6", #does not produce 
    use_percentage_trees_sales=="0"~ "6", #does not produce 
    TRUE~ fair_price_wood))%>%
  #Perception of price fairness: honey
  mutate(fair_price_honey= case_when(
    farm_products.Honey =="0"~ "6", #does not produce 
    use_percentage_honey_sales=="0"~ "6", #does not produce 
    TRUE~ fair_price_honey))

### POLITICAL AND INSTITUTIONAL CONTEXT: Land tenure ----
per_data_clean<-per_data_clean%>%
  mutate(
    #Land tenure status of hosehold: Own
    land_tenure_own_status= ifelse(per_data_clean$land_tenure_own_area>0, "1","0"),
    #Land tenure status of hosehold: Lease
    land_tenure_lease_status = ifelse(land_tenure_lease_area>0, "1","0"),
    #Land tenure status of hosehold:  HOLDS USE RIGHTS, either alone or jointly with someone else
    land_tenure_hold_status = ifelse(land_tenure_hold_area>0, "1","0"))%>%
  #Land tenure status
  mutate(land_tenure_status= case_when(
    land_tenure_own_status=="1" ~ "1",
    land_tenure_own_status=="0" & land_tenure_lease_status=="1"~ "2",
    land_tenure_own_status=="0" & land_tenure_lease_status=="0"& land_tenure_hold_status=="1"~ "3",
    TRUE~ NA))%>%
  mutate(
    #Proportion of land owns
    land_tenure_own_proportion= (land_tenure_own_area/farm_size)*100,
    #Proportion of land  leases
    land_tenure_lease_proportion= (land_tenure_lease_area/farm_size)*100,
    #Proportion of land hold use rights
    land_tenure_hold_proportion= (land_tenure_hold_area/farm_size)*100)
  
  

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
    num_info_exchange_sources= rowSums(across(starts_with("access_info_exchange_")), na.rm = TRUE))%>%
    mutate(access_info_exchange=ifelse(num_info_exchange_sources>0, "1","0"))
    
  
  
### POLITICAL AND INSTITUTIONAL CONTEXT: Financial risk management ----
per_data_clean<-per_data_clean%>%
  #Access to insurance against agricultural losses
  mutate(insurance_agric_losses_access= case_when(insurance_agric_losses_level=="0" ~ "0",TRUE~ "1"))


x<-per_data_clean%>%
  select(income_amount_subsidy)%>%
  mutate(income_access_subsidy= ifelse(income_amount_subsidy>0, "1","0"),
         income_access_subsidy= ifelse(is.na(income_amount_subsidy), "0",income_access_subsidy))
income_amount_subsidy

x<-per_data_clean%>%
  select(starts_with("income_sources."))




sort(unique(per_data_clean$insurance_agric_losses_access))




### SOCIAL CAPITAL ----
per_data_clean<-per_data_clean%>%
  #Number of association/organization memberships
  mutate(across(starts_with("membership."), ~ as.numeric(as.character(.))))%>%
  mutate(num_membership = rowSums(select(., starts_with("membership.")), na.rm = TRUE) -
           rowSums(select(., c("membership.none", "membership.i.dont.know")), na.rm = TRUE))%>%
  #Association/organization membership 
  mutate(membership=ifelse(num_membership>0, "1","0"))



    
    
    
write.csv(per_data_clean,"per_data_clean.csv",row.names=FALSE)

    
    
    
    
    
    
    

## to check: controlar si hay otras preguntas donde se citen dfs, ver las practicas de livestock


