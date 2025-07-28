library(matrixStats)
library(dplyr)
library(readr)  # for parse_number()
library(stringr)
library(tidyr)
library(geosphere)

global_survey<-read.csv("H_global_survey.csv",sep=",")

#############################################################    
########## DATA TYPE CONVERSION #####-----
#############################################################
###### ----- ALL: Mbire and Murehwa, zimbabwe ####----
zwe_data<- read.csv("zwe_data.csv",sep=",")
names(zwe_data)
#####---- NUMERICAL VARIABLES 
#Convert continuous variables to numeric
zwe_columns_numeric <- intersect(global_survey$column_name_new[global_survey$type_question %in% c( "decimal", "integer")], colnames(zwe_data))
print(zwe_columns_numeric)  # Check if it holds expected values

zwe_data_clean<- zwe_data%>%
  mutate(across(all_of(zwe_columns_numeric), as.numeric))%>%
  mutate(across(starts_with("nonhired_labour_"), ~ replace_na(.x, 0)))%>%
  mutate(across(starts_with("hired_labour_"), ~ replace_na(.x, 0)))

#####---- CATEGORICAL AND BINARY VARIABLES 
#Convert categorical and binary to factor
zwe_columns_numeric <- intersect(global_survey$column_name_new[global_survey$type_question %in% c( "decimal", "integer")], colnames(zwe_data))

zwe_columns_factor <- intersect(global_survey$column_name_new[global_survey$type_question %in%c("calculate","select_multiple", "select_one","text" )], colnames(zwe_data_clean))
print(zwe_columns_factor)  # Check if it holds expected values

zwe_data_clean<- zwe_data_clean%>%
  mutate(across(all_of(zwe_columns_factor), as.factor))


#############################################################
########## OUTCOMES CALCULATION #####-----
#############################################################
### Outcomes ----
zwe_data_clean <- zwe_data_clean %>%
  mutate(
    #Agroforestry
    dfs_agroforestry_area=dfs_agroforestry_area*0.404686,#convert to hectares
    dfs_agroforestry_adoption = as.factor(ifelse(is.na(dfs_agroforestry_area) | dfs_agroforestry_area == 0, "0", "1")),
    #Cover crops
    ### TO CHECK, PEST MANAGEMENT ALSO PROVIDES COVER CROPS
    dfs_cover_crops_area=dfs_cover_crops_area*0.404686,#convert to hectares
    dfs_cover_crops_adoption = as.factor(ifelse(is.na(dfs_cover_crops_area) | dfs_cover_crops_area == 0, "0", "1")),
    #Crop rotation
    dfs_crop_rotation_area=dfs_crop_rotation_area*0.404686,#convert to hectares
    dfs_crop_rotation_adoption = as.factor(ifelse(is.na(dfs_crop_rotation_area) | dfs_crop_rotation_area == 0, "0", "1")),
    #Fallow
    dfs_fallow_area=dfs_fallow_area*0.404686,#convert to hectares
    dfs_fallow_adoption = as.factor(ifelse(is.na(dfs_fallow_area) | dfs_fallow_area == 0, "0", "1")),
    #Hedgerows
    dfs_hedgerows_area=dfs_hedgerows_area*0.404686,#convert to hectares
    dfs_hedgerows_adoption = as.factor(ifelse(is.na(dfs_hedgerows_area) | dfs_hedgerows_area == 0, "0", "1")),
    #Homegarden
    dfs_homegarden_area=dfs_homegarden_area*0.404686,#convert to hectares
    dfs_homegarden_adoption = as.factor(ifelse(is.na(dfs_homegarden_area) | dfs_homegarden_area == 0, "0", "1")),
    #Intercropping
    dfs_intercropping_area=dfs_intercropping_area*0.404686,#convert to hectares
    dfs_intercropping_adoption = as.factor(ifelse(is.na(dfs_intercropping_area) | dfs_intercropping_area == 0, "0", "1")),
    #Natural strip vegetation area
    dfs_strip_vegetation_area=dfs_strip_vegetation_area*0.404686,#convert to hectares
    dfs_strip_vegetation_adoption = as.factor(ifelse(is.na(dfs_strip_vegetation_area) | dfs_strip_vegetation_area == 0, "0", "1")),
    #Pull-push
    dfs_pullpush_area=dfs_pullpush_area*0.404686,#convert to hectares
    dfs_pullpush_adoption = as.factor(ifelse(is.na(dfs_pullpush_area) | dfs_pullpush_area == 0, "0", "1")),
    
    # total area (ha) of cropland under diversified farming systems
    across(starts_with("dfs_") & ends_with("_area"), ~ as.numeric(as.character(.))),
    dfs_total_area = rowSums(select(., starts_with("dfs_") & ends_with("_area")) %>% mutate(across(everything(), as.numeric)), na.rm = TRUE),
    # adoption of diversified farming systems binary (1=yes,0=no)
    dfs_adoption_binary = as.factor(ifelse(dfs_total_area > 0, "1","0")))


x <- zwe_data_clean %>%
  select(dfs_agroforestry_area,dfs_agroforestry_adoption,
         dfs_cover_crops_area,dfs_cover_crops_adoption,
         dfs_crop_rotation_area,dfs_crop_rotation_adoption,
         dfs_fallow_area,dfs_fallow_adoption,
         dfs_hedgerows_area,dfs_hedgerows_adoption,
         dfs_homegarden_area,dfs_homegarden_adoption,
         dfs_intercropping_area, dfs_intercropping_adoption,
         dfs_strip_vegetation_area,dfs_strip_vegetation_adoption,
         dfs_pullpush_area,dfs_pullpush_adoption,
         dfs_adoption_binary)

table(x$dfs_adoption_binary)

#############################################################
########## FACTORS CALCULATION #####-----
#############################################################
### BIOPHYSICAL CONTEXT ----
zwe_data_clean<-zwe_data_clean%>%
  #Soil depth
  mutate(across(starts_with("soil_depth_"), ~str_extract(.x, "(?<=depth_).*")))%>%
  mutate(across(starts_with("soil_depth_"), ~ as.numeric(as.character(.))))%>%
  mutate(soil_depth = rowSums(as.matrix(select(., starts_with("soil_depth_"))), na.rm = TRUE)/3)%>%
  mutate(soil_depth= round(soil_depth, digits=0))%>%
  #mutate(soil_depth=paste0("depth_",soil_depth))%>%
  #Year of assessment
  mutate(year_assessment= str_extract(end, "^\\d{4}"))%>%
  #Drought experience -> EXCLUDED! IT WAS ANSWER IN THE WRONG WAY
  mutate(drought_experience= as.factor(drought_experience))%>%
  mutate(drought_experience=case_when(drought_experience%in%c("notsure")~"0",TRUE~drought_experience))%>%
  #Flood experience -> EXCLUDED! IT WAS ANSWER IN THE WRONG WAY
  mutate(flood_experience= as.factor(flood_experience))%>%
  mutate(flood_experience=case_when(flood_experience%in%c("notsure")~"0",TRUE~flood_experience))


### FINANCIAL CAPITAL ----
zwe_data_clean<-zwe_data_clean%>%
  #Income sources
  mutate(across(starts_with("income_sources."), ~ as.numeric(as.character(.))))%>%
  mutate(num_income_sources = rowSums(as.matrix(select(., starts_with("income_sources."))), na.rm = TRUE))%>%
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
                                                             income_amount_fish))), na.rm = TRUE),
         income_amount_onfarm=income_amount_onfarm/350.917)%>% #Exchange rate: 1 USD = 350.917 Zimbabwean Dollar (World Bank, 2025).
  
  #Non-farm income amount
  mutate(income_amount_nonfarm= rowSums(as.matrix(select(., c(income_amount_family_business,
                                                              income_amount_casual_labour,
                                                              income_amount_formal_labour,
                                                              income_amount_transfers,
                                                              income_amount_leasing_land,
                                                              income_amount_subsidy,
                                                              income_amount_other))), na.rm = TRUE),
         income_amount_nonfarm=income_amount_nonfarm/350.917)%>% #Exchange rate: 1 USD = 350.917 Zimbabwean Dollar (World Bank, 2025).
  #Total income amount
  mutate(income_amount_total= income_amount_onfarm+income_amount_nonfarm)%>%
  #Access to credit
  mutate(credit_access= ifelse(credit=="2", "1","0"))%>%
  #Access to credit is a constraint
  mutate(credit_access_constraint= ifelse(credit=="1", "1","0"))%>%
  #Number of assets
  mutate(assets_other=case_when(
    assets_other==9999~ 1,
    is.na(assets_other)~ 0,
    TRUE~assets_other))%>%
  mutate(across(setdiff(names(.), "assets_other_name")[startsWith(setdiff(names(.), "assets_other_name"), "assets_")],
                ~ as.numeric(as.character(.))))%>%
  mutate(assets_count =rowSums(across(setdiff(names(.), "assets_other_name")[startsWith(setdiff(names(.), "assets_other_name"), "assets_")])))%>%
  #Livestock count
  #Reference: Marc BENOIT, Patrick VEYSSET Livestock unit calculation: a method based on energy requirements to refine the study of livestock farming systems
  mutate(livestock_count_tlu= 
           (livestock_main_animal_number_Cattle*1)+ #Cattle TLU=1
           (livestock_main_animal_number_Chickens*0.01)+ #Poultry 0.01
           (livestock_main_animal_number_Donkeys*0.5)+ #Donkey 0.01
           (livestock_main_animal_number_Ducks*0.01)+ #Poultry 0.01
           (livestock_main_animal_number_Goats*0.1)+ #Goats 0.1
           (livestock_main_animal_number_Pigs*0.2)+ #Pigs 0.2
           (livestock_main_animal_number_Sheep*0.1)+ #Sheeps 0.1
           (livestock_main_animal_number_Turkeys*0.01), #Poultry 0.01
           #(livestock_main_animal_number_Cuyes*0.01)+ #Cuyes 0.01
           #(livestock_main_animal_number_rabbits*0.02),
         livestock_count_tlu=ifelse(is.na(livestock_count_tlu),0,livestock_count_tlu))%>% #Rabbits 0.01
  #Household held a debt
  mutate(household_held_debt=case_when(credit_payment_full=="0"~ "1",TRUE~"0"))%>%
  #High-cost roof materials
  mutate(high_cost_roof_material= case_when(
    roof_material.Galvanized_iron_or_aluminum_or_other_metal_sheets=="1"~"1",
    roof_material.Concrete._brick._stone=="1"~"1",
    #roof_material.Concrete=="1"~"1",
    #roof_material._stone=="1"~"1",
    TRUE~"0"))%>%
  #High-cost walls materials
  mutate(high_cost_walls_material= case_when(
    walls_material.Bricks=="1"~"1",
    walls_material.Stones=="1"~"1",
    walls_material.Iron_sheet=="1"~"1",
    TRUE~"0"))%>%
  #Farm is not profitable
  mutate(financial_deficit= case_when(financial_deficit=="2"~"0",TRUE~financial_deficit))


### HUMAN CAPITAL ----
zwe_data_clean<-zwe_data_clean%>%
  #Household head age
  mutate(age = 2025-year_birth)%>%
  #Level of education farmer
  mutate(across(starts_with("education_years_finished"),~ as.numeric(as.character(.))))%>%
  mutate(education_level_finished= case_when(
    education_level%in%c("0")~"0",
    education_level%in%c("1") & education_years_finished_primary<7~"0",
    education_level%in%c("1") & education_years_finished_primary==7~"1",
    education_level%in%c("2") & education_years_finished_secondary<7~"1",
    education_level%in%c("2") & education_years_finished_secondary==7~"2",
    education_level%in%c("3") & education_years_finished_tertiary<10~"2",
    education_level%in%c("3") & education_years_finished_tertiary==10~"3",
    TRUE~NA))%>%
  #Level of education of most male household members
  mutate(education_level_male= case_when(
    is.na(education_level_male)~"0",
    TRUE~education_level_male))%>%
  #Level of education of most female household members
  mutate(education_level_female= case_when(
    is.na(education_level_female)~"0",
    TRUE~education_level_female))%>%
  
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
         num_hlabour_permanent_total= rowSums(across(starts_with("num_workers_hlabour_permanent_")), na.rm = TRUE),
         #Total number of seasonal hired labour
         num_hlabour_seasonal_total= rowSums(across(starts_with("num_workers_hlabour_seasonal_")), na.rm = TRUE),
         #Total number of permanent household labour
         num_nhlabour_permanent_total=rowSums(across(starts_with("num_workers_nhlabour_permanent_")), na.rm = TRUE),
         #Total number of seasonal household labour
         num_nhlabour_seasonal_total=rowSums(across(starts_with("num_workers_nhlabour_seasonal_")), na.rm = TRUE),
         #Farm has hired/free/exchange labour
         hlabour = num_hlabour_permanent_total+num_hlabour_seasonal_total,
         hlabour= ifelse(hlabour>0, "1","0"),
         #Number of secondary occupations
         across(starts_with("occupation_secondary_list"), ~ as.numeric(as.character(.))),  
         num_occupation_secondary_list = rowSums(as.matrix(select(., starts_with("occupation_secondary_list"))), na.rm = TRUE),
         #Farmer as a primary occupation
         occupation_primary_farmer = ifelse(occupation_primary=="1", "1","0"))%>%
  #Full-time farmer
  mutate(full_time_farmer= case_when(occupation_primary_farmer=="1"&occupation_secondary=="0"~"1",TRUE~"0"))

### NATURAL CAPITAL ----
zwe_data_clean<-zwe_data_clean%>%
  # Farm size
  mutate(livestockland_area_owned=ifelse(livestockland_area_owned==9999,0,livestockland_area_owned),
         livestockland_area_shared=ifelse(livestockland_area_shared==9999,0,livestockland_area_shared))%>% 
  mutate(land_tenure_own_area=land_tenure_own_area*0.404686,#convert to hectares
         land_tenure_hold_area=land_tenure_hold_area*0.404686,#convert to hectares
         land_tenure_lease_area=land_tenure_lease_area*0.404686,#convert to hectares
         cropland_area=cropland_area*0.404686,#convert to hectares
         livestockland_area_owned=livestockland_area_owned*0.404686,#convert to hectares
         livestockland_area_shared=livestockland_area_shared*0.404686, #convert to hectares
         fishland_area=fishland_area*0.404686 #convert to hectares
         )%>%
  mutate(farm_size= land_tenure_own_area+land_tenure_lease_area+land_tenure_hold_area)%>%
  mutate(cropland_area= ifelse(cropland_area=="NA", 0,cropland_area),
         cropland_area= ifelse(is.na(cropland_area), 0,cropland_area))%>%
  #Years of farming land
  mutate(years_farming_land = 2025-year_start_farming_land)%>%
  #Livestock land area
  mutate(livestockland_area= livestockland_area_owned+livestockland_area_shared,
         livestockland_area= ifelse(is.na(livestockland_area), 0,livestockland_area),
         livestockland_area= ifelse(livestockland_area=="NA", 0,livestockland_area))%>%
  #Fishland area
  mutate(fishland_area= ifelse(is.na(fishland_area), 0,fishland_area))%>%
  #Total production area
  
  mutate(total_production_area= cropland_area+livestockland_area+fishland_area)%>%
  #Area under sustainable agricultural practices
  mutate(across(.cols = where(~ any(grepl("^ecol_.*_area$", names(df)))),.fns = ~ . * 0.404686))%>%
  mutate(sfp_total_area = rowSums(as.matrix(select(., dfs_total_area,starts_with("ecol_") & ends_with("_area"))), na.rm = TRUE))%>%
  
  
  #Months of water shortage during a normal year
  mutate(months_count_water_accessibility_difficulty_normal_year = rowSums(as.matrix(select(., starts_with("water_accessibility_difficulty_normal_year."))), na.rm = TRUE))%>%
  #Months of water shortage during a flood year
  mutate(months_count_water_accessibility_difficulty_flood_year = rowSums(as.matrix(select(., starts_with("water_accessibility_difficulty_flood_year."))), na.rm = TRUE))%>%
  #Months of water shortage during a drought year
  mutate(months_count_water_accessibility_difficulty_drought_year = rowSums(as.matrix(select(., starts_with("water_accessibility_difficulty_drought_year."))), na.rm = TRUE))%>%
  #Vegetation cover
  mutate(across(starts_with("vegetation_diversity_"), ~as.numeric(replace_na(as.numeric(as.character(.)), 0))))

### HUMAN CAPITAL ----
zwe_data_clean<-zwe_data_clean%>%
  mutate(
    #Total number of permanent hired labour per ha
    numHA_hlabour_permanent_total= num_hlabour_permanent_total/farm_size,
    numHA_hlabour_permanent_total=ifelse(is.na(numHA_hlabour_permanent_total),0,numHA_hlabour_permanent_total),
    #Total number of seasonal hired labour per ha
    numHA_hlabour_seasonal_total= num_hlabour_seasonal_total/farm_size,
    numHA_hlabour_seasonal_total=ifelse(is.na(numHA_hlabour_seasonal_total),0,numHA_hlabour_seasonal_total),
    #Total number of permanent household labour per ha
    numHA_nhlabour_permanent_total=num_nhlabour_permanent_total/farm_size,
    numHA_nhlabour_permanent_total=ifelse(is.na(numHA_nhlabour_permanent_total),0,numHA_nhlabour_permanent_total),
    numHA_nhlabour_permanent_total=ifelse(numHA_nhlabour_permanent_total==Inf,0,numHA_nhlabour_permanent_total),
    
    #Total number of seasonal household labour
    numHA_nhlabour_seasonal_total=num_nhlabour_seasonal_total/farm_size,
    numHA_nhlabour_seasonal_total=ifelse(is.na(numHA_nhlabour_seasonal_total),0,numHA_nhlabour_seasonal_total))


  
  
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
zwe_data_clean <- classify_energy_type(zwe_data_clean, "energy_irrigation", renewable_keywords, nonrenewable_keywords, "energy_irrigation_type")%>%
  mutate(energy_irrigation_type= ifelse(is.na(energy_irrigation_type), 0,energy_irrigation_type))
#Type of energy used for: Tillage, sowing or harvesting
zwe_data_clean <- classify_energy_type(zwe_data_clean, "energy_tillage_haverst", renewable_keywords, nonrenewable_keywords, "energy_tillage_haverst_type")
#Type of energy used for: Cooking
zwe_data_clean <- classify_energy_type(zwe_data_clean, "energy_cooking", renewable_keywords, nonrenewable_keywords, "energy_cooking_type")%>%
  mutate(energy_cooking_type=ifelse(is.na(energy_cooking_type),5,energy_cooking_type))
#Type of energy used for: Cleaning, processing or transporting harvested food
zwe_data_clean <- classify_energy_type(zwe_data_clean, "energy_cleaning_transporting", renewable_keywords, nonrenewable_keywords, "energy_cleaning_transporting_type")
#Type of energy used for: General
zwe_data_clean<-zwe_data_clean%>%
  mutate(energy_type = rowMedians(as.matrix(select(., c(energy_irrigation_type,
                                                        energy_tillage_haverst_type,
                                                        energy_cooking_type,
                                                        energy_cleaning_transporting_type))), na.rm = TRUE))

zwe_data_clean<-zwe_data_clean%>%
  #Access to basic services
  mutate(across(starts_with("accessibility_"), ~ as.numeric(as.character(.))))%>%
  mutate(num_accessibility_basic_services = rowSums(select(., starts_with("accessibility_")), na.rm = TRUE))%>%
  mutate(accessibility_basic_services= case_when(
    num_accessibility_basic_services==0 ~ "0",
    num_accessibility_basic_services>0 & num_accessibility_basic_services<3~ "2",
    num_accessibility_basic_services>2 ~ "5",
    TRUE~ NA))%>%
  #Access to sanitation services
  mutate(accessibility_sanitation= case_when(
    accessibility_piped_sewer==1 &accessibility_waste_collection==1~ "5",
    accessibility_piped_sewer==0 &accessibility_waste_collection==1~ "2",
    accessibility_piped_sewer==1 &accessibility_waste_collection==0~ "2",
    accessibility_piped_sewer==0 &accessibility_waste_collection==0~ "0",
    TRUE~ NA)) %>%
  #Distances to services
  #Distance to the closest farmland
  mutate(across(starts_with("distance_"), ~ as.numeric(as.character(.))))%>%
  mutate(distance_closest_farmland = case_when(
    mode_distance_closest_farmland %in% c("walking")& metric_distance_closest_farmland %in% c("minutes")~distance_closest_farmland,
    mode_distance_closest_farmland %in% c("motobike","motorbike", "car")& metric_distance_closest_farmland %in% c("minutes")~distance_closest_farmland*10, # assumes car speed of 50km/h and walking speed of 5 km/h  
    mode_distance_closest_farmland %in% c("cycling")& metric_distance_closest_farmland %in% c("minutes")~distance_closest_farmland*3, # assumes bike speed of 15km/h and walking speed of 5 km/h
    mode_distance_closest_farmland %in% c("horse","donkey")& metric_distance_closest_farmland %in% c("minutes")~distance_closest_farmland*2, # assumes horse speed of 10km/h and walking speed of 5 km/h
    metric_distance_closest_farmland %in% c("km")~(distance_closest_farmland/5)*60, # assumes walking speed of 5km/h
    TRUE~distance_closest_farmland))%>%
  #Distance to the closest fresh water source
  mutate(distance_water_source = case_when(
    mode_distance_water_source%in% c("walking")& metric_distance_water_source %in% c("minutes")~distance_water_source,
    mode_distance_water_source %in% c("motobike","motorbike", "car")& metric_distance_water_source %in% c("minutes")~distance_water_source*10, # assumes car speed of 50km/h and walking speed of 5 km/h  
    mode_distance_water_source %in% c("cycling")& metric_distance_water_source %in% c("minutes")~distance_water_source*3, # assumes bike speed of 15km/h and walking speed of 5 km/h
    mode_distance_water_source %in% c("horse","donkey")& metric_distance_water_source %in% c("minutes")~distance_water_source*2, # assumes horse speed of 10km/h and walking speed of 5 km/h
    metric_distance_water_source %in% c("km")~(distance_water_source/5)*60, # assumes walking speed of 5km/h
    TRUE~distance_water_source))%>%
  #Distance to livestock market
  mutate(distance_livestock_market = case_when(
    mode_distance_livestock_market%in% c("walking")& metric_distance_livestock_market %in% c("minutes")~distance_livestock_market,
    mode_distance_livestock_market%in% c("motobike","motorbike", "car")& metric_distance_livestock_market %in% c("minutes")~distance_livestock_market*10, # assumes car speed of 50km/h and walking speed of 5 km/h  
    mode_distance_livestock_market %in% c("cycling")& metric_distance_livestock_market %in% c("minutes")~distance_livestock_market*3, # assumes bike speed of 15km/h and walking speed of 5 km/h
    mode_distance_livestock_market %in% c("horse","donkey")& metric_distance_livestock_market %in% c("minutes")~distance_livestock_market*2, # assumes horse speed of 10km/h and walking speed of 5 km/h
    metric_distance_livestock_market %in% c("km")~(distance_livestock_market/5)*60, # assumes walking speed of 5km/h
    TRUE~distance_livestock_market))%>%
  #Distance to crop market
  mutate(distance_crop_market = case_when(
    mode_distance_crop_market%in% c("walking")& metric_distance_crop_market %in% c("minutes")~distance_crop_market,
    mode_distance_crop_market%in% c("motobike","motorbike", "car")& metric_distance_crop_market %in% c("minutes")~distance_crop_market*10, # assumes car speed of 50km/h and walking speed of 5 km/h  
    mode_distance_crop_market %in% c("cycling")& metric_distance_crop_market %in% c("minutes")~distance_crop_market*3, # assumes bike speed of 15km/h and walking speed of 5 km/h
    mode_distance_crop_market %in% c("horse","donkey")& metric_distance_crop_market %in% c("minutes")~distance_crop_market*2, # assumes horse speed of 10km/h and walking speed of 5 km/h
    metric_distance_crop_market %in% c("km")~(distance_crop_market/5)*60, # assumes walking speed of 5km/h
    TRUE~distance_crop_market))%>%
  #Distance to main road
  mutate(distance_main_road = case_when(
    mode_distance_main_road%in% c("walking")& metric_distance_main_road %in% c("minutes")~distance_main_road,
    mode_distance_main_road%in% c("motobike","motorbike", "car")& metric_distance_main_road %in% c("minutes")~distance_main_road*10, # assumes car speed of 50km/h and walking speed of 5 km/h  
    mode_distance_main_road %in% c("cycling")& metric_distance_main_road %in% c("minutes")~distance_main_road*3, # assumes bike speed of 15km/h and walking speed of 5 km/h
    mode_distance_main_road %in% c("horse","donkey")& metric_distance_main_road %in% c("minutes")~distance_main_road*2, # assumes horse speed of 10km/h and walking speed of 5 km/h
    metric_distance_main_road %in% c("km")~(distance_main_road/5)*60, # assumes walking speed of 5km/h
    TRUE~distance_main_road))%>%
  mutate(metric_distance_closest_farmland= case_when(country=="zimbabwe"~ "minutes",TRUE~"NA"),
         metric_distance_water_source=case_when(country=="zimbabwe"~ "minutes",TRUE~"NA"),
         metric_distance_livestock_market=case_when(country=="zimbabwe"~ "minutes",TRUE~"NA"),
         metric_distance_crop_market=case_when(country=="zimbabwe"~ "minutes",TRUE~"NA"),
         metric_distance_main_road=case_when(country=="zimbabwe"~ "minutes",TRUE~"NA"))%>%
  #Mean of transportation
  rename("assets_scotchcart"="X_4_1_4_12_localization")%>%
  mutate(mean_transport= case_when(
    assets_car>0~ "1",
    assets_motorbike>0~ "1",
    assets_bicycle>0~ "1",
    assets_scotchcart>0~ "1",
    TRUE~ "0"))%>%
  #hh has a mobile phone
  mutate(access_mobile_phone= case_when(
    assets_mobile_phone=="1"~ "1",
    assets_smarthphone=="1"~ "1",
    TRUE~ "0"))%>%
  #hh has machinery
  mutate(access_machinery= case_when(
    assets_ox_plough>0~ "1",
    assets_tractor>0~ "1",
    assets_plow>0~ "1",
    assets_seed_drill>0~ "1",
    str_detect(assets_other_name, "Hoes|Hand hoes|Grindingmill") ~ "1",
    TRUE~ "0"))%>%
  #Access to irrigation
  mutate(across(starts_with("irrigation_method."), ~ as.numeric(as.character(.))),
         access_irrigation_method= rowSums(select(., starts_with("irrigation_method.")) %>% mutate(across(everything(), as.numeric)), na.rm = TRUE),
         access_irrigation_method=ifelse(access_irrigation_method>0, "1","0"))%>%
  #Irrigated land
  mutate(irrigated_land_percentage= ifelse(is.na(irrigated_land_percentage), 0,irrigated_land_percentage))%>%
  #Access to rainwater harvesting systems
  mutate(across(starts_with("rainwater_harvesting."), ~ as.numeric(as.character(.))),
         access_water_harvest_system= rowSums(select(., starts_with("rainwater_harvesting.")) %>% mutate(across(everything(), as.numeric)), na.rm = TRUE),
         access_water_harvest_system=access_water_harvest_system-rainwater_harvesting.none,
         access_water_harvest_system=ifelse(access_water_harvest_system>0, "1","0"))


### POLITICAL AND INSTITUTIONAL CONTEXT: Financial risk management ----
zwe_data_clean<-zwe_data_clean%>%
  #Access to insurance against agricultural losses
  mutate(insurance_agric_losses_access= case_when(insurance_agric_losses_level=="0" ~ "0",TRUE~ "1"))%>%
  #Amount of subsidy received as income support
  mutate(income_amount_subsidy=income_amount_subsidy/350.917)%>% #Exchange rate: 1 USD = 350.917 Zimbabwean Dollar (World Bank, 2025).
  
  mutate(income_amount_subsidy= ifelse(is.na(income_amount_subsidy),0,income_amount_subsidy))%>%
  #Household with children attending school and receiving free meals
  mutate(num_free_school_meals_perweek=as.numeric(num_free_school_meals_perweek))%>%
  mutate(access_free_school_meals_perweek= case_when(
    children_attend_school=="0" ~ "0",
    is.na(children_attend_school) ~ "0",
    children_attend_school=="1" & num_free_school_meals_perweek==0  ~ "2",
    children_attend_school=="1" & num_free_school_meals_perweek>0  ~ "5",
    TRUE~ NA))


### POLITICAL AND INSTITUTIONAL CONTEXT: Knowledge ----
zwe_data_clean<-zwe_data_clean%>%
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

### POLITICAL AND INSTITUTIONAL CONTEXT: Land tenure ----
zwe_data_clean<-zwe_data_clean%>%
  mutate(
    #Land tenure status of hosehold: Own
    land_tenure_own_status= ifelse(zwe_data_clean$land_tenure_own_area>0, "1","0"),
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
  #Proportion of land owns
  mutate(land_tenure_own_proportion= (land_tenure_own_area/farm_size)*100,
         land_tenure_own_proportion=ifelse(is.na(land_tenure_own_proportion),0,land_tenure_own_proportion),
         #Proportion of land  leases
         land_tenure_lease_proportion= (land_tenure_lease_area/farm_size)*100,
         land_tenure_lease_proportion=ifelse(is.na(land_tenure_lease_proportion),0,land_tenure_lease_proportion),
         #Proportion of land hold use rights
         land_tenure_hold_proportion= (land_tenure_hold_area/farm_size)*100,
         land_tenure_hold_proportion=ifelse(is.na(land_tenure_hold_proportion),0,land_tenure_hold_proportion))%>%

  #Proportion of land owns by a male member
  mutate(male_land_tenure_own_proportion= (male_land_tenure_own_area/land_tenure_own_area)*100,
         male_land_tenure_own_proportion= ifelse(is.na(male_land_tenure_own_proportion),0,male_land_tenure_own_proportion),
         #Proportion of land owns by a female member
         female_land_tenure_own_proportion= (female_land_tenure_own_area/land_tenure_own_area)*100,
         female_land_tenure_own_proportion= ifelse(is.na(female_land_tenure_own_proportion),0,female_land_tenure_own_proportion),
         #Proportion of land leases by a male member
         male_land_tenure_lease_proportion=(male_land_tenure_lease_area/land_tenure_lease_area)*100,
         male_land_tenure_lease_proportion= ifelse(is.na(male_land_tenure_lease_proportion),0,male_land_tenure_lease_proportion),
         #Proportion of land leases by a female member
         female_land_tenure_lease_proportion=(female_land_tenure_lease_area/land_tenure_lease_area)*100,
         female_land_tenure_lease_proportion= ifelse(is.na(female_land_tenure_lease_proportion),0,female_land_tenure_lease_proportion),
         #Proportion of land hold use rights by a male member
         male_land_tenure_hold_proportion=(male_land_tenure_hold_area/land_tenure_hold_area)*100,
         male_land_tenure_hold_proportion= ifelse(is.na(male_land_tenure_hold_proportion),0,male_land_tenure_hold_proportion),
         #Proportion of land hold use rights by a female member
         female_land_tenure_hold_proportion=(female_land_tenure_hold_area/land_tenure_hold_area)*100,
         female_land_tenure_hold_proportion= ifelse(is.na(female_land_tenure_hold_proportion),0,female_land_tenure_hold_proportion))%>%
  # Gender of land tenure
  mutate(gender_land_tenure_own=case_when(
    male_land_tenure_own_area>0 & female_land_tenure_own_area>0~"shared",
    male_land_tenure_own_area>0 & female_land_tenure_own_area==0~"male",
    male_land_tenure_own_area==0 & female_land_tenure_own_area>0~"female",
    TRUE~NA))%>%
  mutate(gender_land_tenure_lease=case_when(
    male_land_tenure_lease_area>0 & female_land_tenure_lease_area>0~"shared",
    male_land_tenure_lease_area>0 & female_land_tenure_lease_area==0~"male",
    male_land_tenure_lease_area==0 & female_land_tenure_lease_area>0~"female",
    TRUE~NA))%>%
  mutate(gender_land_tenure_hold=case_when(
    male_land_tenure_hold_area>0 & female_land_tenure_hold_area>0~"shared",
    male_land_tenure_hold_area>0 & female_land_tenure_hold_area==0~"male",
    male_land_tenure_hold_area==0 & female_land_tenure_hold_area>0~"female",
    TRUE~NA))%>%
  mutate(gender_land_tenure = case_when(
    if_any(c(gender_land_tenure_own, gender_land_tenure_lease, gender_land_tenure_hold), ~ . == "shared") ~ "shared",
    if_all(c(gender_land_tenure_own, gender_land_tenure_lease, gender_land_tenure_hold), ~ . == "female") ~ "female",
    if_all(c(gender_land_tenure_own, gender_land_tenure_lease, gender_land_tenure_hold), ~ . == "male") ~ "male",
    if_any(c(gender_land_tenure_own, gender_land_tenure_lease, gender_land_tenure_hold), ~ . == "male")&
      if_any(c(gender_land_tenure_own, gender_land_tenure_lease, gender_land_tenure_hold), ~ . == "female")~ "shared",
    if_any(c(gender_land_tenure_own, gender_land_tenure_lease, gender_land_tenure_hold), ~ . != "male") ~ "female",
    if_any(c(gender_land_tenure_own, gender_land_tenure_lease, gender_land_tenure_hold), ~ . != "female") ~ "male",
    TRUE ~ NA))


### POLITICAL AND INSTITUTIONAL CONTEXT: Value chain ----
zwe_data_clean<-zwe_data_clean%>%
  #Perception of price fairness: crops
  mutate(fair_price_crops= case_when(
    farm_products.Crops =="0"~ "0", #does not produce 
    use_percentage_crops_sales=="0"~ "0", #does not produce 
    TRUE~ fair_price_crops))%>%
  #Perception of price fairness: livestock
  mutate(fair_price_livestock= case_when(
    farm_products.Livestock =="0"~ "0", #does not produce 
    use_percentage_livestock_sales=="0"~ "0", #does not produce 
    TRUE~ fair_price_livestock))%>%
  #Perception of price fairness: fish
  mutate(fair_price_fish= case_when(
    farm_products.Fish =="0"~ "0", #does not produce 
    use_percentage_fish_sales=="0"~ "0", #does not produce 
    TRUE~ fair_price_fish))%>%
  #Perception of price fairness: wood
  mutate(fair_price_wood= case_when(
    farm_products.Trees =="0"~ "0", #does not produce 
    use_percentage_trees_sales=="0"~ "0", #does not produce 
    TRUE~ fair_price_wood))%>%
  #Perception of price fairness: honey
  mutate(fair_price_honey= case_when(
    farm_products.Honey =="0"~ "0", #does not produce 
    use_percentage_honey_sales=="0"~ "0", #does not produce 
    TRUE~ fair_price_honey))%>%
  #Crop sales channel
  rename("sales_channel_crops.middle_man.aggregator"="sales_channel_crops.middle_man_aggregator")%>%
  rename("sales_channel_crops.retailers"="sales_channel_crops.trader_or_supermarket")%>%
  mutate(across(starts_with("sales_channel_crops."), ~ as.numeric(as.character(.))))%>%
  mutate(across(starts_with("sales_channel_crops."),~replace_na(., 0)))%>%
  mutate(num_sales_channel_crops = rowSums(select(., starts_with("sales_channel_crops.")), na.rm = TRUE))%>%
  #Livestock sales channel
  mutate(across(starts_with("sales_channel_livestock."), ~ as.numeric(as.character(.))))%>%
  mutate(num_sales_channel_livestock = rowSums(select(., starts_with("sales_channel_livestock.")), na.rm = TRUE))%>%
  #Fish sales channel
  mutate(across(starts_with("sales_channel_fish."), ~ as.numeric(as.character(.))))%>%
  mutate(num_sales_channel_fish = rowSums(select(., starts_with("sales_channel_fish.")), na.rm = TRUE))%>%
  #Wood sales channel
  mutate(across(starts_with("sales_channel_trees."), ~ as.numeric(as.character(.))))%>%
  mutate(num_sales_channel_trees = rowSums(select(., starts_with("sales_channel_trees.")), na.rm = TRUE))%>%
  #Honey sales channel
  mutate(across(starts_with("sales_channel_honey."), ~ as.numeric(as.character(.))))%>%
  mutate(num_sales_channel_honey = rowSums(select(., starts_with("sales_channel_honey.")), na.rm = TRUE))%>%
  #Household sale on farm products
  mutate(farm_products_sale= case_when(
    num_sales_channel_crops>0~ "1",
    num_sales_channel_livestock>0~ "1",
    num_sales_channel_fish>0~ "1",
    num_sales_channel_trees>0~ "1",
    num_sales_channel_honey>0~ "1",
    TRUE~"0"))%>%
  #Perceived market characteristics as risk/shock
  mutate(perceived_shock_market= case_when(
    str_detect(crop_damage_cause, "No enough market")~ "1",
    household_shock.5 =="1"~ "1", #Market disruptions
    household_shock.10 =="1"~ "1", #Price fluctuations in market
    TRUE~"0"))

### SOCIAL CAPITAL ----
zwe_data_clean<-zwe_data_clean%>%
  #Number of association/organization memberships
  mutate(across(starts_with("membership."), ~ as.numeric(as.character(.))))%>%
  mutate(num_membership = rowSums(select(., starts_with("membership.")), na.rm = TRUE) -
           rowSums(select(., c("membership.none", "membership.i.dont.know")), na.rm = TRUE))%>%
  #Association/organization membership 
  mutate(membership=ifelse(num_membership>0, "1","0"))%>%
  #Support from financial institutions
  mutate(across(starts_with("support_provider."),~replace_na(str_replace(.x, "i-dont-know", "0"),"0")))%>%
  mutate(support_provider_financial_institution= case_when(
    support_provider.bank=="1"~"1",
    support_provider.moneylenders=="1"~"1",
    TRUE~"0"))%>%
  #Support from governmental institutions
  mutate(support_provider_governmental_institution= case_when(
    support_provider.local_government=="1"~"1",
    support_provider.national_government=="1"~"1",
    TRUE~"0"))%>%
  #Support from community and social networks
  mutate(support_provider_community= case_when(
    support_provider.community_leaders=="1"~"1",
    support_provider.ind_different_community=="1"~"1",
    support_provider.ind_own_community=="1"~"1",
    TRUE~"0"))%>%
  #Support from community and social networks
  mutate(support_provider_cooperative_organizations= case_when(
    support_provider.cooperatives=="1"~"1",
    support_provider.farmer_organization=="1"~"1",
    support_provider.local_organization=="1"~"1",
    TRUE~"0"))%>%
  #Number of institutions that will support household
  mutate(across(starts_with("support_provider."), ~ as.numeric(as.character(.))),
         support_provider_count= rowSums(select(., starts_with("support_provider.")) %>% mutate(across(everything(), as.numeric)), na.rm = TRUE))

### FARM MANAGEMENT CHARACTERISTICS ----
zwe_data_clean<-zwe_data_clean%>%
  #Farmer use chemical fertilizers
  rename("soil_fertility_management_chemical"="soil_fertility_management.1")%>%
  #Farmer use organic fertilizers or manure
  rename("soil_fertility_management_organic"="soil_fertility_management.2")%>%
  #Farmer use ecological practices to improve soil fertility
  rename("soil_fertility_management_ecol_practices"="soil_fertility_management.3")%>%
  #Number of on-farm products
  mutate(across(starts_with("farm_products."), ~ as.numeric(as.character(.))),
         num_farm_products = rowSums(across(starts_with("farm_products.")), na.rm = TRUE))%>%
  #Number of ecological practices use on cropland to improve soil quality and health
  mutate(across(starts_with("soil_fertility_management_ecol_practices."),~replace_na(., 0)))%>%
  mutate(across(starts_with("soil_fertility_management_ecol_practices."),~ifelse(. == "NA", 0, .)))%>%
  mutate(across(starts_with("soil_fertility_management_ecol_practices."), ~ as.numeric(as.character(.))),
         num_soil_fertility_ecol_practices = rowSums(across(starts_with("soil_fertility_management_ecol_practices.")), na.rm = TRUE))%>%
  #Use Chemical fungicides/pesticides/herbicides.
  rename("pest_management_chemical"="pest_management.1")%>%
  #Use Non-chemical fungicides/pesticides/herbicides.
  rename("pest_management_organic"="pest_management.2")%>%
  #Use Ecological practices (e.g., crop rotation, planting repelling plants).
  rename("pest_management_ecol_practices"="pest_management.3")%>%
  #Number of ecological practices use on cropland to manage pests
  mutate(across(starts_with("pest_management_ecol_practices."),~replace_na(., 0)))%>%
  mutate(across(starts_with("pest_management_ecol_practices."),~ifelse(. == "NA", 0, .)))%>%
  mutate(across(starts_with("pest_management_ecol_practices."), ~ as.numeric(as.character(.))),
         num_pest_management_ecol_practices = rowSums(across(starts_with("pest_management_ecol_practices.")), na.rm = TRUE))%>%
  
  #monoculture perennial adoption
  mutate(sfs_monoculture_perennial_area=sfs_monoculture_perennial_area*0.404686)%>% #convert to hectares
  mutate(sfs_monoculture_perennial_adoption= case_when(sfs_monoculture_perennial_area>0~ "1", TRUE~ "0"))%>%
  #monoculture annual
  mutate(sfs_monoculture_annual_area=sfs_monoculture_annual_area*0.404686)%>% #convert to hectares
  mutate(sfs_monoculture_annual_adoption= case_when(sfs_monoculture_annual_area>0~ "1", TRUE~ "0"))%>%
  #Land clearing
  mutate(sfs_land_clearing_area=sfs_land_clearing_area*0.404686)%>% #convert to hectares
  mutate(sfs_land_clearing_adoption= case_when(sfs_land_clearing_area>0~ "1", TRUE~ "0"))%>%
  #adoption burning residues
  mutate(sfs_burning_residues_area=sfs_burning_residues_area*0.404686)%>% #convert to hectares
  mutate(sfs_burning_residues_adoption= case_when(sfs_burning_residues_area>0~ "1", TRUE~ "0"))%>%
  #PERMANENT HOUSEHOLD LABOUR: total labour hours per year
  mutate(across(starts_with("num_workers_nhlabour_permanent_"), ~ ifelse(is.na(.) |. == 9999, 0, .)),
         across(starts_with("num_hours_nhlabour_permanent_"), ~ ifelse(is.na(.) |. == 9999, 0, .)))%>%
  mutate(total_labour_hours_nhlabour_permanent_adults_wa_female= num_hours_nhlabour_permanent_adults_wa_female*num_workers_nhlabour_permanent_adults_wa_female*12*21)%>%
  mutate(total_labour_hours_nhlabour_permanent_adults_wa_male= num_hours_nhlabour_permanent_adults_wa_male*num_workers_nhlabour_permanent_adults_wa_male*12*21)%>%
  mutate(total_labour_hours_nhlabour_permanent_adults_old_male= num_hours_nhlabour_permanent_adults_old_male*num_workers_nhlabour_permanent_adults_old_male*12*21)%>%
  mutate(total_labour_hours_nhlabour_permanent_adults_old_female= num_hours_nhlabour_permanent_adults_old_female*num_workers_nhlabour_permanent_adults_old_female*12*21)%>%
  mutate(total_labour_hours_nhlabour_permanent_children_male= num_hours_nhlabour_permanent_children_male*num_workers_nhlabour_permanent_children_male*12*21)%>%
  mutate(total_labour_hours_nhlabour_permanent_children_female= num_hours_nhlabour_permanent_children_female*num_workers_nhlabour_permanent_children_female*12*21)%>%
  mutate(total_labour_hours_nhlabour_permanent= rowSums(across(starts_with("total_labour_hours_nhlabour_permanent_")), na.rm = TRUE))%>%
  #SEASONAL HOUSEHOLD LABOUR: total labour hours per year
  mutate(across(starts_with("total_labour_hours_nhlabour_seasonal_"), ~ ifelse(is.na(.) |. == 9999, 0, .)))%>%
  mutate(total_labour_hours_nhlabour_seasonal = rowSums(across(starts_with("total_labour_hours_nhlabour_seasonal_")), na.rm = TRUE))%>%
  #PERMANENT HIRED LABOUR: total labour hours per year
  mutate(across(starts_with("num_workers_hlabour_permanent_"), ~ ifelse(is.na(.) |. == 9999, 0, .)),
         across(starts_with("num_hours_hlabour_permanent_"), ~ ifelse(is.na(.) |. == 9999, 0, .)))%>%
  #mutate(total_labour_hours_hlabour_permanent_adults_wa_male= num_hours_hlabour_permanent_adults_wa_male*num_workers_hlabour_permanent_adults_wa_male*12*21)%>%
  #mutate(total_labour_hours_hlabour_permanent_adults_wa_female= num_hours_hlabour_permanent_adults_wa_female*num_workers_hlabour_permanent_adults_wa_female*12*21)%>%
  mutate(total_labour_hours_hlabour_permanent = rowSums(across(starts_with("total_labour_hours_hlabour_permanent_")), na.rm = TRUE))%>%
  #SEASONAL HIRED LABOUR: total labour hours per year
  mutate(across(starts_with("total_labour_hours_hlabour_seasonal_"), ~ ifelse(is.na(.) |. == 9999, 0, .)))%>%
  mutate(total_labour_hours_hlabour_seasonal = rowSums(across(starts_with("total_labour_hours_hlabour_seasonal_")), na.rm = TRUE))%>%
  #LABOUR PRODUCTIVITY
  mutate(total_labour_hours= rowSums(across(starts_with("total_labour_hours_")), na.rm = TRUE),
         labour_productivity= total_labour_hours/farm_size)%>%
  #Chemical fertilizer per ha
  mutate(chemical_fertilizer_amount_ha= case_when(chemical_fertilizer_unit=="Kilograms"~chemical_fertilizer_amount/chemical_fertilizer_area_affected,TRUE~0))%>%
  #Organic fertilizer and manure per ha
  mutate(organic_fertilizer_onfarm_amount_ha= case_when(organic_fertilizer_onfarm_unit=="Kilograms"~organic_fertilizer_onfarm_amount/organic_fertilizer_onfarm_area_affected,TRUE~0)%>% 
           replace(is.infinite(.), 0),
         organic_fertilizer_offfarm_amount_ha= case_when(organic_fertilizer_offfarm_unit=="Kilograms"~organic_fertilizer_offfarm_amount/organic_fertilizer_offfarm_area_affected,TRUE~0)%>%
           replace(is.infinite(.), 0),
         organic_fertilizer_amount_ha=organic_fertilizer_onfarm_amount_ha+organic_fertilizer_offfarm_amount_ha)%>%
  #Type of crop seeds
  mutate(seeds_certified_local= case_when(
    seeds_certified_local %in%c("5","NA")| # I don't know, or seeds are neither certified or locally adapted..
      is.na(seeds_certified_local)~"0", #Does not produce crops
    TRUE~seeds_certified_local))%>%
  #Are the livestock you keep exotic or local?
  mutate(livestock_exotic_local= case_when(
    livestock_exotic_local %in%c("6","7","NA")| #No exotic or local breeds are kept. | I don't know.
      is.na(livestock_exotic_local)~"0", #Does not produce livestock
    TRUE~livestock_exotic_local))%>%
  #Use percentage
  mutate(across(starts_with("use_percentage_"), ~replace_na(as.character(.), "0")))%>%
  #Household sale produced crops
  mutate(crops_sale= case_when(use_percentage_crops_sales!="0"~ "1",    TRUE~"0"))%>%
  #Household sale produced livestock
  mutate(livestock_sale= case_when(use_percentage_livestock_sales!="0"~ "1",  TRUE~"0"))%>%
  #Household sale produced fish
  mutate(fish_sale= case_when(use_percentage_fish_sales!="0"~ "1",    TRUE~"0"))%>%
  #Household sale produced trees
  mutate(trees_sale= case_when(use_percentage_trees_sales!="0"~ "1",    TRUE~"0"))%>%
  #Household sale produced honey
  mutate(honey_sale= case_when(use_percentage_honey_sales!="0"~ "1",    TRUE~"0"))






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


### FARMER BEHAVIOUR ----
zwe_data_clean<- zwe_data_clean %>%
  #Perceived climatic conditions as risk/shock
  mutate(perceived_shock_climate= case_when(
    str_detect(crop_damage_cause, "Drought|drought|dry|rain|rains|Rainfall|rainfall|Sun|Shortage of water|winds|Flooding|Floods|Waterloging|Water loging")~ "1",
    household_shock.1 =="1"~ "1", #Extreme weather events (e.g. cyclones, dust storm, excess rainfall, insuficient rainfall, frost, high temperatures, high winds)
    TRUE~"0"))%>%
  #Perceived pests as risk/shock
  mutate(perceived_shock_pest= case_when(
    str_detect(crop_damage_cause, "pest|Pest|Mice|mice|rats|Rodents|worm|Weevils|weevils|cricket|Caterpillar|Cartapilla|aphids|Termites")~ "1",
    household_shock.8=="1"~ "1", #Pest or disease outbreaks
    TRUE~"0"))%>%
  #Perceived soil erosion as a production constraint
  mutate(perceived_shock_soil_erosion= case_when(
    soil_erosion_perception=="0"~"0",
    TRUE~"1"))%>%
  #Wildlife conflicts

  #Household decision-making agency
  mutate(across(c(farmer_agency_1, farmer_agency_3), ~ replace_na(as.numeric(as.character(.)), 0)))%>%
  mutate(farmer_agency_1_3 = case_when(
    farmer_agency_1==0 &farmer_agency_3!=0 ~ farmer_agency_3,
    farmer_agency_1!=0 &farmer_agency_3==0 ~ farmer_agency_1,
    farmer_agency_1!=0 &farmer_agency_3!=0 ~ rowMedians(as.matrix(select(., c(farmer_agency_1, farmer_agency_3))), na.rm = TRUE),
    TRUE~ NA))%>%
  mutate(farmer_agency_1_3= round(farmer_agency_1_3, digits=0))


#Distance to the nearest farmer adopting DFS
zwe_data_clean<-zwe_data_clean%>%
  rowwise() %>%
  mutate(
    nearest_distance_dfs_km = tryCatch(
      min(
        distGeo(
          cbind(longitude, latitude),
          cbind(
            (zwe_data_clean$longitude[
              zwe_data_clean$dfs_adoption_binary == "1" & zwe_data_clean$longitude != longitude & zwe_data_clean$latitude != latitude]),
            (zwe_data_clean$latitude[zwe_data_clean$dfs_adoption_binary == "1" & zwe_data_clean$longitude != longitude & zwe_data_clean$latitude != latitude]))),
        na.rm = TRUE) / 1000,  # Convert meters to kilometers
      error = function(e) NA_real_)) %>%
  ungroup()

sort(unique(zwe_data_clean$nearest_distance_dfs_km))

# Nearest farmer has adoption of DFS
zwe_data_clean <- zwe_data_clean %>%
  rowwise() %>%
  mutate(
    nearest_farmer_adopted = tryCatch(
      {
        # Get distances to all other farmers
        distances <- distGeo(
          cbind(longitude, latitude),
          cbind(
            (zwe_data_clean$longitude[zwe_data_clean$kobo_farmer_id != kobo_farmer_id]),
            (zwe_data_clean$latitude[zwe_data_clean$kobo_farmer_id != kobo_farmer_id])))
        # Find the index of the nearest farmer
        nearest_index <- which.min(distances)
        # Check if the nearest farmer has adoption == "1"
        if (length(nearest_index) > 0) {
          as.numeric(zwe_data_clean$dfs_adoption_binary[zwe_data_clean$kobo_farmer_id != kobo_farmer_id][nearest_index] == "1")
        } else {
          NA_real_  # No valid neighbors found
        }},
      error = function(e) NA_real_)) %>%
  ungroup()

zwe_data_clean$nearest_farmer_adopted

#Human well being score
mutate(across(starts_with("human_wellbeing_"), ~ as.numeric(as.factor(.))))%>%
  mutate(human_wellbeing = rowMedians(as.matrix(select(., starts_with("human_wellbeing_"))), na.rm = TRUE))%>%
  mutate(human_wellbeing= round(human_wellbeing, digits=0))%>%
  #Perspective on agroecology score
  mutate(across(starts_with("agroecol_perspective_"), ~ as.numeric(as.factor(.))))%>%
  mutate(agroecol_perspective_median = rowMedians(as.matrix(select(., starts_with("agroecol_perspective_"))), na.rm = TRUE))


### VULNERABILITY CONTEXT ----
zwe_data_clean<-zwe_data_clean%>%
  #Perceived credit repayment confidence
  mutate(credit_payment_hability= case_when(
    credit_payment_full== "1"~ "5",
    is.na(credit_payment_full)~ "0",
    TRUE~credit_payment_hability))%>%
  #Perceived indebtedness as risk/shock
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
  mutate(across(starts_with("household_shock_recover_activities."),~replace_na(., 0)))%>%
  mutate(across(starts_with("household_shock_recover_activities."),~ifelse(. == "NA", 0, .)))%>%
  
  
  
  #mutate(household_shock_recover_activities.5 = NA)%>%
  #mutate(household_shock_strategy_absorptive= case_when(
  #  household_shock_recover_activities.5=="1"~ "1", #Reduced area under cultivation
  #household_shock_recover_activities.6=="1"~ "1", #Reduced food consumption
  #household_shock_recover_activities.7=="1"~ "1", #Reduced household expenditure
  #household_shock_recover_activities.9=="1"~ "1", #Sold assets
  #household_shock_recover_activities.12=="1"~ "1", #Taken loans
  #TRUE~"0"))%>%
  #Adaptive strategies to cope with shock
  #mutate(household_shock_strategy_adaptive= case_when(
  #household_shock_recover_activities.2=="1"~ "1", #Diversified on-farm income sources of income
  #household_shock_recover_activities.3=="1"~ "1", #Engage in off-farm income sources
#household_shock_recover_activities.10=="1"~ "1", #Switched from chemical to organic farming
#household_shock_recover_activities.11=="1"~ "1", #Switched from organic to chemical farming
# TRUE~"0"))%>%
#Transformative strategies to cope with shock
#mutate(household_shock_recover_activities.4 = NA)%>%
#mutate(household_shock_strategy_transformative= case_when(
# household_shock_recover_activities.1=="1"~ "1", #Accessed insurance or risk management mechanisms
#household_shock_recover_activities.4=="1"~ "1", #Migrated
#household_shock_recover_activities.8=="1"~ "1", #Relied on institutional support
#TRUE~"0"))%>%
#Number of activities to cope with shocks
mutate(across(starts_with("household_shock_recover_activities."), ~ as.numeric(as.character(.))))%>%
  mutate(household_shock_strategy_count = rowSums(as.matrix(select(., starts_with("household_shock_recover_activities."))), na.rm = TRUE))%>%
  mutate(household_shock_strategy_count=case_when(household_shock_recover_activities.none==1~ household_shock_strategy_count-1,TRUE~household_shock_strategy_count))%>%
  #Household applied shock coping strategy
  mutate(household_shock_strategy= case_when(household_shock_strategy_count>0~ "1", TRUE~"0")) 

zwe_data_clean$nearest_distance_dfs_km
zwe_data_clean$yield_gap_median

write.csv(zwe_data_clean,"zwe_data_clean.csv",row.names=FALSE)

y<-zwe_data_clean%>%
  select(livestock_count_tlu,
perceived_shock_climate,numHA_nhlabour_permanent_total,land_tenure_own_proportion,income_spend_on_food_percentage,"farmer_agency_1", "farmer_agency_3",
         sales_channel_crops.direct_to_consumer,soil_MO_percentage_mean,
         starts_with("sales_channel_crops.")
  )


## to check: controlar si hay otras preguntas donde se citen dfs, ver las practicas de livestock


