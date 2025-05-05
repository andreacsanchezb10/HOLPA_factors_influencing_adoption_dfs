# installation

library(cluster)  # For clustering
library(factoextra)  # For visualization
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(dplyr)
library(purrr)

# Define the function to process each sheet
process_survey_data <- function(sheet_name, country_name, column_id_rename) {
  survey_data <- read_excel(paste0(per_path,"per_holpa_household_survey_clean.xlsx"), sheet = sheet_name)
  
  # Apply transformations
  survey_data <- survey_data %>%
    mutate(country = country_name,
           sheet_id = sheet_name) %>%
    rename("kobo_farmer_id" := !!sym(column_id_rename)) %>%
    slice(-1)
  
  return(survey_data)
}


nhlabour_begin_group <- function(data,workers,hours,permanent_seasonal) {
  data%>%
    mutate(num_workers = str_extract(workers, "^\\d+"),
           group_workers= str_replace(workers, "^\\d+\\s+", ""),
           group_workers= as.character(group_workers),
           num_workers= as.numeric(num_workers),
           num_hours= as.numeric(hours))%>%
    mutate(group_workers= case_when(
      group_workers %in% c("adultos varones mayores (>65 años)","adultos mayores varones (>65 años)")~ paste0("nhlabour_",permanent_seasonal,"_adults_old_male"),
      group_workers%in%c("Adultos varones (≥18 y ≤65 años)","adultos varones (≥18 y ≤65)")~paste0("nhlabour_",permanent_seasonal,"_adults_wa_male"),
      group_workers%in%c("Mujeres adultas (≥18 y ≤65 años)","mujeres adultas (≥18 y ≤65)")~paste0("nhlabour_",permanent_seasonal,"_adults_wa_female"),
      group_workers=="mujeres adultas mayores (>65 años)"~paste0("nhlabour_",permanent_seasonal,"_adults_old_female"),
      group_workers=="niñas (<18 años)"~ paste0("nhlabour_",permanent_seasonal,"_children_female"),
      group_workers=="niños varones (<18 años)"~paste0("nhlabour_",permanent_seasonal,"_children_male"),
      TRUE ~ group_workers))
}


hlabour_begin_group <- function(data,group_workers,num_workers,hours, permanent_seasonal) {
  data%>%
    mutate(group_workers= as.character(group_workers),
           num_workers= as.numeric(num_workers),
           num_hours= as.numeric(hours))%>%
    mutate(group_workers= case_when(
      group_workers %in% c("adultos varones mayores (>65 años)","adultos mayores varones (>65 años)")~ paste0("hlabour_",permanent_seasonal,"_adults_old_male"),
      group_workers%in%c("Adultos varones (≥18 y ≤65 años)","adultos varones (≥18 y ≤65)")~paste0("hlabour_",permanent_seasonal,"_adults_wa_male"),
      group_workers%in%c("Mujeres adultas (≥18 y ≤65 años)","mujeres adultas (≥18 y ≤65)")~paste0("hlabour_",permanent_seasonal,"_adults_wa_female"),
      group_workers=="mujeres adultas mayores (>65 años)"~paste0("hlabour_",permanent_seasonal,"_adults_old_female"),
      group_workers=="niñas (<18 años)"~ paste0("hlabour_",permanent_seasonal,"_children_female"),
      group_workers=="niños varones (<18 años)"~paste0("hlabour_",permanent_seasonal,"_children_male"),
      
      TRUE ~ group_workers))
    
}

practices_begin_group <- function(data, cropland_practices,cropland_practices_area) {
  data%>%
    mutate(cropland_practices= case_when(
      #simplified farming practices
      cropland_practices %in%c("Monoculture with perennial crops")~ "sfs_monoculture_perennial_area",
      cropland_practices %in%c("Monoculture with annual crops")~ "sfs_monoculture_annual_area",
      cropland_practices %in%c("Land clearing for agriculture")~ "sfs_land_clearing_area",
      cropland_practices %in%c("Burning crop residues")~ "sfs_burning_residues_area",
      #diversified farming practices
      cropland_practices %in%c("Crop rotation")~ "dfs_crop_rotation_area",
      cropland_practices %in%c("Agroforestry")~ "dfs_agroforestry_area",
      cropland_practices %in%c("Cover crops")~ "dfs_cover_crops_area",
      cropland_practices %in%c("Homegarden")~ "dfs_homegarden_area",
      cropland_practices %in%c("Intercropping")~ "dfs_intercropping_area",
      cropland_practices %in%c("Fallow (leave land unproductive)" )~ "dfs_fallow_area",
      cropland_practices %in%c("Natural strips/vegetation" )~ "dfs_strip_vegetation_area",
      cropland_practices %in%c("Hedgerows/Live fences")~ "dfs_hedgerows_area",
      cropland_practices %in%c("Pollinator/Flower_strips")~ "dfs_strip_polinator_area",
      #good agricultural practices
      cropland_practices %in%c("Mulching")~ "ecol_practices_mulching_area",
      cropland_practices %in%c("Push-pull")~ "ecol_practices_pushpull_area",
      TRUE ~ cropland_practices))%>%
    select(kobo_farmer_id,cropland_practices,cropland_practices_area)%>%
    pivot_wider(id_cols=kobo_farmer_id, names_from = cropland_practices, values_from = cropland_practices_area,values_fill = "0")
}


#######################################################################################################################################################################################    
########## DATA LOADING #####----
#######################################################################################################################################################################################    

##### GLOBAL ----
# Read the Excel file
factors_list <- read_excel("factors_list.xlsx")

h_global_survey <- read_excel("factors_list.xlsx",sheet = "holpa_survey")%>%
  select("type", "name","label::English ((en))","column_name_new")%>% #select only the necessary columns
  #rename columns names
  rename("label_question" = "label::English ((en))")%>%
  rename("name_question" = "name")%>%
  #remove rows without questions
  filter(!type%in%c("begin_group","begin_repeat","end_repeat","end_group","start","end","audio"))%>%
  #separate question type components
  mutate(type_question = ifelse(substr(type,1,10)=="select_one","select_one",
                                ifelse(substr(type,1,10)=="select_mul","select_multiple",type)))%>%
  #create column with list_name codes matching the choices worksheet
  mutate(list_name = if_else(type_question== "select_one"|type_question== "select_multiple", 
                             str_replace(.$type, paste0(".*", .$type_question), ""),NA))%>%
  mutate(list_name = str_replace_all(list_name, " ", ""))%>%
  filter(!is.na(column_name_new))

write.csv(h_global_survey,"h_global_survey.csv",row.names=FALSE)

global_choices <- read_excel("factors_list.xlsx",sheet = "holpa_choices")%>%
  select("list_name","name","label::English ((en))","name_new")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  mutate(country="global",
         name_new=as.character(name_new))
         #name_choice= if_else(is.na(name_new),name_choice,name_new))
mutate(name_new= case_when(
  name_choice %in% c("notsure","9999")~"unknown",
  list_name =="3_4_4_1"& name_choice=="7"~"unknown",
  TRUE~name_new ) )
write.csv(global_choices,"global_choices.csv",row.names=FALSE)


f_global_survey <- read_excel("factors_list.xlsx",sheet = "fieldwork_survey")%>%
  select("type", "name","label::English ((en))","column_name_new")%>% #select only the necessary columns
  #rename columns names
  rename("label_question" = "label::English ((en))")%>%
  rename("name_question" = "name")%>%
  #remove rows without questions
  filter(!type%in%c("begin_group","begin_repeat","end_repeat","end_group","start","end","audio"))%>%
  #separate question type components
  mutate(type_question = ifelse(substr(type,1,10)=="select_one","select_one",
                                ifelse(substr(type,1,10)=="select_mul","select_multiple",type)))%>%
  #create column with list_name codes matching the choices worksheet
  mutate(list_name = if_else(type_question== "select_one"|type_question== "select_multiple", 
                             str_replace(.$type, paste0(".*", .$type_question), ""),NA))%>%
  mutate(list_name = str_replace_all(list_name, " ", ""))%>%
  filter(!is.na(column_name_new))


##### Peru ----
# Define file path
per_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Peru/peru_data_clean/" #household form

per_h_choices <- read_excel(paste0(per_path,"per_holpa_household_form_clean.xlsx"), sheet = "choices")%>%
  mutate(country= "peru",
         name_new=NA)%>%
  select("list_name","name","label::English (en)","country",name_new)%>%
  rename("label_choice" = "label::English (en)")%>%
  rename("name_choice" = "name")%>%
  distinct(list_name,name_choice,label_choice,name_new, .keep_all = TRUE)

#Add country choices to global choices
per_global_choices<-global_choices%>%
  rbind(per_h_choices)%>%
  arrange(desc(country == "global")) %>%
  #Removing duplicates
  distinct(list_name,name_choice, .keep_all = TRUE) %>%
  right_join(h_global_survey,by="list_name",relationship="many-to-many")%>%
  mutate(label_choice.country=NA)%>%
  dplyr::bind_rows(data.frame(
    list_name= c(rep("3_3_1_2",8)),
    name_choice= c(rep(c("high","medium","low","none"),2)),
    label_choice= c(rep(c("High: five or more species with different heights, woodiness or flowering seasons.","Medium: two to four species.","Low: only one species.","None"),2)),
    country= c(rep("peru",8)),
    type= c(rep("select_one 3_3_1_2",8)),
    type_question=c(rep("select_one",8)),
    name_question= c(rep("_3_3_1_2_10",4),rep("_3_3_1_2_11",4)),
    label_question = c("How would you describe the plant diversity (i.e., number of plant species) in: Young fallow (less than 10 years)",
                       "How would you describe the plant diversity (i.e., number of plant species) in: Old fallow (more or equal than 10 years)"),
    label_choice.country= c(rep(NA,8)),
    stringsAsFactors = FALSE))%>%
  mutate(label_choice = case_when(
    type =="select_one 1_2_1_12_1"& name_choice %in%c(1,2)~"Primary",
    type =="select_one 1_2_1_12_1"& name_choice %in%c(3,4)~"Seconday",
    type =="select_one 1_2_1_12_1"& name_choice %in%c(5:10)~"Higher",
    type =="select_one 1_2_1_12_1"& name_choice==11~"None",
    TRUE ~ label_choice))

write.csv(per_global_choices,"per_global_choices.csv",row.names=FALSE)

#Fieldwork survey
f_column_mapping <- f_global_survey %>%
  select(name_question,column_name_new )  # Select only the relevant columns
print(f_column_mapping)

per_f_survey<- read.csv(paste0(per_path,"per/per_fieldwork_format.csv"))
colnames(per_f_survey) <- gsub("^X", "", colnames(per_f_survey))
per_f_survey<-per_f_survey%>%
  select(all_of(unique(f_column_mapping$name_question)),kobo_farmer_id,country)

#Soil results
per_soil_results <- read_excel(paste0(per_path,"per_soil_results_250924.xlsx"), sheet = "Resultados suelos")%>%
  rename("soil_main_crop"="NUEVA CLASIFICACION",
         "kobo_farmer_id_fieldwork"="Código del \r\nencuestado" ,
         "MO_percentage"="MO %",
         "sand_percentage"="% Arena",
         "clay_percentage"="% Arciclla",
         "silt_percentage"=  "% Limo",
         "soil_class_tx"="Clase Tx")%>%
  filter(!is.na(kobo_farmer_id_fieldwork))%>%
  mutate(soil_class_tx=case_when(
    soil_class_tx=="Arcillo Arenoso"~ "Clayey Sandy",
    soil_class_tx=="Arcillo Limoso"~ "Clayey Silty",
    soil_class_tx=="Arcilloso"~ "Clayey",
    soil_class_tx=="Arena"~ "Sandy",
    soil_class_tx%in% c("Arena Franca","Arena Franco","Franco Arenosa","Franco Arenoso")~ "Sandy Loam",
    soil_class_tx%in%c("Fraco", "Franco")~ "Loam",
    soil_class_tx=="Franco Arcillo Arenoso"~ "Clayey Sandy Loam",
    soil_class_tx=="Franco Arcillo Limoso"~ "Silty Clay Loam",
    soil_class_tx%in%c("Franco arcilloso","Franco Arcilloso")~ "Clay Loam",
    soil_class_tx=="Franco Limoso"~ "Silty Loam",
    TRUE~ NA)) %>%
  group_by(kobo_farmer_id_fieldwork)%>%
  mutate(soil_pH_mean= mean(pH),
         soil_MO_percentage_mean= mean(MO_percentage))%>%
  ungroup()%>%
  distinct(kobo_farmer_id_fieldwork, soil_pH_mean,soil_MO_percentage_mean, .keep_all = TRUE) %>%
  select(kobo_farmer_id_fieldwork, soil_pH_mean,soil_MO_percentage_mean)%>%
  left_join(read_excel(paste0(per_path,"farmer_id_houshold_fieldwork.xlsx"),sheet = "Cod campo_hogar")%>%
              select("id_CAMPO", "id_HOGAR"),by=c("kobo_farmer_id_fieldwork"="id_CAMPO"))%>%
  rename("kobo_farmer_id"="id_HOGAR")%>%select(-kobo_farmer_id_fieldwork)
  
# Read all sheet names from per_household survey
sheet_names <- excel_sheets(paste0(per_path,"per_holpa_household_survey_clean.xlsx"))
sheet_names

# Define country name and column to rename (adjust accordingly)
country_name <- "peru"  # Replace with actual country name
column_id_rename <- "hid"  # Adjust to your specific column

# Process all sheets and create separate data frames in the environment
walk(sheet_names, function(sheet) {
  df_name <- paste0("per", sheet)  # Create dynamic name
  assign(df_name, process_survey_data(sheet, country_name, column_id_rename), envir = .GlobalEnv)
})


per_maintable <- permaintable
per_maintable$end_time <- as.numeric(per_maintable$end_time)
per_maintable$end_time <- as.Date(per_maintable$end_time, origin = "1899-12-30")

per_3_4_1_1_7_1_begin_repeat<-per_3_4_1_1_7_1_begin_repeat%>%
  rename("workers"="_3_4_1_1_7_1_calculate",
         "num_hours"="_3_4_1_1_7_1_1")%>%
  nhlabour_begin_group(.,.$workers,.$num_hours ,"permanent")%>%
  select(kobo_farmer_id,group_workers,num_workers,num_hours)%>%
  mutate(labour_hours= num_workers*num_hours)%>% #total number of workers * average number of hours per day
  pivot_wider(id_cols=kobo_farmer_id, names_from = group_workers, values_from = c(num_workers,num_hours,labour_hours),values_fill = 0)

per_3_4_1_2_1_1_begin_repeat<-per_3_4_1_2_1_1_begin_repeat%>%
  rename("group_workers"="_3_4_1_2_1_1_calculate",
         "num_workers"= "_3_4_1_2_1_1_1",
         "num_hours"="_3_4_1_2_1_1_2")%>%
  hlabour_begin_group(.,.$group_workers,.$num_workers,.$num_hours ,"permanent")%>%
  select(kobo_farmer_id,group_workers,num_workers,num_hours)%>%
  mutate(labour_hours= num_workers*num_hours)%>% #total number of workers * average number of hours per day
  pivot_wider(id_cols=kobo_farmer_id, names_from = group_workers, values_from = c(num_workers,num_hours,labour_hours),values_fill = 0)

per_3_4_1_1_7_2_begin_repeat<-per_3_4_1_1_7_2_begin_repeat%>%
  rename("workers"="_3_4_1_1_7_2_calculate",
         "num_seasons"="_3_4_1_1_7_2_1")%>%
  left_join(per_3_4_1_2_7_2_1_begin_repeat,by=c("kobo_farmer_id","_3_4_1_1_7_2_begin_repeat_rowid","country"))%>%
  rename("num_workers_season"="_3_4_1_2_7_2_3",
         "num_hours"="_3_4_1_2_7_2_4",
         "months"="_3_4_1_2_7_2_2")%>%
  select(kobo_farmer_id,workers,num_seasons,num_workers_season,num_hours,months)%>%
  mutate(month_count = str_count(months, ",") + 1,
         num_hours=as.numeric(num_hours),
         num_workers_season=as.numeric(num_workers_season),
         across(everything(), ~ ifelse(. == 9999, 0, .)),
         total_labour_hours= num_hours*num_workers_season*month_count*21)%>%
  group_by(kobo_farmer_id,workers)%>%
  mutate(total_labour_hours=sum(total_labour_hours))%>%
  ungroup()%>%
  distinct(kobo_farmer_id, workers,total_labour_hours, .keep_all = TRUE)%>%
  nhlabour_begin_group(.,.$workers,.$num_hours,"seasonal")%>%
  pivot_wider(id_cols=kobo_farmer_id, names_from = group_workers, values_from = c(num_workers,total_labour_hours),values_fill = 0)
  
per_3_4_1_2_1_2_begin_repeat<-per_3_4_1_2_1_2_begin_repeat%>%
  rename("group_workers"="_3_4_1_2_1_2_calculate",
          "num_seasons"= "_3_4_1_2_1_2_1")%>%
  mutate(group_workers= case_when(
    group_workers %in% c("adultos varones mayores (>65 años)","adultos mayores varones (>65 años)")~ paste0("hlabour_seasonal_adults_old_male"),
    group_workers%in%c("Adultos varones (≥18 y ≤65 años)","adultos varones (≥18 y ≤65)")~paste0("hlabour_seasonal_adults_wa_male"),
    group_workers%in%c("Mujeres adultas (≥18 y ≤65 años)","mujeres adultas (≥18 y ≤65)")~paste0("hlabour_seasonal_adults_wa_female"),
    group_workers=="mujeres adultas mayores (>65 años)"~paste0("hlabour_seasonal_adults_old_female"),
    group_workers=="niñas (<18 años)"~ paste0("hlabour_seasonal_children_female"),
    group_workers=="niños varones (<18 años)"~paste0("hlabour_seasonal_children_male"),
    TRUE ~ group_workers))%>%
  left_join(per_3_4_1_2_1_2_1_begin_repeat,by=c("kobo_farmer_id","_3_4_1_2_1_2_begin_repeat_rowid","country"))%>%
  rename("num_workers"="_3_4_1_2_1_2_1_3",
         "months"="_3_4_1_2_1_2_1_2",
         "num_hours"="_3_4_1_2_1_2_1_4")%>%
  select(kobo_farmer_id,group_workers,num_seasons,num_workers, months,num_hours)%>%
  mutate(month_count = str_count(months, ",") + 1,
         num_hours=as.numeric(num_hours),
         num_workers=as.numeric(num_workers),
         across(everything(), ~ ifelse(. == 9999, 0, .)),
         total_labour_hours= num_hours*num_workers*month_count*21)%>% #~21working days per month
  group_by(kobo_farmer_id,group_workers)%>%
  mutate(num_workers= sum(as.numeric(num_workers)),
         total_labour_hours=sum(total_labour_hours))%>%
  ungroup()%>%
  distinct(kobo_farmer_id, group_workers,num_workers,total_labour_hours, .keep_all = TRUE)%>%
  pivot_wider(id_cols=kobo_farmer_id, names_from = group_workers, values_from = c(num_workers,total_labour_hours),values_fill = 0)

per_3_3_3_2_begin_repeat<-per_3_3_3_2_begin_repeat%>%
  rename("cropland_practices"="_3_3_3_1_calculate_2",
         "cropland_practices_area"="_3_3_3_2_2")%>%
  mutate(cropland_practices = str_extract(cropland_practices, "(?<=//).*"))%>%
  practices_begin_group(.,.$cropland_practices,.$cropland_practices_area)%>%
  mutate(sfs_burning_residues_area=0)

per_3_4_2_2_2_begin_repeat<-per_3_4_2_2_2_begin_repeat%>%
  rename("livestock_name_main"="_3_4_2_2_2_calculate",
         "livestock_main_breed_number"="_3_4_2_2_3",
         "livestock_main_animal_number"="_3_4_2_2_4")%>%
  mutate(livestock_name_main=  str_extract(livestock_name_main, "(?<=//).*"),
         livestock_main_breed_number = as.numeric(livestock_main_breed_number), 
         livestock_main_animal_number = as.numeric(livestock_main_animal_number)) %>%
  select(kobo_farmer_id,livestock_name_main,livestock_main_breed_number,livestock_main_animal_number)%>%
  distinct(kobo_farmer_id, livestock_name_main, .keep_all = TRUE) %>% 
  pivot_wider(id_cols=kobo_farmer_id, 
              names_from = livestock_name_main, 
              values_from = c(livestock_main_breed_number,livestock_main_animal_number),
              values_fill = list(livestock_main_breed_number = 0, livestock_main_animal_number = 0))

per_3_3_4_1_3_begin_repeat<- per_3_3_4_1_3_begin_repeat%>%
  distinct(kobo_farmer_id, "_3_3_4_1_3_2", .keep_all = TRUE) 
 
##TO CHECK NEED TO UPDATE IN CASE THE DATA WAS UPDATED
per_post_processing<- read.csv("HOLPA data post-processing_PER_inputs.csv")%>%
  rename("main_crops"="crops_names",
         "production_unit"="production.unit")%>%
  select(main_crops,production_unit,yield_conversion_unit_to_kg,yield_ref_rainfed_clean)%>%
  mutate(production_unit = ifelse(str_detect(production_unit, "//"),
                                       str_extract(production_unit, "(?<=//).*"),production_unit))%>%
  mutate(main_crops = str_replace(main_crops, "^(.)", ~str_to_upper(.x)))

area_per_3_4_3_1_2_begin_repeat<-per_3_4_3_1_2_begin_repeat%>%
  rename("main_crops_cropland_area"="_3_4_2_1_3",
         "main_crops"="_3_4_3_1_3_calculate",
         "production_unit"= "_3_4_2_1_5_1_calculate",
         "main_crops_yield"= "_3_4_2_1_5_2")%>%
  mutate(main_crops = str_extract(main_crops, "(?<=//).*"))%>%
  mutate(production_unit = ifelse(str_detect(production_unit, "//"),
                                       str_extract(production_unit, "(?<=//).*"),production_unit))%>%
  mutate(main_crops_cropland_area = as.numeric(main_crops_cropland_area),
         main_crops_yield = as.numeric(main_crops_yield))%>%
  mutate(main_crops = str_replace(main_crops, "^(.)", ~str_to_upper(.x)))%>%
  left_join(per_post_processing,by=c("main_crops","production_unit"))%>%
  mutate(main_crops_perennial= case_when(
    main_crops%in% c("Aguaje","Avocado","Banana","Caimito","Camu camu","Cassava","Cocoa" ,"Cocona","Coconut","Humari","Lemon",
                     "Papaya","Oil palm","Orange","Pijuayo","Pineapple",  "Pink grapefruit","Tangerine" )~ 1,TRUE~0))%>%
  mutate(main_crops_annual= case_when(
    main_crops%in% c("Bean","Black eye bean","Chili pepper", "Coriander","Cucumber","Maize","Melon", "Rice","Sachapapa","Watermelon"  )~ 1,TRUE~0))%>%
    mutate(main_crops_tree= case_when(
      main_crops%in% c("Aguaje","Avocado","Caimito","Camu camu","Cocoa","Coconut", "Humari","Lemon","Papaya","Oil palm","Orange","Pijuayo","Pink grapefruit","Tangerine")~ 1,TRUE~0))%>%
    mutate(main_crops_shrub= case_when(main_crops%in% c("Camu camu", "Cassava" , "Cocona")~ 1,TRUE~0))%>%
    mutate(main_crops_herb= case_when(
      main_crops%in% c("Banana" ,"Bean" ,"Black eye bean","chili pepper","Coriander","Cucumber","Maize","Melon","Pineapple","Rice","Sachapapa","Watermelon")~ 1,TRUE~0))%>%
  mutate(production_kg_ha = (main_crops_yield*yield_conversion_unit_to_kg)/main_crops_cropland_area)%>%
  mutate(yield_ref_rainfed_clean = as.numeric(yield_ref_rainfed_clean)) %>%
  mutate(yield_to_ref_ratio = production_kg_ha/yield_ref_rainfed_clean) %>%
  mutate(yield_gap = ifelse(yield_to_ref_ratio>1 , 0, 
                            ifelse(yield_to_ref_ratio == 0, NA, (1- yield_to_ref_ratio)*100))) %>%
  #Area of three main crops grown
  group_by(kobo_farmer_id)%>%
  mutate(total_main_crops_cropland_area= sum(main_crops_cropland_area),
         num_main_crops_perennial=sum(main_crops_perennial),
         num_main_crops_annual=sum(main_crops_annual),
         num_main_crops_tree=sum(main_crops_tree),
         num_main_crops_shrub=sum(main_crops_shrub),
         num_main_crops_herb= sum(main_crops_herb),
         yield_gap_median = ifelse(all(is.na(yield_gap)), NA, median(yield_gap, na.rm = TRUE)))%>%
  ungroup()%>%
  select(kobo_farmer_id, total_main_crops_cropland_area,num_main_crops_perennial,num_main_crops_annual,
         num_main_crops_tree,num_main_crops_shrub,num_main_crops_herb,yield_gap_median)%>%
  distinct(kobo_farmer_id, total_main_crops_cropland_area,num_main_crops_perennial,num_main_crops_annual,
           num_main_crops_tree,num_main_crops_shrub,num_main_crops_herb,yield_gap_median, .keep_all = TRUE)%>%
  mutate(main_crops_perennial=ifelse(num_main_crops_perennial>0,"1","0"))%>%
  mutate(main_crops_annual=ifelse(num_main_crops_annual>0,"1","0"))%>%
  mutate(main_crops_tree=ifelse(num_main_crops_tree>0,"1","0"))%>%
  mutate(main_crops_shrub=ifelse(num_main_crops_shrub>0,"1","0"))%>%
  mutate(main_crops_herb=ifelse(num_main_crops_herb>0,"1","0"))
  
  
[21]       "per_3_4_3_1_2_begin_repeat"    
[26]      "per_3_4_2_2_6_begin_repeat"     "per_3_4_2_3_2_begin_repeat"     "per_3_4_2_3_2_4_begin_repeat"          
[36]                        "per_3_4_3_1_1_Corregido"        "per_3_4_3_4_2_begin_repeat"    
[41] "per_3_4_3_3_1_1_Corregido"  


per_data<- per_maintable%>%
  left_join(per_1_2_1_4_begin_group,by=c("kobo_farmer_id","country"))%>%
  left_join(per_1_2_1_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_1_2_1_16_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_1_3_1_1_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_2_1_1_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_2_2_1_begin_group,by=c("kobo_farmer_id","country"))%>%
  left_join(per_1_4_2_begin_group,by=c("kobo_farmer_id","country"))%>%
  left_join(per_3_1_1_begin_group,by=c("kobo_farmer_id","country"))%>%
  left_join(per_3_1_2_begin_group,by=c("kobo_farmer_id" ,"country"))%>%
  left_join(per_3_1_3_begin_group,by=c("kobo_farmer_id" ,"country"))%>%
  left_join(per_1_4_1_begin_group,by=c("kobo_farmer_id","country"))%>%
  left_join(per_3_4_1_1_7_1_begin_repeat, by=c("kobo_farmer_id"))%>%
  left_join(per_3_4_1_1_7_2_begin_repeat, by=c("kobo_farmer_id"))%>%
  left_join(per_3_4_1_2_1_1_begin_repeat, by=c("kobo_farmer_id"))%>%
  left_join(per_3_4_1_2_1_2_begin_repeat, by=c("kobo_farmer_id"))%>%
  left_join(per_4_1_4_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_4_1_3_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_4_1_1_5_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_4_1_1_7_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_3_3_3_2_begin_repeat, by=c("kobo_farmer_id"))%>%
  left_join(per_4_1_7_1_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_4_2_1_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_3_3_4_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_2_8_4_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_3_3_1_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_2_12_1_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_2_3_1_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_3_2_1_3_1_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_2_4_1_begin_group, by=c("kobo_farmer_id","country"))%>%
  left_join(per_f_survey, by=c("kobo_farmer_id","country"))%>%
  left_join(per_3_4_2_2_2_begin_repeat, by=c("kobo_farmer_id"))%>%
  left_join(per_soil_results, by=c("kobo_farmer_id"))%>%
  left_join(per_3_3_4_1_3_begin_repeat, by=c("kobo_farmer_id","country"))%>%
  left_join(area_per_3_4_3_1_2_begin_repeat, by=c("kobo_farmer_id"))
  select(kobo_farmer_id,"_1_4_3_5")


  
# Process all select_multiple columns
select_multiple<-h_global_survey%>%
  filter(type_question=="select_multiple")

per_select_multiple_cols <- intersect(colnames(per_data), unique(select_multiple$name_question))
per_select_multiple_cols

per_select_multiple <- per_data %>%
  select(kobo_farmer_id,all_of(per_select_multiple_cols)) %>%
  pivot_longer(cols =-kobo_farmer_id, names_to = "name_question", values_to = "name_choice")%>%   # Reshape to long format
  separate_rows(name_choice, sep = ",") %>%  # Split multiple responses into separate rows
  left_join(h_global_survey%>%select(name_question, column_name_new),by="name_question")%>%
  mutate(value = 1) %>% # Assign 1 for presence
  mutate(column_name_new= if_else(is.na(column_name_new),name_question,column_name_new))%>%
  pivot_wider(id_cols=kobo_farmer_id,names_from = c(column_name_new, name_choice), values_from = value,
              names_sep = "/", values_fill = 0) %>%
  dplyr::select(-matches("/NA$"))

# add select_multiple responses
per_data<-per_data%>%
  select(!all_of(per_select_multiple_cols)) 

per_data<-per_data%>%
  left_join(per_select_multiple, by=c("kobo_farmer_id"))


#####################################
########## RENAME COLUMN NAMES ----
#####################################
# Ensure old column names exist in per_data
h_column_mapping <- h_global_survey %>%
  select(name_question,column_name_new )  # Select only the relevant columns
print(h_column_mapping)

column_mapping<- rbind(h_column_mapping, f_column_mapping)
print(column_mapping)

existing_cols <- colnames(per_data)
print(existing_cols)

# Create a named vector for renaming
rename_vector <- setNames(column_mapping$column_name_new, column_mapping$name_question)
rename_vector
# Rename only matching columns
colnames(per_data) <- ifelse(existing_cols %in% names(rename_vector), rename_vector[existing_cols], existing_cols)

# Check the updated column names
print(colnames(per_data))

#####################################
########## DATA SELECTION ----
per_data<-per_data %>% 
  select(-matches("-desc$"),
         -matches("^_"),
         -matches("^sheet_id"))
print(colnames(per_data))

#####################################
########## DATA CLEANNING -----
#####################################
### The 9999 in ethnicity refers to mestizo ethnicity for Peru
sort(unique(per_data$ethnicity))
per_data$ethnicity <- ifelse(per_data$ethnicity == "9999", "Mestizo", per_data$ethnicity) # convert the 9999 to mestizo
sort(unique(per_data$ethnicity))

### TO CHECK: I need to standardize education level with the other countries options
sort(unique(per_data$education_level))


### To check for inconsistencies between "soil_fertility_management_ecol_practices/5" (which indicates whether mulching was implemented) and 
#"ecol_practices_mulching_area" (which records the area where mulching was applied)
per_data$"soil_fertility_management_ecol_practices/5" <-as.character(per_data$"soil_fertility_management_ecol_practices/5" )

per_data$"soil_fertility_management_ecol_practices/5" <- ifelse(per_data$ecol_practices_mulching_area == "0", "0",
                                      ifelse(per_data$ecol_practices_mulching_area != "0", "1", per_data$"soil_fertility_management_ecol_practices/5"))



### Change name_choice code to numeric codes for: ----
#gender; marital_status
categorical_choices_new_cols <- intersect(colnames(per_data), unique(per_global_choices$column_name_new[!is.na(per_global_choices$name_new)]))
print(categorical_choices_new_cols)

per_data<-per_data
# Replace values dynamically for all common columns
for (col in categorical_choices_new_cols) {
  mapping <- per_global_choices %>%
    filter(!is.na(name_new))%>%
    
    filter(column_name_new == col) %>%
    select(name_choice, name_new) %>%
    { setNames(.$name_new, .$name_choice) }  # Alternative to deframe()
  
  per_data[[col]] <- recode(per_data[[col]], !!!mapping)
}

sort(unique(per_data$marital_status))
sort(unique(per_data$gender))
str(per_data$marital_status)
str(per_data$gender)


names(per_data)
write.csv(per_data,"per_data.csv",row.names=FALSE)

