# installation

library(factoextra)  # For visualization
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(dplyr)
library(purrr)

# Define the function to process each sheet
process_survey_data <- function(sheet_name, country_name) {
  survey_data <- read_excel(paste0(zwe_path,"zwe_holpa_household_survey_clean.xlsx"), sheet = sheet_name)
  
  column_id <- if ("_id" %in% names(survey_data)) {
    "_id"
  } else if ("_submission__id" %in% names(survey_data)) {
    "_submission__id"
  } else {
    stop(paste("No ID column found in sheet:", sheet_name))
  }
  
  # Apply transformations
  survey_data <- survey_data %>%
    mutate(country = country_name,
           sheet_id = sheet_name) %>%
    rename("kobo_farmer_id" := !!sym(column_id)) %>%
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
      group_workers %in% c("Male adults not of working age (>64 years old)")~ paste0("nhlabour_",permanent_seasonal,"_adults_old_male"),
      group_workers%in%c("Male adults (>14 and <65 years old)")~paste0("nhlabour_",permanent_seasonal,"_adults_wa_male"),
      group_workers%in%c("Female adults (>14 and <65 years old)")~paste0("nhlabour_",permanent_seasonal,"_adults_wa_female"),
      group_workers=="Female adults not of working age (>64 years old)"~paste0("nhlabour_",permanent_seasonal,"_adults_old_female"),
      group_workers=="Female children (<15 years old)"~ paste0("nhlabour_",permanent_seasonal,"_children_female"),
      group_workers=="Male children (<15 years old)"~paste0("nhlabour_",permanent_seasonal,"_children_male"),
      TRUE ~ group_workers))
}


hlabour_begin_group <- function(data,group_workers,num_workers,hours, permanent_seasonal) {
  data%>%
    mutate(group_workers= as.character(group_workers),
           num_workers= as.numeric(num_workers),
           num_hours= as.numeric(hours))%>%
    mutate(group_workers= case_when(
      group_workers %in% c("Male adults not of working age (>64 years old)")~ paste0("hlabour_",permanent_seasonal,"_adults_old_male"),
      group_workers%in%c("Male adults (>14 and <65 years old)")~paste0("hlabour_",permanent_seasonal,"_adults_wa_male"),
      group_workers%in%c("Female adults (>14 and <65 years old)")~paste0("hlabour_",permanent_seasonal,"_adults_wa_female"),
      group_workers=="Female adults not of working age (>64 years old)"~paste0("hlabour_",permanent_seasonal,"_adults_old_female"),
      group_workers=="Female children (<15 years old)"~ paste0("hlabour_",permanent_seasonal,"_children_female"),
      group_workers=="Male children (<15 years old)"~paste0("hlabour_",permanent_seasonal,"_children_male"),
      
      TRUE ~ group_workers))
  
}

practices_begin_group <- function(data, cropland_practices,cropland_practices_area) {
  data%>%
    mutate(cropland_practices= case_when(
      #simplified farming practices
      cropland_practices %in%c("Burning crop residues")~ "sfs_burning_residues_area",
      cropland_practices %in%c("Land clearing for agriculture")~ "sfs_land_clearing_area",
      cropland_practices %in%c("Monoculture with annual crops")~ "sfs_monoculture_annual_area",
      cropland_practices %in%c("Monoculture with perennial crops")~ "sfs_monoculture_perennial_area",
      #diversified farming practices
      cropland_practices %in%c("Agroforestry")~ "dfs_agroforestry_area",
      cropland_practices %in%c("Cover crops")~ "dfs_cover_crops_area",
      cropland_practices %in%c("Crop rotation")~ "dfs_crop_rotation_area",
      cropland_practices %in%c("Fallow (leave land unproductive)" )~ "dfs_fallow_area",
      cropland_practices %in%c("Hedgerows/Live fences")~ "dfs_hedgerows_area",
      cropland_practices %in%c("Homegarden")~ "dfs_homegarden_area",
      cropland_practices %in%c("Intercropping")~ "dfs_intercropping_area",
      cropland_practices %in%c("Mulching")~ "ecol_practices_mulching_area",
      cropland_practices %in%c("Natural strips/vegetation" )~ "dfs_strip_vegetation_area",
      cropland_practices %in%c("Pollinator/Flower_strips")~ "dfs_strip_polinator_area",
      #good agricultural practices
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


##### Zimbabwe ----
# Define file path
zwe_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Zimbabwe/zimbabwe_data_clean/" #household form

zwe_h_choices <- read_excel(paste0(zwe_path,"zwe_holpa_household_form_clean.xlsx"), sheet = "choices")%>%
  mutate(country= "zimbabwe",name_new=NA)%>%
  select("list_name","name","label::English ((en))","country",name_new)%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  distinct(list_name,name_choice,label_choice,name_new, .keep_all = TRUE)

#Add country choices to global choices
zwe_global_choices<-global_choices%>%
  rbind(zwe_h_choices)%>%
  arrange(desc(country == "global")) %>%
  #Removing duplicates
  distinct(list_name,name_choice, .keep_all = TRUE) %>%
  right_join(h_global_survey,by="list_name",relationship="many-to-many")%>%
  mutate(label_choice.country=NA)
  dplyr::bind_rows(data.frame(
    list_name= c(rep("3_3_1_2",8)),
    name_choice= c(rep(c("high","medium","low","none"),2)),
    label_choice= c(rep(c("High: five or more species with different heights, woodiness or flowering seasons.","Medium: two to four species.","Low: only one species.","None"),2)),
    country= c(rep("zimbabwe",8)),
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

write.csv(zwe_global_choices,"zwe_global_choices.csv",row.names=FALSE)

#Fieldwork survey
f_column_mapping <- f_global_survey %>%
  select(name_question,column_name_new )  # Select only the relevant columns
print(f_column_mapping)

zwe_f_survey<- read.csv(paste0(zwe_path,"zwe/zwe_fieldwork_format.csv"))
colnames(zwe_f_survey) <- gsub("^X", "", colnames(zwe_f_survey))
zwe_f_survey<-zwe_f_survey%>%
  select(all_of(unique(f_column_mapping$name_question)),kobo_farmer_id,country)%>%
  mutate(kobo_farmer_id=as.character(kobo_farmer_id))

#Soil results 
## TO CHECK!!
zwe_soil_results <- read_excel(paste0(zwe_path,"zwe_soil_results_250924.xlsx"), sheet = "Resultados suelos")

# Read all sheet names from zwe_household survey
sheet_names <- excel_sheets(paste0(zwe_path,"zwe_holpa_household_survey_clean.xlsx"))
sheet_names

# Define country name and column to rename (adjust accordingly)
country_name <- "zimbabwe"  # Replace with actual country name
column_id_rename <- "_id"  # Adjust to your specific column

# Process all sheets and create separate data frames in the environment
walk(sheet_names, function(sheet) {
  df_name <- paste0("zwe", sheet)  # Create dynamic name
  assign(df_name, process_survey_data(sheet, country_name), envir = .GlobalEnv)
})

zwe_maintable <- `zweFinal HOLPA_Zimbabwe_Household`
zwe_maintable$end <- as.numeric(zwe_maintable$end)
zwe_maintable$end <- as.Date(zwe_maintable$end, origin = "1899-12-30")

zwe_3_4_1_1_7_1_begin_repeat<-zwe_3_4_1_1_7_1_begin_repeat%>%
  rename("workers"="_3_4_1_1_7_1_calculate",
         "num_hours"="_3_4_1_1_7_1_1")%>%
  nhlabour_begin_group(.,.$workers,.$num_hours ,"permanent")%>%
  select(kobo_farmer_id,group_workers,num_workers,num_hours)%>%
  mutate(labour_hours= num_workers*num_hours)%>% #total number of workers * average number of hours per day
  pivot_wider(id_cols=kobo_farmer_id, names_from = group_workers, values_from = c(num_workers,num_hours,labour_hours),values_fill = 0)

zwe_3_4_1_2_1_1_begin_repeat<-zwe_3_4_1_2_1_1_begin_repeat%>%
  rename("group_workers"="_3_4_1_2_1_1_calculate",
         "num_workers"= "_3_4_1_2_1_1_1",
         "num_hours"="_3_4_1_2_1_1_2")%>%
  hlabour_begin_group(.,.$group_workers,.$num_workers,.$num_hours ,"permanent")%>%
  select(kobo_farmer_id,group_workers,num_workers,num_hours)%>%
  mutate(labour_hours= num_workers*num_hours)%>% #total number of workers * average number of hours per day
  pivot_wider(id_cols=kobo_farmer_id, names_from = group_workers, values_from = c(num_workers,num_hours,labour_hours),values_fill = 0)

zwe_3_4_1_1_7_2_begin_repeat<-zwe_3_4_1_1_7_2_begin_repeat%>%
  rename("workers"="_3_4_1_1_7_2_calculate",
         "num_seasons"="_3_4_1_1_7_2_1")%>%
  left_join(zwe_3_4_1_2_7_2_1_begin_repeat,by=c("kobo_farmer_id","_index","country"))%>%
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

zwe_3_4_1_2_1_2_begin_repeat<-zwe_3_4_1_2_1_2_begin_repeat%>%
  rename("group_workers"="_3_4_1_2_1_2_calculate",
         "num_seasons"= "_3_4_1_2_1_2_1")%>%
  mutate(group_workers= case_when(
    group_workers %in% c("Male or Female adults (>65 years old)")~ paste0("hlabour_seasonal_adults_old"),
    group_workers%in%c("Male adults (≥18 and ≤65 years old)")~paste0("hlabour_seasonal_adults_wa_male"),
    group_workers%in%c("Female adults (≥18 and ≤65 years old)")~paste0("hlabour_seasonal_adults_wa_female"),
    group_workers%in%c("Male or Female children (<18 years old)")~paste0("hlabour_seasonal_children"),
    TRUE ~ group_workers))%>%
  left_join(zwe_3_4_1_2_1_2_1_begin_repeat,by=c("kobo_farmer_id","_index","country"))%>%
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

zwe_3_3_3_2_begin_repeat<-zwe_3_3_3_2_begin_repeat%>%
  rename("cropland_practices"="_3_3_3_1_calculate_2",
         "cropland_practices_area"="_3_3_3_2_2")%>%
  practices_begin_group(.,.$cropland_practices,.$cropland_practices_area)

zwe_3_4_2_2_2_begin_repeat<-zwe_3_4_2_2_2_begin_repeat%>%
  rename("livestock_name_main"="_3_4_2_2_2_calculate",
         "livestock_main_breed_number"="_3_4_2_2_3",
         "livestock_main_animal_number"="_3_4_2_2_4")%>%
  mutate(livestock_main_breed_number = as.numeric(livestock_main_breed_number), 
         livestock_main_animal_number = as.numeric(livestock_main_animal_number)) %>%
  select(kobo_farmer_id,livestock_name_main,livestock_main_breed_number,livestock_main_animal_number)%>%
  distinct(kobo_farmer_id, livestock_name_main, .keep_all = TRUE) %>% 
  pivot_wider(id_cols=kobo_farmer_id, 
              names_from = livestock_name_main, 
              values_from = c(livestock_main_breed_number,livestock_main_animal_number),
              values_fill = list(livestock_main_breed_number = 0, livestock_main_animal_number = 0))

zwe_3_3_4_1_3_begin_repeat<- zwe_3_3_4_1_3_begin_repeat%>%
  distinct(kobo_farmer_id, "_3_3_4_1_3_2", .keep_all = TRUE) 

##TO CHECK NEED TO UPDATE IN CASE THE DATA WAS UPDATED
zwe_post_processing<-read.csv("HOLPA data post-processing_ZWE_inputs.csv")%>%
  rename("main_crops"="main_crops_common_names",
         "production_unit"="production.unit")%>%
  select(main_crops,production_unit,yield_conversion_unit_to_kg,yield_ref_rainfed_clean)
  mutate(main_crops = str_replace(main_crops, "^(.)", ~str_to_upper(.x)))

area_zwe_3_4_3_1_2_begin_repeat<-zwe_3_4_3_1_2_begin_repeat%>%
  rename("main_crops_cropland_area"="_3_4_2_1_3",
         "main_crops"="_3_4_3_1_3_calculate",
         "production_unit"= "_3_4_2_1_5_1_calculate",
         "main_crops_yield"= "_3_4_2_1_5_2")%>%
  mutate(main_crops_cropland_area = as.numeric(main_crops_cropland_area),
         main_crops_yield = as.numeric(main_crops_yield))%>%
  mutate(main_crops = str_replace(main_crops, "^(.)", ~str_to_upper(.x)))%>%
  left_join(zwe_post_processing,by=c("main_crops","production_unit"))%>%
  mutate(main_crops_perennial= case_when(
    main_crops%in% c("Avocado","Banana", "Cotton","Mango" ,"Peaches")~ 1,
    TRUE~0))%>%
  mutate(main_crops_annual= case_when(
    main_crops%in% c("Bambara nuts","Beans","Cabbages","Common bean" ,"Cowpea","Finger millet","Groundnuts","Maize",
                     "Masau","Millet","Onion","Pepper","Potato","Rapoko","Rice", "Rosella","Roundnuts" ,"Sesame","Sorghum",
                     "Soybeans","Sugarbeans", "Sunflower","Sweet potatoes","Tobacco","Tomatoes","Velvet  beans (mucuna)")~ 1,
    TRUE~0))%>%
  mutate(main_crops_tree= case_when(
    main_crops%in% c("Avocado","Mango","Peaches")~ 1,TRUE~0))%>%
  mutate(main_crops_shrub= case_when(main_crops%in% c("Cotton","Masau" )~ 1,TRUE~0))%>%
  mutate(main_crops_herb= case_when(
    main_crops%in% c("Banana","Bambara nuts","Beans","Cabbages","Common bean" ,"Cowpea","Finger millet","Groundnuts","Maize",
                     "Millet","Onion","Pepper","Potato","Rapoko","Rice", "Rosella","Roundnuts" ,"Sesame","Sorghum",
                     "Soybeans","Sugarbeans", "Sunflower","Sweet potatoes","Tobacco","Tomatoes","Velvet  beans (mucuna)")~ 1,TRUE~0))%>%
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

sheet_names
[5]      "_1_4_2_7_begin_repeat"                "_3_4_1_2_7_2_1_begin_repeat"   
[9]           "_3_4_3_1_1_begin_repeat"        "_3_4_2_2_6_begin_repeat"       
[13] "_3_4_3_3_1_1_begin_repeat"              "_1_4_2_1_1_begin_repeat"         
 

zwe_data<- zwe_maintable%>%
  left_join(zwe_3_4_1_1_7_1_begin_repeat, by=c("kobo_farmer_id"))%>%
  left_join(zwe_3_4_1_1_7_2_begin_repeat, by=c("kobo_farmer_id"))%>%
  left_join(zwe_3_4_1_2_1_1_begin_repeat, by=c("kobo_farmer_id"))%>%
  left_join(zwe_3_4_1_2_1_2_begin_repeat, by=c("kobo_farmer_id"))%>%
  left_join(zwe_3_3_3_2_begin_repeat, by=c("kobo_farmer_id"))%>%
  left_join(zwe_f_survey, by=c("kobo_farmer_id","country"))%>%
  left_join(zwe_3_4_2_2_2_begin_repeat, by=c("kobo_farmer_id"))%>%
  #left_join(zwe_soil_results, by=c("kobo_farmer_id"))%>%
  left_join(zwe_3_3_4_1_3_begin_repeat, by=c("kobo_farmer_id","country"))%>%
  left_join(area_zwe_3_4_3_1_2_begin_repeat, by=c("kobo_farmer_id"))


# Process all select_multiple columns
select_multiple<-h_global_survey%>%
  filter(type_question=="select_multiple")

zwe_select_multiple_cols <- intersect(colnames(zwe_data), unique(select_multiple$name_question))
zwe_select_multiple_cols

names(zwe_data)


# add select_multiple responses
zwe_data<-zwe_data%>%
  select(!all_of(zwe_select_multiple_cols)) 

#zwe_data<-zwe_data%>%
 # left_join(zwe_select_multiple, by=c("kobo_farmer_id"))


#####################################
########## RENAME COLUMN NAMES ----
#####################################
# Ensure old column names exist in zwe_data
h_column_mapping <- h_global_survey %>%
  select(name_question,column_name_new )  # Select only the relevant columns
print(h_column_mapping)

column_mapping<- rbind(h_column_mapping, f_column_mapping)
print(column_mapping)

existing_cols <- colnames(zwe_data)
print(existing_cols)

# Create a named vector for renaming
rename_vector <- setNames(column_mapping$column_name_new, column_mapping$name_question)
rename_vector
# Rename only matching columns
colnames(zwe_data) <- ifelse(existing_cols %in% names(rename_vector), rename_vector[existing_cols], existing_cols)

# Create a vector of all prefixes that may appear before the "/"
slash_prefixes <- names(rename_vector)
slash_prefixes


# Function to replace prefix in slash-column names
rename_slash_columns <- function(col_name) {
  matched_prefix <- slash_prefixes[startsWith(col_name, paste0(slash_prefixes, "/"))]
  if (length(matched_prefix) == 1) {
    return(sub(paste0("^", matched_prefix, "/"), paste0(rename_vector[[matched_prefix]], "/"), col_name))
  } else {
    return(col_name)
  }
}

# Apply to all column names
colnames(zwe_data) <- sapply(colnames(zwe_data), rename_slash_columns)


# Check the updated column names
print(colnames(zwe_data))

#####################################
########## DATA SELECTION ----
#zwe_data<-zwe_data %>% 
 # select(-matches("-desc$"),
  #       -matches("^_"),
   #      -matches("^sheet_id"))

#####################################
########## DATA CLEANNING -----
#####################################
### TO CHECK: I need to standardize education level with the other countries options
sort(unique(zwe_data$education_level))

### To check for inconsistencies between "soil_fertility_management_ecol_practices/5" (which indicates whether mulching was implemented) and 
#"ecol_practices_mulching_area" (which records the area where mulching was applied)
zwe_data$"soil_fertility_management_ecol_practices/5" <-as.character(zwe_data$"soil_fertility_management_ecol_practices/5" )

zwe_data$"soil_fertility_management_ecol_practices/5" <- ifelse(zwe_data$ecol_practices_mulching_area == "0", "0",
                                                                ifelse(zwe_data$ecol_practices_mulching_area != "0", "1", zwe_data$"soil_fertility_management_ecol_practices/5"))



### Change name_choice code to numeric codes for: ----
#gender; marital_status
categorical_choices_new_cols <- intersect(colnames(zwe_data), unique(zwe_global_choices$column_name_new[!is.na(zwe_global_choices$name_new)]))
print(categorical_choices_new_cols)

zwe_data<-zwe_data
# Replace values dynamically for all common columns
for (col in categorical_choices_new_cols) {
  mapping <- zwe_global_choices %>%
    filter(!is.na(name_new))%>%
    
    filter(column_name_new == col) %>%
    select(name_choice, name_new) %>%
    { setNames(.$name_new, .$name_choice) }  # Alternative to deframe()
  
  zwe_data[[col]] <- recode(zwe_data[[col]], !!!mapping)
}

sort(unique(zwe_data$marital_status))
sort(unique(zwe_data$gender))
str(zwe_data$marital_status)
str(zwe_data$gender)


#Remove respondents that are not farmers
zwe_data<-zwe_data%>%
  filter(kobo_farmer_id!="274186917")

names(zwe_data)
write.csv(zwe_data,"zwe_data.csv",row.names=FALSE)

