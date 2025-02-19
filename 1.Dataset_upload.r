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
  survey_data <- read_excel(per_survey_path, sheet = sheet_name)
  
  # Apply transformations
  survey_data <- survey_data %>%
    mutate(country = country_name,
           sheet_id = sheet_name) %>%
    rename("kobo_farmer_id" := !!sym(column_id_rename)) %>%
    slice(-1)
  
  return(survey_data)
}

nhlabour_begin_group <- function(data,workers,permanent_seasonal) {
  data%>%
    mutate(n_workers = str_extract(workers, "^\\d+"),
           group_workers= str_replace(workers, "^\\d+\\s+", ""),
           group_workers= as.character(group_workers),
           n_workers= as.numeric(n_workers))%>%
    mutate(group_workers= case_when(
      group_workers %in% c("adultos varones mayores (>65 años)","adultos mayores varones (>65 años)")~ paste0("nhlabour_",permanent_seasonal,"_adults_old_male"),
      group_workers%in%c("Adultos varones (≥18 y ≤65 años)","adultos varones (≥18 y ≤65)")~paste0("nhlabour_",permanent_seasonal,"_adults_wa_male"),
      group_workers%in%c("Mujeres adultas (≥18 y ≤65 años)","mujeres adultas (≥18 y ≤65)")~paste0("nhlabour_",permanent_seasonal,"_adults_wa_female"),
      group_workers=="mujeres adultas mayores (>65 años)"~paste0("nhlabour_",permanent_seasonal,"_adults_old_female"),
      group_workers=="niñas (<18 años)"~ paste0("nhlabour_",permanent_seasonal,"_children_female"),
      group_workers=="niños varones (<18 años)"~paste0("nhlabour_",permanent_seasonal,"_children_male"),
      
      TRUE ~ group_workers))%>%
    select(kobo_farmer_id,n_workers,group_workers)%>%
    pivot_wider(id_cols=kobo_farmer_id, names_from = group_workers, values_from = n_workers,values_fill = 0)
  
}

hlabour_begin_group <- function(data,group_workers,n_workers,permanent_seasonal) {
  data%>%
    mutate(group_workers= as.character(group_workers),
           n_workers= as.numeric(n_workers))%>%
    mutate(group_workers= case_when(
      group_workers %in% c("adultos varones mayores (>65 años)","adultos mayores varones (>65 años)")~ paste0("hlabour_",permanent_seasonal,"_adults_old_male"),
      group_workers%in%c("Adultos varones (≥18 y ≤65 años)","adultos varones (≥18 y ≤65)")~paste0("hlabour_",permanent_seasonal,"_adults_wa_male"),
      group_workers%in%c("Mujeres adultas (≥18 y ≤65 años)","mujeres adultas (≥18 y ≤65)")~paste0("hlabour_",permanent_seasonal,"_adults_wa_female"),
      group_workers=="mujeres adultas mayores (>65 años)"~paste0("hlabour_",permanent_seasonal,"_adults_old_female"),
      group_workers=="niñas (<18 años)"~ paste0("hlabour_",permanent_seasonal,"_children_female"),
      group_workers=="niños varones (<18 años)"~paste0("hlabour_",permanent_seasonal,"_children_male"),
      
      TRUE ~ group_workers))%>%
    select(kobo_farmer_id,n_workers,group_workers)%>%
    pivot_wider(id_cols=kobo_farmer_id, names_from = group_workers, values_from = n_workers,values_fill = 0)
}

practices_begin_group <- function(data, cropland_practices,cropland_practices_area) {
  data%>%
    mutate(cropland_practices= case_when(
      #simplified farming practices
      cropland_practices %in%c("Monoculture with perennial crops")~ "sfs_monoculture_perennial_area",
      cropland_practices %in%c("Monoculture with annual crops")~ "sfs_monoculture_annual_area",
      cropland_practices %in%c("Land clearing for agriculture")~ "sfs_land_clearing_area",
      #diversified farming practices
      cropland_practices %in%c("Crop rotation")~ "dfs_crop_rotation_area",
      cropland_practices %in%c("Agroforestry")~ "dfs_agroforestry_area",
      cropland_practices %in%c("Cover crops")~ "dfs_cover_crops_area",
      cropland_practices %in%c("Homegarden")~ "dfs_homegarden_area",
      cropland_practices %in%c("Intercropping")~ "dfs_intercropping_area",
      cropland_practices %in%c("Fallow (leave land unproductive)" )~ "dfs_fallow_area",
      cropland_practices %in%c("Natural strips/vegetation" )~ "dfs_strip_vegetation_area",
      cropland_practices %in%c("Hedgerows/Live fences")~ "dfs_hedgerows_area",
      
      #good agricultural practices
      cropland_practices %in%c("Mulching")~ "ecol_practices_mulching_area",
      
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

global_survey <- read_excel("factors_list.xlsx",sheet = "holpa_survey")%>%
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

global_choices <- read_excel("factors_list.xlsx",sheet = "holpa_choices")%>%
  select("list_name","name","label::English ((en))","name_new")%>%
  rename("label_choice" = "label::English ((en))")%>%
  rename("name_choice" = "name")%>%
  mutate(country="global",
         name_new=as.character(name_new))
mutate(name_new= case_when(
  name_choice %in% c("notsure","9999")~"unknown",
  list_name =="3_4_4_1"& name_choice=="7"~"unknown",
  TRUE~name_new ) )


##### Peru ----
# Define file path
per_form_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Peru/peru_data_clean/per_holpa_household_form_clean.xlsx" #path andrea
per_survey_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Peru/peru_data_clean/per_holpa_household_survey_clean.xlsx" #path andrea


per_choices <- read_excel(per_form_path, sheet = "choices")%>%
  mutate(country= "peru",
         name_new=NA)%>%
  select("list_name","name","label::English (en)","country",name_new)%>%
  rename("label_choice" = "label::English (en)")%>%
  rename("name_choice" = "name")%>%
  distinct(list_name,name_choice,label_choice,name_new, .keep_all = TRUE)

#Add country choices to global choices
per_global_choices<-global_choices%>%
  rbind(per_choices)%>%
  arrange(desc(country == "global")) %>%
  #Removing duplicates
  distinct(list_name,name_choice, .keep_all = TRUE) %>%
  right_join(global_survey,by="list_name",relationship="many-to-many")%>%
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

# Read all sheet names
sheet_names <- excel_sheets(per_survey_path)
sheet_names


# Define country name and column to rename (adjust accordingly)
country_name <- "peru"  # Replace with actual country name
column_id_rename <- "hid"  # Adjust to your specific column

exists("process_survey_data")


# Process all sheets and create separate data frames in the environment
walk(sheet_names, function(sheet) {
  df_name <- paste0("per", sheet)  # Create dynamic name
  assign(df_name, process_survey_data(sheet, country_name, column_id_rename), envir = .GlobalEnv)
})


per_maintable <- permaintable
per_3_4_1_1_7_1_begin_repeat<-per_3_4_1_1_7_1_begin_repeat%>%
  rename("nonhired_permanent_workers"="_3_4_1_1_7_1_calculate")%>%
  nhlabour_begin_group(.,.$nonhired_permanent_workers,"permanent")

per_3_4_1_1_7_2_begin_repeat<-per_3_4_1_1_7_2_begin_repeat%>%
  rename("nonhired_seasonal_workers"="_3_4_1_1_7_2_calculate")%>%
  nhlabour_begin_group(.,.$nonhired_seasonal_workers,"seasonal")

per_3_4_1_2_1_1_begin_repeat<-per_3_4_1_2_1_1_begin_repeat%>%
  rename("group_workers"="_3_4_1_2_1_1_calculate",
         "n_workers"= "_3_4_1_2_1_1_1")%>%
  hlabour_begin_group(.,.$group_workers,.$n_workers,"permanent")

per_3_4_1_2_1_2_begin_repeat<-per_3_4_1_2_1_2_begin_repeat%>%
  rename("group_workers"="_3_4_1_2_1_2_calculate",
         "n_workers"= "_3_4_1_2_1_2_1")%>%
  hlabour_begin_group(.,.$group_workers,.$n_workers,"seasonal")

per_3_3_3_2_begin_repeat<-per_3_3_3_2_begin_repeat%>%
  rename("cropland_practices"="_3_3_3_1_calculate_2",
         "cropland_practices_area"="_3_3_3_2_2")%>%
  mutate(cropland_practices = str_extract(cropland_practices, "(?<=//).*"))%>%
  practices_begin_group(.,.$cropland_practices,.$cropland_practices_area)


colnames(per_3_3_3_2_begin_repeat)

sheet_names


[11]  "per_3_4_1_2_7_2_1_begin_repeat"
[16] "per_3_4_1_2_1_2_1_begin_repeat" "per_2_4_1_begin_group"          "per_3_2_1_3_1_begin_group"     
[21]       "per_3_4_3_1_2_begin_repeat"    


[26] "per_3_4_2_2_2_begin_repeat"     "per_3_4_2_2_6_begin_repeat"     "per_3_4_2_3_2_begin_repeat"     "per_3_4_2_3_2_4_begin_repeat"          
[31]                     "per_3_3_4_1_3_begin_repeat"             
[36]               "per_2_3_1_begin_group"          "per_3_4_3_1_1_Corregido"        "per_3_4_3_4_2_begin_repeat"    
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
  left_join(per_2_12_1_begin_group, by=c("kobo_farmer_id","country"))

names(per_data)

# Process all select_multiple columns
select_multiple<-global_survey%>%
  filter(type_question=="select_multiple")

per_select_multiple_cols <- intersect(colnames(per_data), unique(select_multiple$name_question))
per_select_multiple_cols


per_select_multiple <- per_data %>%
  select(kobo_farmer_id,all_of(per_select_multiple_cols)) %>%
  pivot_longer(cols =-kobo_farmer_id, names_to = "name_question", values_to = "name_choice")%>%   # Reshape to long format
  separate_rows(name_choice, sep = ",") %>%  # Split multiple responses into separate rows
  left_join(global_survey%>%select(name_question, column_name_new),by="name_question")%>%
  mutate(value = 1) %>% # Assign 1 for presence
  mutate(column_name_new= if_else(is.na(column_name_new),name_question,column_name_new))%>%
  pivot_wider(id_cols=kobo_farmer_id,names_from = c(column_name_new, name_choice), values_from = value,
              names_sep = "/", values_fill = 0) %>%
  dplyr::select(-matches("/NA$"))
names(per_select_multiple)
# add select_multiple responses
per_data<-per_data%>%
  select(!all_of(per_select_multiple_cols)) 

per_data<-per_data%>%
  left_join(per_select_multiple, by=c("kobo_farmer_id"))

names(per_data)
#####################################
########## RENAME COLUMN NAMES ----
#####################################
# Ensure old column names exist in per_data
column_mapping <- global_survey %>%
  select(name_question,column_name_new )  # Select only the relevant columns
print(column_mapping)

existing_cols <- colnames(per_data)

# Create a named vector for renaming
rename_vector <- setNames(column_mapping$column_name_new, column_mapping$name_question)

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
         #None responses
         -"livestock_health_practice/none",
         -"livestock_practices/none")
print(colnames(per_data))

#####################################
########## DATA CLEANNING -----
#####################################
### The 9999 in ethnicity refers to mestizo ethnicity for Peru
sort(unique(per_data$ethnicity))
per_data$ethnicity <- ifelse(per_data$ethnicity == "9999", "mestizo", per_data$ethnicity) # convert the 9999 to mestizo
sort(unique(per_data$ethnicity))

### TO CHECK: I need to standardixe education level with the other countries options
sort(unique(per_data$education_level))


### To check for inconsistencies between "soil_fertility_ecol_practices/5" (which indicates whether mulching was implemented) and 
#"ecol_practices_mulching_area" (which records the area where mulching was applied)
per_data$"soil_fertility_ecol_practices/5" <-as.character(per_data$"soil_fertility_ecol_practices/5" )

per_data$"soil_fertility_ecol_practices/5" <- ifelse(per_data$ecol_practices_mulching_area == "0", "0",
                                      ifelse(per_data$ecol_practices_mulching_area != "0", "1", per_data$"soil_fertility_ecol_practices/5"))


names(per_data)
write.csv(per_data,"per_data.csv",row.names=FALSE)

