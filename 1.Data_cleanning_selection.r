# installation

library(cluster)  # For clustering
library(factoextra)  # For visualization
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(dplyr)
library(purrr)

########## DATA LOADING #####----
# Define the function to process each sheet
process_survey_data <- function(sheet_name, country_name, column_id_rename) {
  survey_data <- read_excel(per_data_path, sheet = sheet_name)
  
  # Apply transformations
  survey_data <- survey_data %>%
    mutate(country = country_name,
           sheet_id = sheet_name) %>%
    rename("kobo_farmer_id" := !!sym(column_id_rename)) %>%
    slice(-1)
  
  return(survey_data)
}

labour_begin_group <- function(data,workers,nh_h_workers_permanent_seasonal) {
  data%>%
    mutate(n_workers = str_extract(workers, "^\\d+"),
           group_workers= str_replace(workers, "^\\d+\\s+", ""),
           group_workers= as.character(group_workers),
           n_workers= as.numeric(n_workers))%>%
    mutate(group_workers= case_when(
      group_workers %in% c("adultos varones mayores (>65 años)","adultos mayores varones (>65 años)")~ paste0(nh_h_workers_permanent_seasonal,"_adults_old_male"),
      group_workers%in%c("Adultos varones (≥18 y ≤65 años)","adultos varones (≥18 y ≤65)")~paste0(nh_h_workers_permanent_seasonal,"_adults_male"),
      group_workers%in%c("Mujeres adultas (≥18 y ≤65 años)","mujeres adultas (≥18 y ≤65)")~paste0(nh_h_workers_permanent_seasonal,"_adults_female"),
      group_workers=="mujeres adultas mayores (>65 años)"~paste0(nh_h_workers_permanent_seasonal,"_adults_old_female"),
      group_workers=="niñas (<18 años)"~ paste0(nh_h_workers_permanent_seasonal,"_children_female"),
      group_workers=="niños varones (<18 años)"~paste0(nh_h_workers_permanent_seasonal,"_children_male"),
      group_workers %in%c("Monoculture with perennial crops")~"Monoculture_perennial",
      
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
      cropland_practices %in%c("Mulching")~ "gfs_mulching_area",
      
      TRUE ~ cropland_practices))%>%
    select(kobo_farmer_id,cropland_practices,cropland_practices_area)%>%
    pivot_wider(id_cols=kobo_farmer_id, names_from = cropland_practices, values_from = cropland_practices_area,values_fill = "0")
}
        


##### ALL ----
# Read the Excel file
factors_list <- read_excel("C:/Users/andreasanchez/OneDrive - CGIAR/3_chapter_PhD/HOLPA_factors_influencing_adoption_dfs/factors_list.xlsx")

factors_list_holpa<-factors_list%>%
  filter(data_source=="holpa")
print(factors_list_holpa)

column_mapping <- factors_list %>%
  select(column_name_old, column_name_new)  # Select only the relevant columns
print(column_mapping)

##### Peru ----
# Define file path
per_data_path <- "C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Peru/peru_data_clean/per_holpa_household_survey_clean.xlsx" #path andrea

# Read all sheet names
sheet_names <- excel_sheets(per_data_path)

# Define country name and column to rename (adjust accordingly)
country_name <- "peru"  # Replace with actual country name
column_id_rename <- "hid"  # Adjust to your specific column

# Process all sheets and create separate data frames in the environment
walk(sheet_names, function(sheet) {
  df_name <- paste0("per", sheet)  # Create dynamic name
  assign(df_name, process_survey_data(sheet, country_name, column_id_rename), envir = .GlobalEnv)
})


per_maintable <- permaintable
per_3_4_1_1_7_1_begin_repeat<-per_3_4_1_1_7_1_begin_repeat%>%
  rename("nonhired_permanent_workers"="_3_4_1_1_7_1_calculate")%>%
  labour_begin_group(.,.$nonhired_permanent_workers,"nonhired_labour_permanent")
  
per_3_4_1_1_7_2_begin_repeat<-per_3_4_1_1_7_2_begin_repeat%>%
  rename("nonhired_seasonal_workers"="_3_4_1_1_7_2_calculate")%>%
  labour_begin_group(.,.$nonhired_seasonal_workers,"nonhired_labour_seasonal")

per_3_4_1_2_1_1_begin_repeat<-per_3_4_1_2_1_1_begin_repeat%>%
  rename("hired_permanent_workers"="_3_4_1_2_1_1_calculate")%>%
  labour_begin_group(.,.$hired_permanent_workers,"hired_labour_permanent")

per_3_4_1_2_1_2_begin_repeat<-per_3_4_1_2_1_2_begin_repeat%>%
  rename("hired_seasonal_workers"="_3_4_1_2_1_2_calculate")%>%
  labour_begin_group(.,.$hired_seasonal_workers,"hired_labour_seasonal")



per_3_3_3_2_begin_repeat<-per_3_3_3_2_begin_repeat%>%
  rename("cropland_practices"="_3_3_3_1_calculate_2",
         "cropland_practices_area"="_3_3_3_2_2")%>%
  mutate(cropland_practices = str_extract(cropland_practices, "(?<=//).*"))%>%
practices_begin_group(.,.$cropland_practices,.$cropland_practices_area)




sheet_names
[1] ""                   ""        ""          ""       ""       
[6] ""          ""          ""          ""          ""         
[11] ""          ""          ""   ""   "per_3_4_1_2_7_2_1_begin_repeat"
[16] ""   ""   "per_3_4_1_2_1_2_1_begin_repeat" "per_2_4_1_begin_group"          "per_3_2_1_3_1_begin_group"     
[21] ""          ""          ""        ""        "per_3_4_3_1_2_begin_repeat"    


[26] "per_3_4_2_2_2_begin_repeat"     "per_3_4_2_2_6_begin_repeat"     "per_3_4_2_3_2_begin_repeat"     "per_3_4_2_3_2_4_begin_repeat"   "per_4_1_7_1_begin_group"       
[31] "per_4_2_1_begin_group"          "per_3_3_4_begin_group"          "per_3_3_4_1_3_begin_repeat"     "per_2_8_4_begin_group"          "per_3_3_1_begin_group"         
[36]        "per_2_12_1_begin_group"         "per_2_3_1_begin_group"          "per_3_4_3_1_1_Corregido"        "per_3_4_3_4_2_begin_repeat"    
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
  left_join(per_3_3_3_2_begin_repeat, by=c("kobo_farmer_id"))
  
  
  


### RENAME COLUMN NAMES ----
# Ensure old column names exist in per_data
existing_cols <- colnames(per_data)

# Create a named vector for renaming
rename_vector <- setNames(column_mapping$column_name_new, column_mapping$column_name_old)

# Rename only matching columns
colnames(per_data) <- ifelse(existing_cols %in% names(rename_vector), rename_vector[existing_cols], existing_cols)

# Check the updated column names
print(colnames(per_data))

########## DATA SELECTION ----
per_data<-per_data %>% select(-matches("-desc$"))
print(colnames(per_data))


  rename(
    ### HUMAN CAPITAL
    "n_adults_male", # h # of adults male
    "n_adults_female", # h # of adults female
    "n_adults_old_male" , # h # of old adults male
    "n_adults_old_female",# h # of old adults female
    "n_children_male", # h # of children male
    "n_children_female", # h # of children female
     ### SOCIAL CAPITAL
    "project_participation", #involvement in research or development project
    "years_in_community", # years of living in the community
    "n_other_farmers_visits", #number of visits from Other farmers
    "n_visits_government", #numer of visits from Government
    "n_visits_ngo", #numer of visits from NGOs	
    
         
    ### POLITICAL AND INSTITUTIONAL CONTEXT
    ## Access to knowledge
    "training_best_practices", #Training in innovative or best management agricultural practices
    "training_agribusiness" , #Training in agribusiness management and value addition
    "training_other", # Other training (please specify)
    "agroecology_knowledge", #Do you know what Agroecology means?
    "n_visits_extensionist", #number of visits from Agricultural extension workers	
    "n_visits_researchers", #number of visits from Researchers
         
         
    ## Value chain
    "n_visits_consumers", #number of visits from Consumers	
    "n_visits_traders"= , #number of visits from Food traders
    
    ## Land tenure
    "land_tenure_security", #Do you perceive that you could involuntarily lose ownership or use rights to any of the land (agricultural or not) you currently own or hold use rights to in the next 5 years?
    
    ### FARM MANAGEMENT CHARACTERISTICS
    "farm_products", #  In the last 12 months [add country meaning], what did you produce on your farm?
    
    ## FARMER BEHAVIOUR
    
    ## Governance
    "participation_nr_frequency", # How often does your household participate in activities and meetings related to the management of your community's land and natural resources?
    "influence_nr_frequency",	#How often does your household influence the decision-making that goes into the management of your community's land and natural resources?
    "nr_management_opinion"	#In your opinion, are your community's land and natural resources well-managed?
  ) 

names(per_data)


         "_2_7_1_1", #When you sell the produced CROPs, who do you sell to?
         "_2_7_1_1_1", #Specify other places where you sell the produced CROPS:
         "_2_7_1_2", #When you sell the produced LIVESTOCK, who do you sell to?
         "_2_7_1_2_1", #Specify other places where you sell the produced LIVESTOCK:
         "_2_7_1_3", #When you sell the produced FISH, who do you sell to?
         "_2_7_1_3_1", #Specify other places where you sell the produced FISH:
         "_2_7_1_4", # When you sell the produced wood, bark, rubber, etc. (from TREES), who do you sell to?
         "_2_7_1_4_1", # Specify other places where you sell the produced  WOOD/BARK/RUBBER/ETC (FROM TREES):
         "_2_7_1_5", # When you sell the produced HONEY, who do you sell to?
         "_2_7_1_5_1", #Specify other places where you sell the produced HONEY:
         
        
         
         
         
         ### FARMERS ATTITUDE
         ## Perspective on agroecology
         "_1_3_1_1_1", ##I care a lot about nature.
         "_1_3_1_1_2", ##Being in nature benefits me.
         "_1_3_1_1_3", ##I live in a place where most people take good care of the land and nature.
         "_1_3_1_1_4", ##I take care of the land and nature on my farm.
         "_1_3_1_1_5", ##I identify myself as an agroecological farmer.
         "_1_3_1_1_6", ##I have power and freedom to change farm production practices if I want to.
         "_1_3_1_1_7", ##If I work together with others in my community, we have power and freedom to solve problems facing farmers.
         "_1_3_2_1_5", ##I make decisions about what food to buy based primarily on price.
         "_1_3_2_1_6", ##I would prefer to eat food that is produced without chemical inputs.
         "_1_3_2_1_7", ##I would prefer to eat food that is grown locally.
         "_1_3_1_1_10", ##I would prefer that the food I buy is produced and processed in ways that provide a fair wage and good conditions for workers.##
         "_1_3_2_1_3", ##I think shifting to agroecological farming is a sensible business decision.
         "_1_3_2_1_4", ##I think current farming systems are working well and do not need changing.
         
         ## Governance
         "_2_2_1_1", # How often does your household participate in activities and meetings related to the management of your community's land and natural resources?
         "_2_2_1_2",	#How often does your household influence the decision-making that goes into the management of your community's land and natural resources?
         "_2_2_1_3",	#In your opinion, are your community's land and natural resources well-managed?
         
         ## Perception of fair price for products
         "_2_6_1_4_1", #Do you get a fair price for your produced CROPS?
         "_2_6_1_4_2", #Do you get a fair price for your produced LIVESTOCK?
         "_2_6_1_4_3", #Do you get a fair price for your produced FISH?
         "_2_6_1_4_4", # Do you get a fair price for your produced wood, bark, rubber, etc. (from TREES)?
         "_2_6_1_4_5" ,#Do you get a fair price for your produced HONEY?
         
         ## Human wellbeing
         "_3_1_1_1", #Thinking about your own life and personal circumstances, how satisfied are you with your life as a whole?
         "_3_1_1_2"	, #How satisfied are you with your standard of living?
         "_3_1_1_3", #How satisfied are you with your health?
         "_3_1_1_4"	, #How satisfied are you with what you are achieving in life?
         "_3_1_1_5"	, #How satisfied are you with your personal relationships?
         "_3_1_1_6"	, #How satisfied are you with how safe you feel?
         "_3_1_1_7", #How satisfied are you with feeling part of your community?
         "_3_1_1_8", #How satisfied are you with your economic security?
         "_3_1_1_9"	, #How satisfied are you with your nutritional security?
         "_3_1_1_10", #How satisfied are you with the amount of time you have to do the things that you like doing?
         "_3_1_1_11", #How satisfied are you with the quality of your local environment?
         "_3_1_1_12",	#How satisfied are you with your occupation?
         
         ## Farmer agency
         "_3_1_2_2", # On which step of the ladder would you position the majority of women in your household today?
         "_3_1_2_3", #On which step of the ladder would you position the majority of women in your household 10 years ago?
         "_3_1_2_4", #On which step of the ladder would you position the majority of men in your household today?
         "_3_1_2_5", #On which step of the ladder would you position the majority of men in your household 10 years ago?
         "_3_1_2_6", #On which step of the ladder would you position the majority of women in your community today?
         "_3_1_2_6_001", #Would you position different types of women in different places on this ladder? Older people, younger people, poorer people, single people, etc.?
         "_3_1_2_7", #On which step of the ladder would you position the majority of men in your community today?
         "_3_1_2_7_001", #Would you position different types of men in different places on this ladder? Older people, younger people, poorer people, single people, etc.?
         
         
         
         ## NATURAL CAPITAL
         "_1_4_1_1", # What is the total area in hectares (or acres) of land (agricultural or not) that your household:
         "_1_4_1_1_1", #What is the total area in hectares (or acres) of land (agricultural or not) that your household: Currently OWNS:
         "_1_4_1_1_2", #What is the total area in hectares (or acres) of land (agricultural or not) that your household: Currently LEASES  from another person:
         "_1_4_1_1_3" #What is the total area in hectares (or acres) of land (agricultural or not) that your household: Currently HOLDS USE RIGHTS, either alone or jointly with someone else:
         
         
  )

  dplyr::select("kobo_farmer_id", #farmer id
         ### HUMAN CAPITAL 
         ## Household head
         "_1_2_1_9", # gender
         "_1_2_1_10", # marital status
         "_4_1_1_1", # can you read and write
         "_1_2_1_12_1", # highest level of school atteded
         "_1_2_1_8", # year of birth
         
         ## Household
         "_1_2_1_14_1", # # of adults male
         "_1_2_1_14_2", # # of adults female
         "_1_2_1_14_3", # # of adults old male
         "_1_2_1_14_4", # # of adults old female
         "_1_2_1_14_5", # # of children male
         "_1_2_1_14_6", # # of children female
         
         ## Labour
         "h_workers_permanent_adults_old_male", # # of old male permanent household workers 
         "h_workers_permanent_adults_male", # of  male permanent household workers 
         "h_workers_permanent_adults_female", # of female permanent household workers 
         "h_workers_permanent_adults_old_female", # of old female permanent household workers 
         "h_workers_permanent_children_female", # of children female permanent household workers 
         "h_workers_permanent_children_male", # of children male permanent household workers
         
         "h_workers_seasonal_adults_old_male", # # of old male seasonal household workers 
         "h_workers_seasonal_adults_male", # of  male seasonal household workers 
         "h_workers_seasonal_adults_female", # of female seasonal household workers 
         "h_workers_seasonal_adults_old_female", # of old female seasonal household workers 
         "h_workers_seasonal_children_female", # of children female seasonal household workers 
         "h_workers_seasonal_children_male", # of children male seasonal household workers 
         
         "nonh_workers_permanent_adults_old_male", # # of old male permanent non-household workers 
         "nonh_workers_permanent_adults_male", # of  male permanent non-household workers 
         "nonh_workers_permanent_adults_female", # of female permanent non-household workers 
         "nonh_workers_permanent_adults_old_female", # of old female permanent non-household workers 
         "nonh_workers_permanent_children_female", # of children female permanent non-household workers 
         "nonh_workers_permanent_children_male", # of children male permanent non-household workers
         
         
         ### SOCIAL CAPITAL
         "_1_2_1_15", #involvement in research or development project
         "_1_2_1_7", # years of living in the community
         "_2_1_1_6", #numer of visits from Other farmers
         
         ### POLITICAL AND INSTITUTIONAL CONTEXT
         ## Access to knowledge
         "_4_1_1_4_1", #Training in innovative or best management agricultural practices
         "_4_1_1_4_3", #Training in agribusiness management and value addition
         "_4_1_1_4_4", # Other training (please specify)
         "_1_2_1_16_001", #Do you know what Agroecology means?
         "_2_1_1_1", #numer of visits from Agricultural extension workers	
         "_2_1_1_7", #numer of visits from Researchers
         
         
         ## Value chain
         "_2_1_1_2", #numer of visits from Consumers	
         "_2_1_1_3", #numer of visits from Food traders
         "_2_1_1_4", #numer of visits from Government
         "_2_1_1_5", #numer of visits from NGOs	
         
         "_2_7_1_1", #When you sell the produced CROPs, who do you sell to?
         "_2_7_1_1_1", #Specify other places where you sell the produced CROPS:
         "_2_7_1_2", #When you sell the produced LIVESTOCK, who do you sell to?
         "_2_7_1_2_1", #Specify other places where you sell the produced LIVESTOCK:
         "_2_7_1_3", #When you sell the produced FISH, who do you sell to?
         "_2_7_1_3_1", #Specify other places where you sell the produced FISH:
         "_2_7_1_4", # When you sell the produced wood, bark, rubber, etc. (from TREES), who do you sell to?
         "_2_7_1_4_1", # Specify other places where you sell the produced  WOOD/BARK/RUBBER/ETC (FROM TREES):
         "_2_7_1_5", # When you sell the produced HONEY, who do you sell to?
         "_2_7_1_5_1", #Specify other places where you sell the produced HONEY:
           

         

         ### FARMERS ATTITUDE
         ## Perspective on agroecology
         "_1_3_1_1_1", ##I care a lot about nature.
         "_1_3_1_1_2", ##Being in nature benefits me.
         "_1_3_1_1_3", ##I live in a place where most people take good care of the land and nature.
         "_1_3_1_1_4", ##I take care of the land and nature on my farm.
         "_1_3_1_1_5", ##I identify myself as an agroecological farmer.
         "_1_3_1_1_6", ##I have power and freedom to change farm production practices if I want to.
         "_1_3_1_1_7", ##If I work together with others in my community, we have power and freedom to solve problems facing farmers.
         "_1_3_2_1_5", ##I make decisions about what food to buy based primarily on price.
         "_1_3_2_1_6", ##I would prefer to eat food that is produced without chemical inputs.
         "_1_3_2_1_7", ##I would prefer to eat food that is grown locally.
         "_1_3_1_1_10", ##I would prefer that the food I buy is produced and processed in ways that provide a fair wage and good conditions for workers.##
         "_1_3_2_1_3", ##I think shifting to agroecological farming is a sensible business decision.
         "_1_3_2_1_4", ##I think current farming systems are working well and do not need changing.


         ## Perception of fair price for products
         "_2_6_1_4_1", #Do you get a fair price for your produced CROPS?
         "_2_6_1_4_2", #Do you get a fair price for your produced LIVESTOCK?
         "_2_6_1_4_3", #Do you get a fair price for your produced FISH?
         "_2_6_1_4_4", # Do you get a fair price for your produced wood, bark, rubber, etc. (from TREES)?
         "_2_6_1_4_5" ,#Do you get a fair price for your produced HONEY?
          
        ## Human wellbeing
         "_3_1_1_1", #Thinking about your own life and personal circumstances, how satisfied are you with your life as a whole?
         "_3_1_1_2"	, #How satisfied are you with your standard of living?
         "_3_1_1_3", #How satisfied are you with your health?
         "_3_1_1_4"	, #How satisfied are you with what you are achieving in life?
         "_3_1_1_5"	, #How satisfied are you with your personal relationships?
         "_3_1_1_6"	, #How satisfied are you with how safe you feel?
         "_3_1_1_7", #How satisfied are you with feeling part of your community?
         "_3_1_1_8", #How satisfied are you with your economic security?
         "_3_1_1_9"	, #How satisfied are you with your nutritional security?
         "_3_1_1_10", #How satisfied are you with the amount of time you have to do the things that you like doing?
         "_3_1_1_11", #How satisfied are you with the quality of your local environment?
         "_3_1_1_12",	#How satisfied are you with your occupation?

         ## Farmer agency
         "_3_1_2_2", # On which step of the ladder would you position the majority of women in your household today?
           "_3_1_2_3", #On which step of the ladder would you position the majority of women in your household 10 years ago?
           "_3_1_2_4", #On which step of the ladder would you position the majority of men in your household today?
           "_3_1_2_5", #On which step of the ladder would you position the majority of men in your household 10 years ago?
           "_3_1_2_6", #On which step of the ladder would you position the majority of women in your community today?
           "_3_1_2_6_001", #Would you position different types of women in different places on this ladder? Older people, younger people, poorer people, single people, etc.?
           "_3_1_2_7", #On which step of the ladder would you position the majority of men in your community today?
           "_3_1_2_7_001", #Would you position different types of men in different places on this ladder? Older people, younger people, poorer people, single people, etc.?
           
        
         
         ## NATURAL CAPITAL
         "_1_4_1_1", # What is the total area in hectares (or acres) of land (agricultural or not) that your household:
         "_1_4_1_1_1", #What is the total area in hectares (or acres) of land (agricultural or not) that your household: Currently OWNS:
         "_1_4_1_1_2", #What is the total area in hectares (or acres) of land (agricultural or not) that your household: Currently LEASES  from another person:
         "_1_4_1_1_3" #What is the total area in hectares (or acres) of land (agricultural or not) that your household: Currently HOLDS USE RIGHTS, either alone or jointly with someone else:
           
         )
  
  

########## DATA TYPE CONVERSION -----
#as numeric
numeric_valid_columns <- intersect(factors_list_holpa$column_name_new[factors_list_holpa$metric_type == "continuous"], colnames(per_data))
print(numeric_valid_columns)  # Check if it holds expected values
str(per_data)

per_data_clean<- per_data%>%
  mutate(across(all_of(numeric_valid_columns), as.numeric))%>%
  mutate(across(starts_with("nonhired_labour_"), ~ replace_na(.x, 0)))%>%
  mutate(across(starts_with("hired_labour_"), ~ replace_na(.x, 0)))
  
#as factor 
factor_valid_columns <- intersect(factors_list_holpa$column_name_new[factors_list_holpa$metric_type %in%c("categorical","binary")], colnames(per_data))
print(factor_valid_columns)  # Check if it holds expected values
str(per_data_clean)

per_data_clean<- per_data_clean%>%
  mutate(across(all_of(factor_valid_columns), as.factor))

########## DATA CALCULATION #####-----
### Potential factors ----
#household head age
per_data_clean$age <- 2025-per_data_clean$year_birth 
#total adults (18-65 years old) in household
per_data_clean$n_adults_working_age <- per_data_clean$n_adults_male+per_data_clean$n_adults_female 
#total adults (>65 years old) in household
per_data_clean$n_adults_old <- per_data_clean$n_adults_old_male+per_data_clean$n_adults_female 
# total children in household
per_data_clean$n_children <- per_data_clean$n_children_male+per_data_clean$n_children_female 
#total adults in household
per_data_clean$n_adults_total <- per_data_clean$n_adults_working_age+per_data_clean$n_adults_old 
# total number of people in household
per_data_clean$n_people <- per_data_clean$n_adults_total+per_data_clean$n_children 
### Outcomes ----
# total area (ha) of cropland under diversified farming systems
per_data_clean$dfs_total_area <- rowSums(select(per_data_clean, starts_with("dfs_") & ends_with("_area")), na.rm = TRUE)

# adoption of diversified farming systems binary (1=yes,0=no)
per_data_clean$dfs_adoption_binary <- as.factor(ifelse(per_data_clean$dfs_total_area > 0, "1","0"))

## to check: controlar si hay otras preguntas donde se citen dfs, ver las practicas de livestock

names(per_data_clean)




##########---------------- OLD
## Select variables for clustering farmers
per_selected_features <- per_data[, c("read_write", "education_level")]

# Perform k-means clustering (creating 3 clusters)
kmeans_result <- kmeans(per_selected_features, centers = 3, nstart = 25)         
kmeans_result

# Add cluster assignments to the original data
per_data$cluster <- as.factor(kmeans_result$cluster)

# Create separate Random Forest models for each cluster

# Cluster 1: Farmers in cluster 1
cluster_1 <- per_data[per_data$cluster == 1, ]
cluster_1 <- cluster_1[, !(colnames(cluster_1) %in% c("cluster", "farmer_id"))]
sort(unique(cluster_1$read_write))
cluster_1$read_write <- droplevels(cluster_1$read_write)
sort(unique(cluster_1$read_write))

rf_cluster_1 <- randomForest(read_write ~ ., data = cluster_1, ntree = 500, importance = TRUE)
importance_cluster_1 <- importance(rf_cluster_1)

# Print variable importance for each cluster
cat("\nVariable Importance - Cluster 1:\n")
print(importance_cluster_1)

# Cluster 2: Farmers in cluster 2
cluster_2 <- per_data[per_data$cluster == 2, ]
cluster_2 <- cluster_2[, !(colnames(cluster_2) %in% c("cluster", "farmer_id"))]
sort(unique(cluster_2$read_write))
cluster_2$read_write <- droplevels(cluster_2$read_write)
sort(unique(cluster_2$read_write))

rf_cluster_2 <- randomForest(read_write ~ ., data = cluster_2, ntree = 500, importance = TRUE)

# Cluster 3: Farmers in cluster 3
cluster_3 <- per_data[per_data$cluster == 3, ]
cluster_3 <- cluster_3[, !(colnames(cluster_3) %in% c("cluster", "farmer_id"))]
sort(unique(cluster_3$read_write))
cluster_3$read_write <- droplevels(cluster_3$read_write)
sort(unique(cluster_3$read_write))

rf_cluster_3 <- randomForest(read_write ~ ., data = cluster_3[, -ncol(cluster_3)], ntree = 500, importance = TRUE)


# Extract and view variable importance for each cluster

importance_cluster_1 <- importance(rf_cluster_1)
importance_cluster_2 <- importance(rf_cluster_2)
importance_cluster_3 <- importance(rf_cluster_3)

# Print variable importance for each cluster
cat("\nVariable Importance - Cluster 1:\n")
print(importance_cluster_1)

cat("\nVariable Importance - Cluster 2:\n")
print(importance_cluster_2)

cat("\nVariable Importance - Cluster 3:\n")
print(importance_cluster_3)

# Visualize the importance (optional)
# Bar plot for Cluster 1
barplot(importance_cluster_1[, 1], main = "Variable Importance - Cluster 1", col = "lightblue", las = 2)

# Bar plot for Cluster 2
barplot(importance_cluster_2[, 1], main = "Variable Importance - Cluster 2", col = "lightgreen", las = 2)

# Bar plot for Cluster 3
barplot(importance_cluster_3[, 1], main = "Variable Importance - Cluster 3", col = "lightcoral", las = 2)


