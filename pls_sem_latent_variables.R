library(dplyr)
library(readxl)

#############################################################    
########## UPLOAD DATA #####-----
#############################################################
#per_data0<- read.csv("per_data0.csv",sep=",")#all correlated factors
per_data1<- read.csv("per_data_Filterednzv.csv",sep=",") #data1
#per_data2<- read.csv("per_data2.csv",sep=",") #data2

sort(unique(per_data1$year_assessment.2023))
factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")%>%
  filter(category_1!="xxx")%>%
  filter(is.na(peru_remove))%>%
  mutate(column_name_new=case_when(
    column_name_new == "year_assessment"~ "year_assessment.2023",
    TRUE~column_name_new))
factors_list$category_1[grepl("^P&I context", factors_list$category_1)] <- "P&I context"
factors_list$category_1[grepl("^P&I context", factors_list$category_1)] <- paste0(
  factors_list$category_1[grepl("^P&I context", factors_list$category_1)],"_",  factors_list$category_2[grepl("^P&I context", factors_list$category_1)])
factors_list$category_1[grepl("^P&I context_knowledge", factors_list$category_1)] <- "P&I context_knowledge"


per_outcomes<-factors_list%>%
  filter(category_1=="outcome")
per_outcomes<-per_outcomes$column_name_new


#############################################################    
########## SELECTED FACTORS #####-----
#############################################################
# Get column names
constructs_variables <- factors_list%>%
  select(category_1,constructs, column_name_new,constructs, constructs_type,weights)%>%
  filter(category_1=="biophysical_context"|
           category_1=="farmers_behaviour"|
           category_1=="financial_capital"|
           category_1=="natural_capital"|
           category_1=="P&I_context_financial_risk_management"|
           category_1=="P&I_context_knowledge"|
           category_1== "P&I_context_land_tenure"|
           category_1=="social_capital"|
         
           constructs=="dfs_adoption_binary")
  #filter(constructs=="climate_stress_exposure") #ERROR
  #filter(constructs=="farm_elevation") #ERROR
  filter(constructs=="perception_rainfall_timing_variability") #ERROR
  
sort(unique(constructs_variables$constructs))
#[1] ""                ""                         "" "soil_erosion"                          
#[5] "soil_quality"                           "soil_slope"                             "year_assessment"                       
 
#per_data0_analysis<- per_data0%>%
  #  select(any_of(constructs_variables$column_name_new))%>%
  #mutate(across(everything(), ~ as.numeric(as.character(.))))

  #dim(per_data0_analysis) #[1] 200   48
  #str(per_data0_analysis)

per_data1_analysis<- per_data1%>%
  select(dfs_adoption_binary,any_of(constructs_variables$column_name_new))%>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
dim(per_data1_analysis)#[1] 200   106
str(per_data1_analysis)

#per_data2_analysis<- per_data2%>%
#  select(dfs_adoption_binary, any_of(per_data2_selected_factors$selected_factors))%>%
#  mutate(across(everything(), ~ as.numeric(as.character(.))))
#dim(per_data2_analysis)
#str(per_data2_analysis)

#############################################################    
########## PLS-SEM ANALYSIS #####-----
#############################################################
library(semPLS)
library(seminr)
library(purrr)
library(rlang)
library(psych)

colnames(per_data1_analysis)
describe(per_data1_analysis)

check_multicollinearity <- function(construct_list, data, threshold = 0.999) {
  problematic_constructs <- list()
  
  for (construct_name in names(construct_list)) {
    construct <- construct_list[[construct_name]]
    
    # Skip construct name and weight codes (A, B, C)
    weights <- c("A", "B", "C")
    vars <- setdiff(construct, c(construct_name, weights))
    
    # Keep only variables present in data
    vars_in_data <- vars[vars %in% colnames(data)]
    missing <- setdiff(vars, colnames(data))
    
    if (length(missing) > 0) {
      warning(sprintf("⚠️ Variables not found in data for '%s': %s", 
                      construct_name, paste(missing, collapse = ", ")))
    }
    
    if (length(vars_in_data) > 1) {
      sub_data <- data[, vars_in_data, drop = FALSE]
      cor_matrix <- cor(sub_data, use = "pairwise.complete.obs")
      
      # Check near-perfect correlation or determinant near zero
      high_cor <- which(abs(cor_matrix) > threshold & abs(cor_matrix) < 1, arr.ind = TRUE)
      if (length(high_cor) > 0 || abs(det(cor_matrix)) < 1e-12) {
        problematic_constructs[[construct_name]] <- cor_matrix
      }
    }
  }
  
  return(problematic_constructs)
}

#########################################################
########## Measurements model (also called outer model) ====
##########################################################
#- The measurement models (also called outer models in PLS-SEM), which describe 
#the relationships between the latent variables and their measures (i.e., their indicators)
#display the relationships between the constructs and the indicator variables
##=== STEP 1: Merge construct metadata with items ====
constructs_def <- constructs_variables %>%
  #inner_join(plssem_constructs, by = "constructs") %>%
  filter(column_name_new %in% intersect(constructs_variables$column_name_new, colnames(per_data1_analysis)))%>%
  group_by(constructs, constructs_type, weights) %>%
  summarise(items = list(column_name_new), .groups = "drop") %>%
  mutate(n_items = lengths(items))

##=== STEP 2: Get Reflective constructs ====
reflective_constructs <- constructs_def %>%
  filter(constructs_type == "reflective", n_items >= 2)%>%
  mutate(item_str = map_chr(items, ~ paste0("'", .x, "'", collapse = ", ")),
         formula = paste0("reflective('", constructs, "', c(", item_str, "))"))

reflective_constructs$formula

reflective_measures <- map(
  reflective_constructs$formula,
  ~ eval(parse_expr(.x))
)
reflective_measures
names(reflective_measures) <- reflective_constructs$constructs
reflective_measures

##=== STEP 3: Get Composite constructs multiple items ====
composite_multi_constructs <- constructs_def %>%
  filter(constructs_type == "composite", n_items > 1)%>%
  mutate(weights = trimws(weights))%>%   # remove leading/trailing whitespace
  filter(weights %in% c("mode_A", "mode_B"))%>%
  mutate(
    item_str = map_chr(items, ~ paste0("'", .x, "'", collapse = ", ")),
    formula = paste0("composite('", constructs, "', c(", item_str, "), weights = ", weights, ")"))
composite_multi_constructs$formula

composite_multi_measures <- map(
  composite_multi_constructs$formula,
  ~ eval(parse_expr(.x))
)
names(composite_multi_measures) <- composite_multi_constructs$constructs
composite_multi_measures
class(composite_multi_measures[[1]])

##=== STEP 4: Get Composite constructs single items ====
composite_single_constructs <- constructs_def %>%
  filter(n_items == 1)%>%
  mutate(item_str = map_chr(items, ~ paste0("'", .x, "'", collapse = ", ")),
         formula = paste0("composite('", constructs, "', c(", item_str, "))"))
composite_single_constructs$formula

composite_single_measures <- map(
  composite_single_constructs$formula,
  ~ eval(parse_expr(.x))
)
composite_single_measures
names(composite_single_measures) <- composite_single_constructs$constructs
composite_single_measures

##=== STEP 4: Merge Constructs ====
#reflective_measurement_model <- do.call(seminr::constructs, reflective_measures)
#reflective_measurement_model

#composite_measurement_model<- do.call(seminr::constructs, c(composite_multi_measures, composite_single_measures))
#composite_measurement_model

measurement_model <- do.call(seminr::constructs, c(  
  reflective_measures, 
  composite_multi_measures,
  composite_single_measures
  ))
measurement_model

problematic <- check_multicollinearity(c(  reflective_measures, composite_multi_measures, composite_single_measures), per_data1_analysis)
problematic


#########################################################
########## Structural model (also called inner model) ====
#########################################################

#measurements2 <- constructs(
# composite("household_demographic",        c("num_children" ,"education_level_household_finished" ,"num_adults_total")),
# composite("household_head_health", c("human_wellbeing_3","human_wellbeing_10"), weights = mode_B),
# composite("household_food_security",c("access_diversified_food","access_healthy_food" ,"access_seasonal_food","access_traditional_food"), weights = mode_B ),
# composite("household_head_demographic", c("age","gender","education_level_finished"), weights = mode_B),
# composite("household_head_occupation", single_item("num_occupation_secondary_list")),
  #composite("labour", c("age","gender","education_level_finished"), weights = mode_B), #CHECK LABOUR
# composite("soil_characteristics_perception", single_item("soil_fertility_perception")),
# composite("soil_characteristics", c("soil_depth","soil_MO_percentage_mean","soil_pH_mean" ), weights = mode_B),
# composite("farm_topography", c("farm_elevation","soil_slope_perception"), weights = mode_B),
#  composite("dfs_adoption_binary", single_item("dfs_adoption_binary")))
#measurements2

#direct_effect2<- purrr::map_chr(measurements2, ~ .[1]) %>%
#  unique() %>%
#  setdiff("dfs_adoption_binary")
#direct_effect2

#structural_model2<-relationships(
# paths(from= c(direct_effect2), to="dfs_adoption_binary"))
#structural_model2

#reflective_direct_effect<- purrr::map_chr(reflective_measurement_model, ~ .[1]) %>%
 # unique() %>%
  #setdiff("dfs_adoption_binary")

#reflective_structural_model <-relationships(
# paths(from= c(reflective_direct_effect), to="dfs_adoption_binary"))
#reflective_structural_model

direct_effect<- purrr::map_chr(measurement_model, ~ .[1]) %>%
  unique() %>%
  setdiff("dfs_adoption_binary")
direct_effect

structural_model <-relationships(
  paths(from= c(direct_effect), to="dfs_adoption_binary"))


structural_model


#measurement_model<-constructs(
 # composite("perception_rainfall_timing_variability", c("rainfall_timing_change_perception.startearlier",  "rainfall_timing_change_perception.startlater" ,   "rainfall_timing_change_perception.stopearlier" , 
                                                       # "rainfall_timing_change_perception.stoplater" ,    "rainfall_timing_change_perception.unpredictable"),weights = mode_B),
# composite("dfs_adoption_binary",c("dfs_adoption_binary")))
  
#structural_model <-relationships(
  #paths(from= "climate_stress_exposure", to="dfs_adoption_binary"),
# paths(from= "perception_rainfall_timing_variability", to="dfs_adoption_binary"))
  

#structural_model

#########################################################
########## Estimating the PLS-SEM model ====
#########################################################
names(per_data1_analysis)
pls_sem_model <- estimate_pls(data = per_data1_analysis,
                              measurement_model = measurement_model,
                              structural_model = structural_model)

# Summarize the model results
pls_sem_summary<- summary(pls_sem_model)
# Inspect the model’s path coefficients and the R^2 values
pls_sem_summary$paths
# Inspect the construct reliability metrics 
pls_sem_summary$reliability
# Inspect descriptive statistics
pls_sem_summary$descriptives$statistics
#- Number of iterations that the PLS-SEM algorithm needed to converge 
#This number should be lower than the maximum number of iterations (e.g., 300)
pls_sem_summary$iterations 
#[1] 1

#########################################################
########## Bootstrapping the model ====
#########################################################
boot_model <- bootstrap_model(seminr_model = pls_sem_model, 
                                         nboot = 1000, 
                                         cores = 2 ) 
# Store the summary of the bootstrapped model 
boot_model_summary <- summary(boot_model,alpha = 0.10)
boot_model_summary
plot(boot_model, title = "")


#########################################################
########## Assessing the reflective measurement models ====
#########################################################
#An important characteristic of PLS-SEM is that the model estimates will change when 
#any of the model relationships or variables are changed. We thus need to reassess the 
#reflective measurement models to ensure that this portion of the model remains valid 
#and reliable before continuing to evaluate the four new exogenous formative constructs.

##=== STEP 1: Assess indicator reliability ====
#- Rule of thumb: Reflective indicator loading's threshold >= 0.708
#Obs: Rather than automatically eliminating indicators when their loading is below 0.70,
#researchers should carefully examine the effects of indicator removal on other reliability and validity measures. 

#Inspect the outer loading

outer_loading<- as.data.frame(pls_sem_summary$loadings)%>%
  tibble::rownames_to_column("factors") %>%
  tidyr::pivot_longer(
    cols = -factors,
    names_to = "constructs",
    values_to = "reliability"
  ) %>%
  filter(reliability != 0)%>%
  select(constructs,factors,reliability)%>%
  left_join(constructs_def%>%select(constructs,constructs_type),by="constructs")%>%
  filter(constructs_type=="reflective")%>%
  mutate(reliability_assessment= case_when(
    reliability>=0.708~ "Good",
    reliability >0.4 ~ "acceptable", # check if excluding them improve or not 
    TRUE~"Please check"))

#INTERPRETATION: check if indicator loadings of the reflective measured constructs are above or bellow 0.708
#Rather than automatically eliminating indicators 
#when their loading is below 0.70, researchers should carefully examine the effects 
#of indicator removal on other reliability and validity measures. Generally, indica-
#tors with loadings between 0.40 and 0.708 should be considered for removal only 
#when deleting the indicator leads to an increase in the INTERNAL CONSISTENCY RELIABILITY (STEP 2) 
#or convergent validity (discussed in the next sections) above the suggested threshold value.

#Another consideration in the decision of whether to delete an indicator
#is the extent to which its removal affects content validity, which refers to the 
#extent to which a measure represents all facets of a given construct. As a consequence, 
#indicators with weaker loadings are sometimes retained. Indicators with 
#very low loadings (below 0.40) should, however, always be eliminated from the 
#measurement model (Hair, Hult, Ringle, & Sarstedt, 2022).

#Inspect the indicator reliability
reliability<-as.data.frame(pls_summary$loadings^2)%>%
  tibble::rownames_to_column("factors") %>%
  tidyr::pivot_longer(
    cols = -factors,
    names_to = "constructs",
    values_to = "reliability"
  ) %>%
  filter(reliability != 0)%>%
  select(constructs,factors,reliability)

head(reliability)

##=== STEP 2: Assess internal consistency reliability AND ====
#Internal consistency reliability is the extent to which 
#indicators measuring the same construct are associated with each other. 
#- Rule of thumb:
#Cronbach’s alpha is the lower bound, and the composite reliability rhoc is 
#the upper bound for internal consistency reliability. The reliability 
#coefficient rhoA usually lies between these bounds and may serve as a good 
#representation of a construct’s internal consistency reliability
#Minimum 0.70 (or 0.60 in exploratory research)
#Maximum of 0.95 to avoid indicator redundancy, which would compromise content validity
#Recommended 0.80 to 0.90

##=== STEP 3: Assess the convergent validity ====
#Convergent validity is the extent to which the construct converges in order to explain the variance of its indicators.
# average variance extracted (AVE): is defined as the grand mean value of the squared loadings of the 
#indicators associated with the construct (i.e., the sum of the squared loadings divided by the number of indicators).
#- Rule of thumb: AVE≥0.50
# INTERPRETATION: The minimum acceptable AVE is 0.50 – an AVE of 0.50 or 
#higher indicates the construct explains 50 percent or more of the indicators’ 
#variance that make up the construct (Hair et al., 2022).

# Inspect the composite reliability AND Convergent validity
internal_consistency_reliability<- as.data.frame(relective_pls_sem_summary$reliability)%>%
  tibble::rownames_to_column("constructs")%>%
  left_join(constructs_def%>%select(constructs,constructs_type),by="constructs")%>%
  filter(constructs_type=="reflective")%>%
  mutate(alpha_assessment= case_when(
    alpha >0.95 ~ "Check for redundancy",
    alpha <=0.95 & alpha >0.9 ~ "Maximum accepted",
    alpha<=0.9 & alpha >=0.8 ~ "Recommended",
    alpha <0.8 & alpha >= 0.6~ "Acceptable for exploratory research",
    alpha <0.6 ~ "Please check",TRUE~"NA"))%>%
  mutate(rhoC_assessment= case_when(
    rhoC >0.95 ~ "Check for redundancy",
    rhoC <=0.95 & rhoC >0.9 ~ "Maximum accepted",
    rhoC<=0.9 & rhoC >=0.8 ~ "Recommended",
    rhoC <0.8 & rhoC >= 0.6~ "Acceptable for exploratory research",
    rhoC <0.6 ~ "Please check",TRUE~"NA"))%>%
  mutate(rhoA_assessment= case_when(
    rhoA >0.95 ~ "Check for redundancy",
    rhoA <=0.95 & rhoA >0.9 ~ "Maximum accepted",
    rhoA<=0.9 & rhoA >=0.8 ~ "Recommended",
    rhoA <0.8 & rhoA >= 0.6~ "Acceptable for exploratory research",
    rhoA <0.6 ~ "Please check", TRUE~"NA"))%>%
  mutate(AVE_assessment= case_when(
    AVE >=0.5 ~"Good",
    TRUE~"Please check"))

plot(pls_summary$reliability)
  
##=== STEP 4: Assess discriminant validity ====
#This metric measures the extent to which a construct is empirically distinct from other constructs in the structural model
#- Rule of thumb: 
#For conceptually similar constructs, HTMT <0.90
#For conceptually different constructs, HTMT <0.85
#Test if the HTMT is significantly lower than the threshold value

#INTERPRETATION: Discriminant validity problems are present when HTMT values are high. 
#Henseler et al. (2015) propose a threshold value of 0.90 for structural models with 
#constructs that are conceptually very similar, such as cognitive satisfaction, affective 
#satisfaction, and loyalty. In such a setting, an HTMT value above 0.90 would 
#suggest that discriminant validity is not present. But when constructs are conceptually 
#more distinct, a lower, more conservative, threshold value is suggested, such as 0.85 (Henseler et al., 2015).

discriminat_validity<- as.data.frame(relective_pls_sem_summary$validity$htmt)%>%
  tibble::rownames_to_column("constructs")
  

# Extract the bootstrapped HTMT 
reflective_boot_model_summary$bootstrapped_HTMT  ### CHECK HOW TO INTERPRET THIS!!!!!!!

#########################################################
########## Assessing the formative measurement models ====
#########################################################
##=== STEP 1: Assess the convergent validity (redundancy analysis) ====
#In formative measurement model evaluation, convergent validity refers to the 
#degree to which the formatively specified construct correlates with an alternative 
#reflectively measured variable(s) of the same concept.
#- Rule of thumb: 
#≥ 0.708 correlation between the formative construct and a 
#reflective (or single-item) measurement of the same concept

#INTERPRETATION: Hair et al. (2022) suggest the correlation 
#of the formatively measured construct with the reflectively measured item(s) should 
#be 0.708 or higher, which implies that the construct explains (more than) 50% of 
#the alternative measure’s variance.

demo("seminr-primer-chap5")



###-- Multicollinearity: VIF

pls_summary$validity$vif_items #item reliability










summary(pls_model)$validity$vif_items #All VIF values are below 3.3, which means no concerning multicollinearity
summary(pls_model)$validity$htmt
#HTMT Value	Interpretation	Recommended Action
#< 0.85	Good discriminant validity	Keep both constructs
#0.85–0.90	Acceptable in some disciplines	Review, may keep with justification
#> 0.90	Problematic — constructs overlap	Consider merging, revising, or removing


#Reliability:
#                            alpha  rhoC   AVE  rhoA
#climate_change_perceptions  0.662 0.667 0.344 1.000
#climate_events_exposure    -0.707 0.123 0.611 1.000 REMOVE LOW rhoC
#farm_topography             0.557 0.818 0.692 1.000
#"household_demographic"       0.290 0.006 0.356 1.000 REMOVE LOW rhoC
#household_food_security     0.807 0.826 0.550 1.000
#household_head_demographic -0.247 0.123 0.280 1.000 REMOVE LOW rhoC
#household_head_health      -0.050 0.590 0.494 1.000 REMOVE NEGATIVE ALPHA
#soil_characteristics        0.056 0.079 0.267 1.000 REMOVE LOW rhoC
#household_head_occupation   1.000 1.000 1.000 1.000
#dfs_adoption_binary         1.000 1.000 1.000 1.000

remove<- c("climate_events_exposure",
           "household_demographic",
           "household_head_demographic",
           "household_head_health",
           "soil_characteristics"
           
           )

#Alpha, rhoC, and rhoA should exceed 0.7 while AVE should exceed 0.5
measurements
measurements2 <- constructs(
  composite("household_food_security",c("access_diversified_food","access_healthy_food" ,"access_seasonal_food","access_traditional_food"), weights = mode_B ),
  composite("farm_topography", c("farm_elevation","soil_slope_perception"), weights = mode_B),
  composite("household_head_occupation", single_item("num_occupation_secondary_list")),
  composite("climate_change_perceptions", c("rainfall_amount_change_perception","rainfall_timing_change_perception.startearlier", "rainfall_timing_change_perception.startlater",
                                            "rainfall_timing_change_perception.stopearlier" ,"rainfall_timing_change_perception.stoplater","rainfall_timing_change_perception.unpredictable"), weights = mode_B),

  composite("dfs_adoption_binary", single_item("dfs_adoption_binary")))
measurements2


direct_effect2<- purrr::map_chr(measurements2, ~ .[1]) %>%
  unique() %>%
  setdiff("dfs_adoption_binary")
  
direct_effect2

structural_model2<-relationships(
  paths(from= c(direct_effect2), to="dfs_adoption_binary")
  #paths(from= "dfs_adoption_binary", to="household_food_security")
  )
structural_model2

data2<- per_data1_analysis%>%
  select("access_diversified_food","access_healthy_food" ,"access_seasonal_food","access_traditional_food",
         "farm_elevation","soil_slope_perception",
         "num_occupation_secondary_list",
         "rainfall_amount_change_perception","rainfall_timing_change_perception.startearlier", "rainfall_timing_change_perception.startlater",
         "rainfall_timing_change_perception.stopearlier" ,"rainfall_timing_change_perception.stoplater","rainfall_timing_change_perception.unpredictable",
         
   
         "dfs_adoption_binary")
names(data2)

pls_model2 <- estimate_pls(
  data = data2,
  measurement_model = measurements2,
  structural_model = structural_model2
)

summary(pls_model2) 

boot_model_est2 <- bootstrap_model(seminr_model = pls_model2, 
                                  nboot = 1000, 
                                  cores = 2 ) 
summary(boot_model_est2)
plot(boot_model_est2, title = "")


boot_model_est3
boot_model_est2$boot_paths


plot_boot_safe <- function(boot_model, title = "") {
  # Set all diagonal path values in each bootstrap slice to NA
  for (i in 1:dim(boot_model$boot_paths)[3]) {
    diag(boot_model$boot_paths[, , i]) <- NA
  }
  
  # Also set the diagonal of the original matrix to NA if needed
  diag(boot_model$paths) <- NA
  
  # Now plot safely
  plot(boot_model, title = title)
}

plot_boot_safe(boot_model_est3, title = "Bootstrapped PLS Path Coefficients")

plot_boot_safe(boot_model_est3, title = "Bootstrapped PLS Path Coefficients")


sd_paths <- apply(boot_model_est3$boot_paths, c(1, 2), sd, na.rm = TRUE)
sd_paths
describe(per_data1_analysis)
per_data1_analysis
  

###########check errors
reduced_constructs <- all_constructs[names(all_constructs) != "household_demographic"]
measurements_test <- do.call(seminr::constructs, reduced_constructs)

# Also remove from structural model
direct_effect_test <- setdiff(direct_effect, "household_head_health")
structural_model_test <- relationships(paths(from = direct_effect_test, to = "dfs_adoption_binary"))
structural_model_test
# Re-run
pls_model_test2 <- estimate_pls(
  data = per_data1_analysis,
  measurement_model = measurements_test,
  structural_model = structural_model_test
)
pls_model_test2




  ##############################
sort(unique(constructs_variables$constructs))

head(constructs_variables)
head(plssem_constructs)

# Merge constructs with their items and data
constructs_def <- constructs_variables %>%
  inner_join(plssem_constructs, by = "constructs")

sort(unique(constructs_def$constructs_type))

##=== Composite constructs ====
composite_constructs <- constructs_def %>%
  filter(constructs_type == "composite") %>%
  group_by(constructs, weights) %>%
  summarise(items = list(column_name_new))

composite_multi_items <- composite_constructs %>%
  filter(lengths(items) > 1)

composite_single_items <- composite_constructs %>%
  filter(lengths(items) == 1)%>%
  select(-weights)

##=== Reflective constructs ====
reflective_constructs <- constructs_def %>%
  filter(constructs_type == "reflective") %>%
  group_by(constructs) %>%
  summarise(items = list(column_name_new))%>%
  ungroup()



# Reflective constructs (mode A)
reflective_measures <- purrr::pmap(
  list(reflective_constructs$constructs, reflective_constructs$items),
  ~ reflective(.x, .y)
)

composite_measures <- purrr::pmap(
  list(composite_constructs$constructs, composite_constructs$items, composite_constructs$weights),
  function(name, items, weight_mode) {
    composite(name, items, weights = weight_mode)
  }
)



# Composite constructs (mode B or A)
composite_measures <- purrr::pmap(
  list(composite_constructs$constructs, composite_constructs$items, composite_constructs$weights),
  ~ composite(name, items, weights = weight_mode)
)

# Combine all into the measurement model
measurements <- constructs(
  !!!reflective_measures,
  !!!composite_measures
)


# Distinguish and mix composite measurement (used in PLS-PM)
# or reflective (common-factor) measurement (used in CBSEM, CFA, and PLSc)
# - We will first use composites in PLS-PM analysis
# - Later we will convert the omposites into reflectives for CFA/CBSEM (step 3)
measurements <- constructs(
    composite("Image",        multi_items("IMAG", 1:5)),
    composite("Expectation",  multi_items("CUEX", 1:3)),
    composite("Value",        multi_items("PERV", 1:2)),
    composite("Satisfaction", multi_items("CUSA", 1:3)),
    interaction_term(iv = "Image", moderator = "Expectation")
  )





