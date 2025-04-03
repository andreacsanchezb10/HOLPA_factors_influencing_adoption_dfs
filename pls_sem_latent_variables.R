library(dplyr)
library(readxl)

#############################################################    
########## UPLOAD DATA #####-----
#############################################################
per_data0<- read.csv("per_data0.csv",sep=",")#all correlated factors
per_data1<- read.csv("per_data1.csv",sep=",") #data1
per_data2<- read.csv("per_data2.csv",sep=",") #data2

factor_list<-read_excel("factors_list.xlsx",sheet = "factors_list")
  
plssem_constructs<-read_excel("factors_list.xlsx",sheet = "plssem_constructs")


per_outcomes<-factor_list%>%
  filter(category_1=="outcome")
per_outcomes<-per_outcomes$column_name_new



#############################################################    
########## SELECTED FACTORS #####-----
#############################################################
# Get column names
constructs_variables <- factor_list%>%
  select(category_1,constructs, column_name_new)%>%
  filter(category_1=="Biophysical context"|
           category_1=="Human capital"|
           constructs== "dfs_adoption_binary")%>%
  #CHECK THIS PROBLEMS TOMORROW
  #filter(constructs!="household_head_demographic")%>%
  filter(constructs!="labour")


per_data0_analysis<- per_data0%>%
  select(any_of(constructs_variables$column_name_new))%>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

dim(per_data0_analysis) #[1] 200   36
str(per_data0_analysis)

per_data1_analysis<- per_data1%>%
  select(any_of(constructs_variables$column_name_new))%>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
dim(per_data1_analysis)#[1] 200   32
str(per_data1_analysis)

per_data2_analysis<- per_data2%>%
  select(dfs_adoption_binary, any_of(per_data2_selected_factors$selected_factors))%>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
dim(per_data2_analysis)
str(per_data2_analysis)

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

# Step 1: Merge construct metadata with items
constructs_def <- constructs_variables %>%
  inner_join(plssem_constructs, by = "constructs") %>%
  filter(column_name_new %in% intersect(constructs_variables$column_name_new, colnames(per_data1_analysis)))%>%
  group_by(constructs, constructs_type, weights) %>%
  summarise(items = list(column_name_new), .groups = "drop") %>%
  mutate(n_items = lengths(items))
head(constructs_def)

##=== Reflective constructs ====
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

##=== Composite constructs multiple items ====
composite_multi_constructs <- constructs_def %>%
  filter(constructs_type == "composite", n_items > 1)%>%
  mutate(weights = trimws(weights)) %>%  # remove leading/trailing whitespace
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

##=== Composite constructs single items ====
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

##=== Measurements object ====
all_constructs <- c(
  #reflective_measures, 
  composite_multi_measures, composite_single_measures)
all_constructs

measurements <- do.call(seminr::constructs, all_constructs)
measurements

problematic <- check_multicollinearity(all_constructs, per_data1_analysis)
problematic

##=== Structural model ====
measurements
measurements2 <- constructs(
  composite("household_demographic",        c("num_children" ,"education_level_household_finished" ,"num_adults_total"), weights = mode_B),
  composite("household_head_health", c("human_wellbeing_3","human_wellbeing_10"), weights = mode_B),
  composite("household_food_security",c("access_diversified_food","access_healthy_food" ,"access_seasonal_food","access_traditional_food"), weights = mode_B ),
  composite("household_head_demographic", c("age","gender","education_level_finished"), weights = mode_B),
  composite("household_head_occupation", single_item("num_occupation_secondary_list")),
  #composite("labour", c("age","gender","education_level_finished"), weights = mode_B), #CHECK LABOUR
  
  composite("soil_characteristics_perception", single_item("soil_fertility_perception")),
  composite("soil_characteristics", c("soil_depth","soil_MO_percentage_mean","soil_pH_mean" ), weights = mode_B),
  composite("farm_topography", c("farm_elevation","soil_slope_perception"), weights = mode_B),
  
  composite("dfs_adoption_binary", single_item("dfs_adoption_binary")))
measurements2

direct_effect2<- purrr::map_chr(measurements2, ~ .[1]) %>%
  unique() %>%
  setdiff("dfs_adoption_binary")
direct_effect2

structural_model2<-relationships(
  paths(from= c(direct_effect2), to="dfs_adoption_binary"))
structural_model2


direct_effect<- purrr::map_chr(measurements, ~ .[1]) %>%
  unique() %>%
  setdiff("dfs_adoption_binary")
direct_effect

structural_model<-relationships(
  paths(from= c(direct_effect), to="dfs_adoption_binary"))
structural_model

##=== PLS model ====
data=per_data1_analysis%>%
  select("num_children" ,"education_level_household_finished" ,"num_adults_total",
         "human_wellbeing_3","human_wellbeing_10",
         "access_diversified_food","access_healthy_food" ,"access_seasonal_food","access_traditional_food",
         "age","gender","education_level_finished",
         "num_occupation_secondary_list",
         "soil_depth","soil_MO_percentage_mean","soil_pH_mean",
         "soil_fertility_perception",
         "farm_elevation","soil_slope_perception",
         "dfs_adoption_binary")
names(data)
set.seed(123)

pls_model <- estimate_pls(
  data = per_data1_analysis,
  measurement_model = measurements,
  structural_model = structural_model
)

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
  setdiff("dfs_adoption_binary")%>%
  
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




