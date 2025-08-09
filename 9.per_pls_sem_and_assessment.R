library(dplyr)
library(readxl)
library(seminr)
library(purrr)
library(rlang)
library(psych)
library(ggplot2)
#############################################################    
########## UPLOAD DATA #####-----
#############################################################
factors_list_analysis<-read_excel("factors_list.pruebaNEW.xlsx",sheet = "factors_list_analysis")

per_structural_model<-read_excel("factors_list.pruebaNEW.xlsx",sheet = "structural_model")%>%
  filter(country=="peru")

per_measurement_model<- read_excel("factors_list.pruebaNEW.xlsx",sheet = "measurement_model")%>%
  select(category_1,path,constructs, column_name_new,constructs,factor, constructs_type,weights,country)%>%
  filter(country=="peru")

sort(unique(per_measurement_model$constructs))

#############################################################    
########## SELECTED FACTORS #####-----
#############################################################
per_data_analysis<-  read.csv("per_data_Binary.csv",sep=",")%>%
  filter(crop_type.camucamu==0)
rownames(per_data_analysis) <- per_data_analysis$X
per_data_analysis<- per_data_analysis%>%
  dplyr::select(-X)%>%
  select(all_of(per_measurement_model$column_name_new))%>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

sort(names(per_data_analysis))
str(per_data_analysis)
dim(per_data_analysis)#[1] 130   20
summary(per_data_analysis)
describe(per_data_analysis)

apply(per_data_analysis, 2, function(x) var(as.numeric(x), na.rm = TRUE))

#############################################################    
########## PLS-SEM MODEL ANALYSIS #####-----
#############################################################
describe(per_data_analysis)

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


per_data_analysis %>%
  #select(indicators_used) %>%
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value)) +
  facet_wrap(~ variable, scales = "free") +
  geom_histogram(bins = 10)

#################################################################################
##=== SPECIFYING THE MEASUREMENT MODELS (Also called outer models) ======
#################################################################################
#- The measurement models (also called outer models in PLS-SEM), which describe 
#the relationships between the latent variables and their measures (i.e., their indicators)
#display the relationships between the constructs and the indicator variables
per_constructs_def <- per_measurement_model %>%
  filter(column_name_new %in% intersect(per_measurement_model$column_name_new, colnames(per_data_analysis)))%>%
  group_by(category_1,constructs, constructs_type, weights) %>%
  summarise(items = list(column_name_new), .groups = "drop") %>%
  mutate(n_items = lengths(items))

sort(unique(per_constructs_def$constructs))

#constructs_def_indirect <- per_measurement_model %>%
#  filter(column_name_new %in% intersect(per_measurement_model$column_name_new, colnames(per_data_analysis_indirect)))%>%
#  group_by(constructs, constructs_type, weights) %>%
#  summarise(items = list(column_name_new), .groups = "drop") %>%
#  mutate(n_items = lengths(items))

build_measurement_model <- function(data_analysis, constructs_variables) {
  ## ==== STEP 1: Merge construct metadata with items ====
  constructs_def <- constructs_variables %>%
    filter(column_name_new %in% intersect(constructs_variables$column_name_new, colnames(data_analysis)))%>%
    group_by(constructs, constructs_type, weights) %>%
    summarise(items = list(column_name_new), .groups = "drop") %>%
    mutate(n_items = lengths(items))
  
  ## ==== STEP 2: Get Reflective constructs ====
  reflective_constructs <- constructs_def %>%
    filter(constructs_type == "reflective", n_items >=2)%>%
    mutate(item_str = map_chr(items, ~ paste0("'", .x, "'", collapse = ", ")),
           formula = paste0("reflective('", constructs, "', c(", item_str, "))"))
  
  reflective_measures <- map(
    reflective_constructs$formula,
    ~ eval(parse_expr(.x)))
  names(reflective_measures) <- reflective_constructs$constructs
  
  ## ==== STEP 3: Get Composite constructs multiple items ====
  composite_multi_constructs <- constructs_def %>%
    filter(constructs_type == "composite", n_items > 1)%>%
    mutate(weights = trimws(weights))%>%   # remove leading/trailing whitespace
    filter(weights %in% c("mode_A", "mode_B"))%>%
    mutate(
      item_str = map_chr(items, ~ paste0("'", .x, "'", collapse = ", ")),
      formula = paste0("composite('", constructs, "', c(", item_str, "), weights = ", weights, ")"))
  
  composite_multi_measures <- map(
    composite_multi_constructs$formula,
    ~ eval(parse_expr(.x))
  )
  names(composite_multi_measures) <- composite_multi_constructs$constructs
  
  ## ==== STEP 4: Get Composite constructs single items ====
  composite_single_constructs <- constructs_def %>%
    filter(n_items == 1)%>%
    #mutate(constructs=if_else(constructs_type=="composite",as.character(items),constructs))%>%
    mutate(item_str = map_chr(items, ~ paste0("'", .x, "'", collapse = ", ")),
           formula = case_when(
             constructs_type == "composite" ~ paste0("composite('", constructs, "', c(", item_str, "))"),
             constructs_type == "reflective" ~ paste0("reflective('", constructs, "', c(", item_str, "))"),
             TRUE ~ NA_character_
           ))
             
           #formula = paste0("composite('", constructs, "', c(", item_str, "))"))
  
  composite_single_measures <- map(
    composite_single_constructs$formula,
    ~ eval(parse_expr(.x)))
  names(composite_single_measures) <- composite_single_constructs$constructs
  
  ## ==== STEP 5: Merge Constructs ====
  measurement_model <- do.call(seminr::constructs, c(  
    reflective_measures, 
    composite_multi_measures,
    composite_single_measures
  ))
  
  return(measurement_model)
}

## ==== STEP 6: Measurement model  ====
per_measurement_model_formula<- build_measurement_model(
  data_analysis=per_data_analysis,
  constructs_variables= per_measurement_model)
per_measurement_model_formula
class(per_measurement_model_formula)

#problematic <- check_multicollinearity(c(  reflective_measures, composite_multi_measures, composite_single_measures), per_data_analysis)
#problematic
summary(per_data_analysis$num_info_exchange_extension)
sd(per_data_analysis$num_info_exchange_extension, na.rm = TRUE)

summary(per_data_analysis$num_info_exchange_ngo)
sd(per_data_analysis$num_info_exchange_ngo, na.rm = TRUE)

loadings(per_boot_model_complete)

#################################################################################
##=== SPECIFYING THE STRUCTURAL MODELS (Also called inner models) ======
#################################################################################
## Direct and indirect effect on adoption ----
per_structural_model_formula<- per_structural_model%>%
  #filter(path=="indirect")%>%
  filter(!is.na(to))%>%
  mutate(formula=paste0("paths(from= '", from, "',to= '", to, "')"))%>%
  select(formula)
per_structural_model_formula

per_structural_model_formula <- map(
  per_structural_model_formula$formula,
  ~ eval(parse_expr(.x)))
per_structural_model_formula

per_structural_model_formula<-do.call(seminr::relationships, c(
  #paths(from= c(per_direct_effect), to="dfs_adoption_binary"),
  per_structural_model_formula))

per_structural_model_formula
str(per_data_analysis)
#################################################################################
##===  Estimating the PLS-SEM model: Direct and indirect effect on adoption ======
#################################################################################
## STEP 1: PLS-SEM model ----
per_measurement_model_formula
per_structural_model_formula
per_pls_sem_model_complete <- estimate_pls(data = per_data_analysis,
                                         measurement_model = per_measurement_model_formula,
                                         structural_model = per_structural_model_formula)
per_pls_sem_model_complete

# Summarize the model results
per_pls_sem_summary_complete<- summary(per_pls_sem_model_complete)
per_pls_sem_summary_complete
# Inspect the model’s path coefficients and the R^2 values
per_pls_sem_summary_complete$paths
# Inspect the construct reliability metrics 
per_pls_sem_summary_complete$reliability
# Inspect descriptive statistics
per_pls_sem_summary_complete$descriptives$statistics
#- Number of iterations that the PLS-SEM algorithm needed to converge 
#This number should be lower than the maximum number of iterations (e.g., 300)
per_pls_sem_summary_complete$iterations 
#[1] 9

per_pls_sem_summary_complete$descriptives$statistics$constructs

## STEP 2: Bootstrapping the model ----
per_boot_model_complete <- bootstrap_model(seminr_model = per_pls_sem_model_complete, 
                                         nboot = 10000, # for the final result I should use 10,000
                                         cores = 2 ) 
per_boot_model_complete
# Store the summary of the bootstrapped model 
per_boot_model_summary_complete <- summary(per_boot_model_complete,alpha = 0.01)
per_boot_model_summary_complete
per_boot_model_summary_complete$weights
per_boot_model_summary_complete$validity
per_boot_model_summary_complete$bootstrapped_paths

plot(per_boot_model_complete, title = "")

#########################################################
##===  Assessing the reflective measurement models ====
#########################################################
#An important characteristic of PLS-SEM is that the model estimates will change when 
#any of the model relationships or variables are changed. We thus need to reassess the 
#reflective measurement models to ensure that this portion of the model remains valid 
#and reliable before continuing to evaluate the four new exogenous formative constructs.

## STEP 1: Assess indicator reliability ====
#- Rule of thumb: Reflective indicator loading's threshold >= 0.708
#Obs: Rather than automatically eliminating indicators when their loading is below 0.70,
#researchers should carefully examine the effects of indicator removal on other reliability and validity measures. 

# Extract reflective constructs directly
per_measurement_model_formula
per_reflective_constructs <- unique(per_measurement_model_formula$reflective[seq(1, length(per_measurement_model_formula$reflective), 3)])
per_reflective_constructs

per_composite_constructs <- per_measurement_model_formula[names(per_measurement_model_formula) == "composite"]
per_composite_constructs
composite_by_mode <- function(mode) {
  unique(unlist(lapply(per_composite_constructs, \(x) x[seq(1, length(x), 3)][x[seq(3, length(x), 3)] == mode])))
}
per_composite_mode_A <- composite_by_mode("A")
per_composite_mode_A

per_reflective_constructs_list<-c(per_composite_mode_A)#,reflective_constructs)
per_reflective_constructs_list

per_pls_sem_summary_complete$loadings

per_assessment.mmr.step1<- as.data.frame(per_pls_sem_summary_complete$loadings)%>%
  tibble::rownames_to_column("factors") %>%
  tidyr::pivot_longer(
    cols = -factors,
    names_to = "constructs",
    values_to = "reliability") %>%
  filter(reliability != 0)%>%
  select(constructs,factors,reliability)%>%
  left_join(per_constructs_def%>%select(constructs,constructs_type,weights),by="constructs")%>%
  left_join(factors_list_analysis%>%select(category_1,column_name_new,factor),by=c("factors"="column_name_new"))%>%
  filter(constructs %in%c(per_reflective_constructs_list))%>%
  mutate(reliability_assessment= case_when(
    reliability>=0.708~ "Good",
    reliability >0.4 ~ "acceptable", # check if excluding them improve or not 
    TRUE~"Please check"))%>%
  select(category_1,constructs,factors,factor, reliability,constructs_type,weights,reliability_assessment)

write.csv(per_assessment.mmr.step1,"results/per/per_assessment.mmr.step1.csv",row.names=FALSE)

#INTERPRETATION: check if indicator loadings of the reflective measured constructs are above or bellow 0.708
#Rather than automatically eliminating indicators 
#when their loading is below 0.70, researchers should carefully examine the effects 
#of indicator removal on other reliability and validity measures. Generally, indicators 
#with loadings between 0.40 and 0.708 should be considered for removal only 
#when deleting the indicator leads to an increase in the INTERNAL CONSISTENCY RELIABILITY (STEP 2) 
#or convergent validity (discussed in the next sections) above the suggested threshold value.

#Another consideration in the decision of whether to delete an indicator
#is the extent to which its removal affects content validity, which refers to the 
#extent to which a measure represents all facets of a given construct. As a consequence, 
#indicators with weaker loadings are sometimes retained. Indicators with 
#very low loadings (below 0.40) should, however, always be eliminated from the 
#measurement model (Hair, Hult, Ringle, & Sarstedt, 2022).

#Inspect the indicator reliability
per_reliability<-as.data.frame(per_pls_sem_summary_complete$loadings^2)%>%
  tibble::rownames_to_column("factors") %>%
  tidyr::pivot_longer(
    cols = -factors,
    names_to = "constructs",
    values_to = "reliability"
  ) %>%
  filter(reliability != 0)%>%
  select(constructs,factors,reliability)

head(per_reliability)

## STEP 2: Assess internal consistency reliability AND ====
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

## STEP 3: Assess the convergent validity ====
#Convergent validity is the extent to which the construct converges in order to explain the variance of its indicators.
# average variance extracted (AVE): is defined as the grand mean value of the squared loadings of the 
#indicators associated with the construct (i.e., the sum of the squared loadings divided by the number of indicators).
#- Rule of thumb: AVE≥0.50
# INTERPRETATION: The minimum acceptable AVE is 0.50 – an AVE of 0.50 or 
#higher indicates the construct explains 50 percent or more of the indicators’ 
#variance that make up the construct (Hair et al., 2022).

# Inspect the composite reliability AND Convergent validity
per_assessment.mmr.step2.3<- as.data.frame(per_pls_sem_summary_complete$reliability)%>%
  tibble::rownames_to_column("constructs")%>%
  left_join(per_constructs_def%>%select(category_1,constructs,constructs_type),by="constructs")%>%
  filter(constructs %in%c(per_reflective_constructs_list))%>%
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

plot(per_pls_sem_summary_direct$reliability)

write.csv(per_assessment.mmr.step2.3,"results/per/per_assessment.mmr.step2.3.csv",row.names=FALSE)

## STEP 4: Assess discriminant validity ====
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
per_pls_sem_summary_complete$validity$htmt
per_assessment.mmr.step4<- as.data.frame(per_pls_sem_summary_complete$validity$htmt)%>%
  tibble::rownames_to_column("constructs1")%>%
  left_join(per_constructs_def%>%select(constructs,constructs_type),by=c("constructs1"="constructs"))%>%
  filter(constructs1 %in%c(per_reflective_constructs_list))%>%
  select(constructs1, all_of(per_reflective_constructs_list))%>%
  tidyr::pivot_longer(-constructs1, names_to = "constructs2", values_to = "correlation") 
filter(!is.na(correlation),
       constructs1 != constructs2,
       correlation >= 0.85) %>%
  rowwise() 

head(per_assessment.mmr.step4)
write.csv(per_assessment.mmr.step4,"results/per/per_assessment.mmr.step4.csv",row.names=FALSE)


# Extract the bootstrapped HTMT 
per_boot_model_summary_complete <- summary(per_boot_model_complete,alpha = 0.1)

per_boot_model_summary_complete$bootstrapped_HTMT  ### CHECK HOW TO INTERPRET THIS!!!!!!!

#########################################################
##===  Assessing the formative measurement models ====
#########################################################
## STEP 1: Assess for collinearity issues ====
###-- Multicollinearity: VIF
# Convert to dataframe
per_pls_sem_summary_complete$validity$vif_items

per_assessment.mmf.step1 <- lapply(names(per_pls_sem_summary_complete$validity$vif_items), function(construct) {
  factors <- names(per_pls_sem_summary_complete$validity$vif_items[[construct]])
  vifs <- as.numeric(per_pls_sem_summary_complete$validity$vif_items[[construct]])
  
  tibble(
    construct = construct,
    factors = factors,
    VIF = vifs
  )
}) %>% bind_rows()%>%
  left_join(per_constructs_def%>%select(constructs,constructs_type,weights),by=c("construct"="constructs"))%>%
  left_join(factors_list_analysis%>%select(category_1,column_name_new,factor),by=c("factors"="column_name_new"))%>%
  filter(constructs_type=="composite")%>%
  filter(weights=="mode_B")


## STEP 2: Assess the statistical significance and relevance of the factors weights ====
# Store the summary of the bootstrapped model 
per_boot_model_summary_complete <- summary(per_boot_model_complete,alpha = 0.05)


# Inspect the bootstrapping results for indicator weights 
#CHECK: if T Stats is >1.96
per_boot_model_summary_complete$bootstrapped_weights

per_assessment.mmf.step2<- as.data.frame(per_boot_model_summary_complete$bootstrapped_weights)
per_assessment.mmf.step2

# Inspect the boostrapping results for the outer loadings
per_boot_model_summary_complete$bootstrapped_loadings

#########################################################
##===  Adjusting for binary outcome using logistic regression----
#########################################################
## STEP 1: Extract the latent constructs ====
per_pls_sem_model_complete$construct_scores
per_pls_sem_model_complete.construct_scores<-as.data.frame(per_pls_sem_model_complete$construct_scores)

head(per_pls_sem_model_complete.construct_scores)
normalize_min_max <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

## STEP 2: Normalized the latent constructs ====
# Apply normalization to all latent variables (columns)
latent_scores_normalized <- as.data.frame(lapply(per_pls_sem_model_complete.construct_scores, normalize_min_max))

# Check the first rows of normalized latent scores
head(latent_scores_normalized)

## STEP 3: Extract the normalized latent constructs and the original values for observed variables ====
# Extract all observed variables from the measurement model
per_measurement_model_formula
per_reflective_constructs <- unique(per_measurement_model_formula$reflective[seq(1, length(per_measurement_model_formula$reflective), 3)])
per_reflective_constructs

per_reflective_constructs <- unique(per_measurement_model_formula$composite[seq(1, length(per_measurement_model$reflective), 3)])
per_reflective_constructs

per_composite_constructs <- per_measurement_model_formula[names(per_measurement_model_formula) == "composite"]
per_composite_constructs
composite_by_mode <- function(mode) {
  unique(unlist(lapply(per_composite_constructs, \(x) x[seq(1, length(x), 3)][x[seq(3, length(x), 3)] == mode])))
}
per_composite_mode_A <- composite_by_mode("A")
per_composite_mode_A
per_composite_mode_A <- setdiff(per_composite_mode_A, "environmental_quality")
per_composite_mode_A

per_reflective_constructs_list<-c(per_composite_mode_A)#,reflective_constructs)
per_reflective_constructs_list

per_observed_vars<-per_data_analysis%>%
  select(all_of(per_reflective_constructs_list),human_wellbeing_11)
names(per_observed_vars)

#Extract the latent constructs
per_composite_mode_B <- composite_by_mode("B")
per_composite_mode_B

per_data_logistic_regression_direct<- per_pls_sem_model_complete.construct_scores%>%
  select(all_of(per_composite_mode_B))%>%
  cbind(per_observed_vars)
write.csv(per_data_logistic_regression_direct, "results/per/per_data_logistic_regression_direct.csv")


## STEP 4: Apply the logistic regression model ====
#https://stats.oarc.ucla.edu/r/dae/logit-regression/
per_direct_dfs_adoption<-per_data_logistic_regression_direct%>%
  select(all_of(per_structural_model%>%
                  filter(country=="peru",to=="dfs_adoption_binary",from!="environmental_quality")%>%
           pull(from)),dfs_adoption_binary,human_wellbeing_11)

  select(-sales_channel_crops.cooperative,-crop_type.cacao)

names(per_direct_dfs_adoption)
per_direct_dfs_adoption$dfs_adoption_binary<- as.factor(per_direct_dfs_adoption$dfs_adoption_binary)

per_logit_model_dfs_adoption <- glm(dfs_adoption_binary ~ ., 
                       data = per_direct_dfs_adoption, 
                       family = binomial(link = "logit"))
summary(per_logit_model_dfs_adoption)

exp(coef(per_logit_model_dfs_adoption))
per_logit_model_dfs_adoption.results<-as.data.frame(exp(cbind(OR = coef(per_logit_model_dfs_adoption), confint(per_logit_model_dfs_adoption))))

per_direct_training_participation<-per_data_logistic_regression_direct%>%
  select(all_of(per_structural_model%>%
                  filter(country=="peru",to=="training_participation",from!="environmental_quality")%>%
                  pull(from)),training_participation)

names(per_direct_training_participation)
per_direct_training_participation$dfs_adoption_binary<- as.factor(per_direct_training_participation$dfs_adoption_binary)


per_logit_model_training_participation <- glm(training_participation ~ ., 
                                    data = per_direct_training_participation, 
                                    family = binomial(link = "logit"))
summary(per_logit_model_training_participation)

exp(coef(per_logit_model_training_participation))
per_logit_model_training_participation.results<-as.data.frame(exp(cbind(OR = coef(per_logit_model_training_participation), confint(per_logit_model_training_participation))))


#########################################################
##===  Assessing the structural model----
#########################################################
## STEP 1: Collinearity ====
per_pls_sem_summary_complete$vif_antecedents
per_assessment.sm.vif<-per_pls_sem_summary_complete$vif_antecedents%>%
  map_df(~ tibble(variable = names(.x), VIF = .x), .id = "construct")%>%
  left_join(factors_list_analysis%>%select(category_1,column_name_new,factor),by=c("variable"="column_name_new"))
  
write.csv(per_assessment.sm.vif,"results/per/per_assessment.sm.vif.csv",row.names=FALSE)

## STEP 2: Explanatory power ====
# Dependent variable: continues -> PLS-SEM results
per_pls_sem_summary_complete$paths

#Dependent variable: binary -> Logistic regression results
library(pscl)
pR2(per_logit_model_dfs_adoption)

pR2(per_logit_model_training_participation)

## STEP 3: Predictive power ====
# Dependent variable: continues -> PLS-SEM results
per_pls_sem_model_complete
rownames(per_pls_sem_model_complete$data) <- as.character(1:nrow(per_pls_sem_model_complete$data))
per_pls_sem_model_complete
per.assessment.sm.predictivePower<-seminr::predict_pls( model = per_pls_sem_model_complete,
                                                technique = predict_DA,
                                                noFolds = 10,
                                                reps = 10)
per.assessment.sm.predictivePower_summary<- summary(per.assessment.sm.predictivePower)
per.assessment.sm.predictivePower_summary
# Analyze the distribution of prediction error 
par(mfrow=c(1,4)) 
plot(per.assessment.sm.predictivePower_summary, indicator = "dfs_adoption_binary") 
plot(per.assessment.sm.predictivePower_summary, indicator = "training_participation") 
plot(per.assessment.sm.predictivePower_summary, indicator = "governance_involvement") 
plot(per.assessment.sm.predictivePower_summary, indicator = "household_shock_recover_capacity") 
par(mfrow=c(1,1))

per.assessment.sm.predictivePower_summary

#Dependent variable: binary -> Logistic regression results
#https://bradleyboehmke.github.io/HOML/logistic-regression.html

#5.5 assessing accuracy
per_direct_dfs_adoption$dfs_adoption_binary <- factor(
  per_direct_dfs_adoption$dfs_adoption_binary,
  levels = c("0", "1"),
  labels = c("No", "Yes")  # "Yes" = positive class
)
set.seed(123)
library(caret)

per.assessment.sm.predictivePower_dfs_adoption <- train(
  dfs_adoption_binary ~ ., 
  data = per_direct_dfs_adoption, 
  method = "glm",
  family = binomial(link = "logit"),
  trControl = trainControl(method = "repeatedcv", number = 10,repeats = 10,classProbs = TRUE,summaryFunction = twoClassSummary,),
  metric = "ROC"
)
per.assessment.sm.predictivePower_dfs_adoption
summary(per.assessment.sm.predictivePower_dfs_adoption$results$ROC)


per_direct_training_participation$training_participation <- factor(
  per_direct_training_participation$training_participation,
  levels = c("0", "1"),
  labels = c("No", "Yes")  # "Yes" = positive class
)
set.seed(123)
per.assessment.sm.predictivePower_training_participation <- train(
  training_participation ~ ., 
  data = per_direct_training_participation, 
  method = "glm",
  family = binomial(link = "logit"),
  trControl = trainControl(method = "repeatedcv", number = 10,repeats = 10,classProbs = TRUE,summaryFunction = twoClassSummary,),
  metric = "ROC"
)
per.assessment.sm.predictivePower_training_participation
summary(per.assessment.sm.predictivePower_training_participation$results$ROC)
