
#############################################################    
########## UPLOAD DATA #####-----
#############################################################
factors_list_analysis<-read_excel("factors_list.xlsx",sheet = "factors_list_analysis")%>%
  filter(is.na(peru_remove_adoption_status))

per_structural_model<-read_excel("factors_list.xlsx",sheet = "structural_model")%>%
  filter(country=="peru")

per_measurement_model<- read_excel("factors_list.xlsx",sheet = "measurement_model")%>%
  select(category_1,path,constructs, column_name_new,constructs,factor, constructs_type,weights,country)%>%
  filter(country=="peru")

sort(unique(per_measurement_model$constructs))

#############################################################    
########## SELECTED FACTORS #####-----
#############################################################
per_data_analysis<-  read.csv("per_data_Binary.csv",sep=",")
rownames(per_data_analysis) <- per_data_analysis$X
per_data_analysis<- per_data_analysis%>%
  dplyr::select(-X)%>%
  select(all_of(per_measurement_model$column_name_new))%>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))


names(per_data_analysis)
str(per_data_analysis)
dim(per_data_analysis)#[1] 200   24
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

per_measurement_model_formula
per_structural_model_indirect
str(per_data_analysis)
#################################################################################
##===  Estimating the PLS-SEM model: Direct and indirect effect on adoption ======
#################################################################################
## STEP 1: PLS-SEM model ----
per_pls_sem_model_complete <- estimate_pls(data = per_data_analysis,
                                         measurement_model = per_measurement_model_formula,
                                         structural_model = per_structural_model_formula)
per_pls_sem_model_complete

# Summarize the model results
per_pls_sem_summary_direct<- summary(per_pls_sem_model_complete)
per_pls_sem_summary_direct
# Inspect the model’s path coefficients and the R^2 values
per_pls_sem_summary_direct$paths
# Inspect the construct reliability metrics 
per_pls_sem_summary_direct$reliability
# Inspect descriptive statistics
per_pls_sem_summary_direct$descriptives$statistics
#- Number of iterations that the PLS-SEM algorithm needed to converge 
#This number should be lower than the maximum number of iterations (e.g., 300)
per_pls_sem_summary_direct$iterations 
#[1] 1

per_pls_sem_summary_direct$descriptives$statistics$constructs

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

plot(per_boot_model_direct, title = "")

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
per_measurement_model_direct
per_reflective_constructs <- unique(per_measurement_model_direct$reflective[seq(1, length(per_measurement_model_direct$reflective), 3)])
per_reflective_constructs

per_composite_constructs <- per_measurement_model_direct[names(per_measurement_model_direct) == "composite"]
per_composite_constructs
composite_by_mode <- function(mode) {
  unique(unlist(lapply(per_composite_constructs, \(x) x[seq(1, length(x), 3)][x[seq(3, length(x), 3)] == mode])))
}
per_composite_mode_A <- composite_by_mode("A")
per_composite_mode_A

per_reflective_constructs_list<-c(per_composite_mode_A)#,reflective_constructs)
per_reflective_constructs_list

per_pls_sem_summary_direct$loadings

per_assessment.mmr.step1<- as.data.frame(per_pls_sem_summary_direct$loadings)%>%
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

write.csv(per_assessment.mmr.step1,"results/direct/per_assessment.mmr.step1.csv",row.names=FALSE)

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
per_reliability<-as.data.frame(per_pls_sem_summary_direct$loadings^2)%>%
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
per_assessment.mmr.step2.3<- as.data.frame(per_pls_sem_summary_direct$reliability)%>%
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

write.csv(per_assessment.mmr.step2.3,"results/direct/per_assessment.mmr.step2.3.csv",row.names=FALSE)

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
per_assessment.mmr.step4<- as.data.frame(per_pls_sem_summary_direct$validity$htmt)%>%
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
write.csv(per_assessment.mmr.step4,"results/direct/per_assessment.mmr.step4.csv",row.names=FALSE)


# Extract the bootstrapped HTMT 
per_boot_model_summary_complete <- summary(per_boot_model_direct,alpha = 0.1)

per_boot_model_summary_complete$bootstrapped_HTMT  ### CHECK HOW TO INTERPRET THIS!!!!!!!

#########################################################
##===  Assessing the formative measurement models ====
#########################################################
## STEP 1: Assess for collinearity issues ====
###-- Multicollinearity: VIF
# Convert to dataframe
per_pls_sem_summary_direct$validity$vif_items

per_assessment.mmf.step1 <- lapply(names(per_pls_sem_summary_direct$validity$vif_items), function(construct) {
  factors <- names(per_pls_sem_summary_direct$validity$vif_items[[construct]])
  vifs <- as.numeric(per_pls_sem_summary_direct$validity$vif_items[[construct]])
  
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
per_boot_model_summary_complete <- summary(per_boot_model_direct,alpha = 0.05)


# Inspect the bootstrapping results for indicator weights 
#CHECK: if T Stats is >1.96
per_boot_model_summary_complete$bootstrapped_weights

per_assessment.mmf.step2
per_assessment.mmf.step2<- as.data.frame(per_boot_model_summary_complete$bootstrapped_weights)

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
per_measurement_model_direct
per_reflective_constructs <- unique(per_measurement_model_direct$reflective[seq(1, length(per_measurement_model_direct$reflective), 3)])
per_reflective_constructs

per_reflective_constructs <- unique(per_measurement_model_direct$composite[seq(1, length(per_measurement_model$reflective), 3)])
per_reflective_constructs

per_composite_constructs <- per_measurement_model_direct[names(per_measurement_model_direct) == "composite"]
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

per_observed_vars<-per_data_adoptionBinary_analysis%>%
  select(all_of(per_reflective_constructs_list))
names(per_observed_vars)

#Extract the latent constructs
per_composite_mode_B <- composite_by_mode("B")
per_composite_mode_B

per_data_logistic_regression_direct<- per_pls_sem_model_complete.construct_scores%>%
  select(all_of(per_composite_mode_B))%>%
  cbind(per_observed_vars)
write.csv(per_data_logistic_regression_direct, "per_data_logistic_regression_direct.csv")


## STEP 4: Apply the logistic regression model ====
#https://stats.oarc.ucla.edu/r/dae/logit-regression/
per_logit_model <- glm(dfs_adoption_binary ~ ., 
                       data = per_data_logistic_regression_direct, 
                       family = binomial(link = "logit"))
summary(per_logit_model)

exp(coef(per_logit_model))
per_logit_model.results<-as.data.frame(exp(cbind(OR = coef(per_logit_model), confint(per_logit_model))))












###########desde aca el nuevo procedimiento #########----------------------------------------------
##=== MODEL 2: Indirect effect to adoption NOT APPLY ----
per_indirect_effect<- per_structural_model%>%
  filter(path_type=="indirect")%>%
  filter(!is.na(to))%>%
  mutate(formula=paste0("paths(from= '", from, "',to= '", to, "')"))%>%
  select(formula)
per_indirect_effect

per_indirect_effect <- map(
  per_indirect_effect$formula,
  ~ eval(parse_expr(.x)))
per_indirect_effect

per_structural_model_indirect<-do.call(seminr::relationships, c(  
  paths(from= c(per_direct_effect), to="dfs_adoption_binary"),
  per_indirect_effect))

per_structural_model_indirect


#################################################################################
##===  Estimating the PLS-SEM model: Indirect effect ======
#################################################################################
## STEP 1: PLS-SEM model ----
per_pls_sem_model_indirect <- estimate_pls(data = per_data_adoptionBinary_analysis,
                                           measurement_model = per_measurement_model.formula,
                                           structural_model = per_structural_model_indirect)
per_pls_sem_model_indirect

# Summarize the model results
per_pls_sem_summary_indirect<- summary(per_pls_sem_model_indirect)
per_pls_sem_summary_indirect
# Inspect the model’s path coefficients and the R^2 values
per_pls_sem_summary_indirect$paths
# Inspect the construct reliability metrics 
per_pls_sem_summary_indirect$reliability
# Inspect descriptive statistics
per_pls_sem_summary_indirect$descriptives$statistics
#- Number of iterations that the PLS-SEM algorithm needed to converge 
#This number should be lower than the maximum number of iterations (e.g., 300)
per_pls_sem_summary_indirect$iterations 
#[1] 4

## STEP 2: Bootstrapping the model ----
per_boot_model_indirect <- bootstrap_model(seminr_model = per_pls_sem_model_indirect, 
                                           nboot = 10000, # for the final result I should use 10,000
                                           cores = 2 ) 
per_boot_model_indirect
# Store the summary of the bootstrapped model 
per_boot_model_summary_indirect <- summary(per_boot_model_indirect,alpha = 0.01)
per_boot_model_summary_indirect
per_boot_model_summary_indirect$bootstrapped_weights

plot(per_boot_model_indirect, title = "")


#########################################################
##===  Assessing the structural model ====
#########################################################

## STEP 1: Assess Collinearity issues of the structural model ====
#VIF values above 5 are indicative of probable collinearity issues among predictor constructs,
#but collinearity can also occur at lower VIF values of 3–5
per_pls_sem_summary_direct$vif_antecedents
per_pls_sem_summary_indirect$vif_antecedents

per_assessment.sm.step1<- as.data.frame(per_pls_sem_summary_indirect$vif_antecedents)%>%
  tibble::rownames_to_column("constructs") %>%
  mutate(collinearity_assessment= case_when(
    dfs_adoption_binary>=5~"critical",
    dfs_adoption_binary>=3~"uncritical",
    TRUE~"not problem"
    
  ))%>%
  #left_join(per_constructs_def%>%select(constructs,constructs_type,weights),by=c("constructs"="constructs"))
  left_join(per_factors_list%>%select(category_1,constructs,factor),by=c("constructs"="constructs"))

write.csv(per_assessment.sm.step1,"results/per_assessment.sm.step1.csv",row.names=FALSE)

## STEP 2: Assess the significance and relevance of the structural model relationships ====
#A path coefficient is significant at the 5% level if the value zero does not fall into the 95% confidence interval.  
per_boot_model_summary_complete <- summary(per_boot_model_direct, alpha = 0.05)
per_boot_model_summary_indirect <- summary(per_boot_model_indirect, alpha = 0.05)

# Inspect the structural paths 
per_boot_model_summary_complete$bootstrapped_paths
per_boot_model_summary_indirect$bootstrapped_paths

per_assessment.sm.step2.paths<-as.data.frame(per_boot_model_summary_indirect$bootstrapped_paths)%>%
  tibble::rownames_to_column("constructs")%>%
  mutate(p_value = 2 * pnorm(-abs(`T Stat.`)),
         significance = case_when(
           p_value < 0.001 ~ "***",
           p_value < 0.01  ~ "**",
           p_value < 0.05  ~ "*",
           TRUE            ~ ""))

# Inspect the total effects 
per_boot_model_summary_complete$bootstrapped_total_paths 
per_boot_model_summary_indirect$bootstrapped_total_paths

per_assessment.sm.step2.total.paths<-as.data.frame(per_boot_model_summary_indirect$bootstrapped_total_paths)%>%
  tibble::rownames_to_column("paths") %>%
  mutate(p_value = 2 * pnorm(-abs(`T Stat.`)),
         significance = case_when(
           p_value < 0.001 ~ "***",
           p_value < 0.01  ~ "**",
           p_value < 0.05  ~ "*",
           TRUE            ~ ""))
head(per_assessment.sm.step2.total.paths)

## STEP 3: Assess the model's explanatory power ====
# Inspect the model RSquares 
per_pls_sem_summary_direct$paths
per_pls_sem_summary_indirect$paths

per_assessment.sm.step3<- as.data.frame(per_pls_sem_summary_indirect$paths )%>%
  tibble::rownames_to_column("paths") %>%
  mutate(Rsquare_evaluation= case_when(
    abs(dfs_adoption_binary) >=0.75~ "substantial",
    abs(dfs_adoption_binary) >=0.5~ "moderate",
    
    TRUE~"weak"
  ))
per_assessment.sm.step3

# Inspect the effect sizes 
per_pls_sem_summary_direct$fSquare  
per_pls_sem_summary_indirect$fSquare  


## STEP 4: Assess the model's predictive power ====
# Generate the model predictions
per_predict_model_indirect <- predict_pls(model = per_pls_sem_model_indirect, 
                                          technique = predict_DA, 
                                          noFolds = 10, 
                                          reps = 20) # Summarize the prediction results sum_predict_corp_rep_ext <- summary(predict_corp_rep_ext)

# Summarize the prediction results
per_predict_model_indirect
summary(per_predict_model_indirect)
per_predict_summary_indirect <- summary(per_predict_model_indirect)


# Analyze the distribution of prediction error 
par(mfrow=c(1,1)) 
plot(per_predict_summary_indirect, indicator = "dfs_adoption_binary") 

# Compute the prediction statistics
#Interpretation: the error distribution is symmetric so RMSE should be use
per_predict_summary_indirect

#INTERPRETATION:
#PLS out of sample: dfs_adoption_binary: RMSE= 0.313
#LM out of sample: dfs_adoption_binary: RMSE= 0.318
#PLS out of sample is lower than LM out of sample, so we conclude that the model has a high predictive power.



############### OLD #################
per_constructs_factors1 <- factors_list%>%
  select(category_1,constructs, column_name_new,constructs, constructs_type,weights)%>%
  filter(!(constructs %in% per_reflective_consctructs_remove))

per_data_analysis1<- per_data_analysis%>%
  select(dfs_adoption_binary,any_of(per_constructs_factors1$column_name_new))%>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
dim(per_data_analysis1)#[1] 200   196
str(per_data_analysis1)


measurement_model1<- build_measurement_model(
  data_analysis=per_data_analysis1,
  constructs_variables= per_constructs_factors1 )
measurement_model1

direct_effect1<- purrr::map_chr(measurement_model1, ~ .[1]) %>%
  unique() %>%
  setdiff("dfs_adoption_binary")%>%
  setdiff("dfs_adoption_area")
direct_effect1

structural_model1 <-relationships(
  paths(from= c(direct_effect1), to="dfs_adoption_binary"),
  paths(from=c("on_farm_income","non_farm_income"), to="income"))


structural_model1

#########################################################
########## Estimating the PLS-SEM model ====
#########################################################
pls_sem_model1 <- estimate_pls(data = per_data_analysis1,
                               measurement_model = measurement_model1,
                               structural_model = structural_model1)

# Summarize the model results
pls_sem_summary1<- summary(pls_sem_model1)

#########################################################
########## Bootstrapping the model ====
#########################################################
boot_model1 <- bootstrap_model(seminr_model = pls_sem_model1, 
                               nboot = 1000, 
                               cores = 2 ) 
# Store the summary of the bootstrapped model 
boot_model_summary1 <- summary(boot_model,alpha = 0.10)
boot_model_summary1$validity
plot(boot_model, title = "")

#########################################################
########## Assessing the reflective measurement models AGAIN ====
#########################################################






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





