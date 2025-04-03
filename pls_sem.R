library(dplyr)
library(readxl)

#############################################################    
########## UPLOAD DATA #####-----
#############################################################
per_data0<- read.csv("per_data0.csv",sep=",")#all correlated factors
per_data1<- read.csv("per_data1.csv",sep=",") #data1
per_data2<- read.csv("per_data2.csv",sep=",") #data2

per_outcomes<-read_excel("factors_list.xlsx",sheet = "factors_list")%>%
  filter(category_1=="outcome")
per_outcomes<-per_outcomes$column_name_new

per_data0_selected_factors<- read.csv("per_data0_selected_factors.csv",sep=",")# selected factors from all correlated factors
per_data1_selected_factors<- read.csv("per_data1_selected_factors.csv",sep=",")#selected factors from data1
per_data2_selected_factors<- read.csv("per_data2_selected_factors.csv",sep=",")#selected factors from data2

#############################################################    
########## SELECTED FACTORS #####-----
#############################################################
per_data0_analysis<- per_data0%>%
  select(dfs_adoption_binary,any_of(per_data0_selected_factors$selected_factors))%>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

dim(per_data0_analysis) #[1] 200   5
str(per_data0_analysis)

per_data1_analysis<- per_data1%>%
  select(dfs_adoption_binary, any_of(per_data1_selected_factors$selected_factors))%>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))
dim(per_data1_analysis)#[1] 200   36
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

# Get column names
variables <- colnames(per_data1_analysis)

# Build measurement model automatically
measurements <- rlang::exec(
  seminr::constructs,
  !!!map(variables, ~ seminr::composite(.x, seminr::single_item(.x)))
)

measurements

per_factors1 <- per_data1_analysis %>% select(-any_of(per_outcomes))
per_factors1<-colnames(per_factors1)
per_factors1

structure <- relationships(
  paths(from = c(per_factors1), to = "dfs_adoption_binary")
  #paths(from = c("farmer_agency_1","membership"), to = "farm_size")
  )

structure

pls_model <- estimate_pls(data = per_data1_analysis, 
                          measurement_model = measurements, 
                          structural_model = structure)
summary(pls_model)
model_summary <- summary(pls_model)

# Item Reliability
model_summary$loadings #item reliability

#internal consistency 
model_summary$reliability #Cronbach's alpha, Composite reliability rhoC, Dillon-Goldstein's rhoA

#convergent validity
model_summary$reliability #AVE


#Discriminant Validity
model_summary$validity$htmt #Heterotrait–Monotrait ratio of correlations → Should be < 0.90 (or 0.85 for strict thresholds)
model_summary$validity$fl_criteria #Fornell–Larcker Criterion → Square roots of AVE (diagonals) should be greater than inter-construct correlations (off-diagonals).
model_summary$validity$cross_loadings #Ensures that items load highest on their intended construct.

#model_summary$reliability reports composite reliability (rhoC), cronbachs alpha, average variance extracted (AVE), and rhoA
model_summary$validity$cross_loadings



# note: PLS requires seperate bootstrapping for PLS path estimates
# SEMinR uses multi-core parallel processing to speed up bootstrapping
boot_estimates <- bootstrap_model(pls_model, nboot = 1000, cores = 2)
summary(boot_estimates)
plot(boot_estimates, title = "Bootstrapped Model")


specific_effect_significance(
  boot_seminr_model = boot_estimates,
  from = "farmer_agency_1",
  #through = "farm_size",
  to = "dfs_adoption_binary"
)

specific_effect_significance(
  boot_seminr_model = boot_estimates,
  from = "membership",
  through = "farm_size",
  to = "dfs_adoption_binary"
)



# Alternatively, we could estimate our model using CBSEM, which uses the Lavaan package
# We often wish to conduct a CFA of our measurement model prior to CBSEM
# note: we must convert composites in our measurement model into reflective constructs for CFA/CBSEM
cfa_model <- estimate_cfa(data = per_data0_analysis, as.reflective(measurements))
summary(cfa_model)
