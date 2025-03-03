library(dplyr)
library(readxl)
library(caret)
library(reshape2)


factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")
per_data_clean<- read.csv("per_data_clean.csv",sep=",")
per_summary_categorical<-read.csv("per_summary_categorical.csv",sep=",")
per_summary_numerical<-read.csv("per_summary_numerical.csv",sep=",")

sort(unique(per_data_clean$temperature_change_perception))


#### Select the factors that were listed as important for adoption according to:
# - data availability
# - systematic evidence map

per_variables_list<-c(unique(per_summary_categorical$column_name_new2))#,unique(per_summary_numerical$column_name_new))
per_variables_list

per_data_analysis<- per_data_clean%>%
  dplyr::select(all_of(per_variables_list))

dim(per_data_analysis) #[1] 200 109 #200 farmers; 109 variables evaluated


#############################################################    
########## DATA TYPE CONVERSION #####-----
#############################################################

###### --- CATEGORICAL AND BINARY VARIABLES -----
#### Convert categorical and binary to factor
columns_categorical_nominal <- intersect(per_summary_categorical$column_name_new2, colnames(per_data_analysis))

print(columns_categorical_nominal)  # Check if it holds expected values

per_data_analysis<- per_data_analysis%>%
  mutate(across(all_of(columns_categorical_nominal), as.factor))
sort(unique(per_data_analysis$dfs_adoption_binary))

###### --- NUMERICAL VARIABLES -----
#### Convert continuous variables to numeric
columns_continuous <- intersect(per_summary_numerical$column_name_new, colnames(per_data_analysis))
print(columns_continuous)  # Check if it holds expected values

per_data_analysis<- per_data_analysis%>%
  mutate(across(all_of(columns_continuous), as.numeric))

#############################################################    
############# DISCRETIZATION -----
#############################################################

#############################################################    
############# FEATURE SELECTION -----
#############################################################

###### --- ZERO AND NEAR ZERO VARIANCE PREDICTORS -----
#In some situations, the data generating mechanism can create predictors that only have a 
#single unique value (i.e. a “zero-variance predictor”). For many models (excluding tree-based models),
#this may cause the model to crash or the fit to be unstable.
#Similarly, predictors might have only a handful of unique values that occur with very low frequencies.
## frequency ratio: would be near one for well-behaved predictors and very large for highly-unbalanced data.
## percent of unique values: is the number of unique values divided by the total number of samples (times 100)
#that approaches zero as the granularity of the data increases

nzv <- nearZeroVar(per_data_analysis, saveMetrics= TRUE) 
nzv # the variables with nzv== TRUE should be remove

nzv_list <- nearZeroVar(per_data_analysis)

#### Create the ggplot
long_data <- melt(per_data_analysis, id.vars = "dfs_adoption_binary", measure.vars = nzv_list)

ggplot(long_data, aes(x = value, fill = dfs_adoption_binary)) +
  geom_bar(position = "stack") +
  facet_wrap(~variable, scales = "free", ncol = 6) + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

problems<- per_data_analysis[, nzv_list]

#### Remove nzv variables from data
per_data_analysis_Filterednzv<- per_data_analysis[, -nzv_list]

dim(per_data_analysis_Filterednzv) #[1] 200 75 #200 farmers; 75 variables retained

###### only for continuous variables...
###### --- CORRRELATED PREDICTORS -----


descrCor <-  cor(per_data_analysis_Filterednzv)

###### ---  identify linear dependencies
x<-findLinearCombos(per_data_analysis_Filterednzv)

###### --- IMPUTING MISSING DATA ----
na_columns <- colnames(per_data_analysis_Filterednzv)[colSums(is.na(per_data_analysis_Filterednzv)) > 0]
print(na_columns)

#[1] "energy_cooking_type" FIRST CHECH WHAT IS THE PROBLEM HERE

problems2<- per_data_analysis_Filterednzv%>%
  select(all_of(na_columns))

###### --- FBED -----
library(MXM)
dataset_name<-per_data_analysis_Filterednzv
bc_dataset <- per_data_analysis_Filterednzv[ , !(names(per_data_analysis_Filterednzv) %in% c("dfs_adoption_binary"))] # Remove target from feature matrix
bc_target <- as.factor(per_data_analysis_Filterednzv$dfs_adoption_binary)

alphas <- c(0.001, 0.005, 0.01, 0.05, 0.1) #significance level α of the conditional independence test
test<- "testIndLogistic" # conditional independence test for outcome = binary, predictors= mixed





cv.fbed.reg(bc_target, bc_dataset, #id, kfolds = 10, 
                  folds = NULL, alphas = c(0.01, 0.05), ks = 0:2) 

help(cv.fbed.lmm.reg)

results <- lapply(alphas, function(thresh) {
  fbed.reg(
    target = bc_target,
    dataset = bc_dataset,
    test =test, # 
    K = 1:10,
    threshold = thresh,  # Setting alpha as the significance threshold
    backward= TRUE
  )
})

results$[[1]]$res

num_selected<-length(results$selectedVars)


fbed.reg<- fbed.reg(target= bc_target,
                    dataset= bc_dataset,
                    test= "testIndLogistic",
                    K=0:10,
                    threshold= 0.05,
                    )
         #, wei,  method, gam, )
fbed.reg
fbed.reg$res$univ
fbed.reg$mod
fbed.reg$res
cv.fbed.reg
fbed.reg$best_performance
fbed.reg$best_configuration

plot(fbed.reg, mode = "all")
fbed.reg
fbed.reg$res$info
###### --- RECURSIVE FEATURE SELECTION -----
library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)
### Advantages of Recursive Feature Elimination (RFE)
# - Can handle high-dimensional datasets and identify the most important features.
# - Can handle interactions between features and is suitable for complex datasets.
# - Can be used with any supervised learning algorithm.
### Limitations of Recursive Feature Elimination (RFE)
# - Can be computationally expensive for large datasets.
# - May not be the best approach for datasets with many correlated features.
# - May not work well with noisy or irrelevant features.

dataset_name<-per_data_analysis_Filterednzv%>%
  select(-energy_cooking_type,-farmer_agency_1,-farmer_agency_3 ,
         -dfs_agroforestry_adoption, -dfs_homegarden_adoption, -dfs_intercropping_adoption)

predictors <- dataset_name[ , !(names(dataset_name) %in% c("dfs_adoption_binary"))] # Remove target from feature matrix
target <- as.factor(dataset_name$dfs_adoption_binary)
ncolPredictors<-ncol(predictors)


set.seed(123)  # For reproducibility

# Define control parameters for RFE
rfctrl <- rfeControl(
  functions = rfFuncs,     # Use Random Forest for feature selection
  method = "repeatedcv",   # Use repeated k-fold cross-validation
  number = 10,             # 10-fold cross-validation
  repeats = 5,             # Repeat X times for stability
  verbose = TRUE
)


set.seed(123)  # Ensure reproducibility

# Run Recursive Feature Elimination
rfe_result <- rfe(
  x = predictors, 
  y = target, 
  sizes = c(1:ncolPredictors ),  # Number of features to evaluate
  rfeControl = rfctrl
)

# View the results
print(rfe_result)
predictors(rfe_result)
rfe_result$fit
head(rfe_result$resample)
print(rfe_result$metric) #Accuracy if target is categorical


# Plot variable selection results
plot(rfe_result, type = c("g", "o"))  # Graphical + Overlayed plots
trellis.par.set(caretTheme())
plot(rfe_result, type = c("g", "o"))

# Extract the best set of features selected by RFE
best_features <- predictors(rfe_result)
best_features






