library(dplyr)
library(readxl)
library(caret)
library(reshape2)
library(summarytools)
library(bnlearn)
library(corrplot)


factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")%>%
  filter(is.na(remove))
per_data_clean<- read.csv("per_data_clean.csv",sep=",")
per_summary_categorical<-read.csv("per_summary_categorical.csv",sep=",")
per_summary_numerical<-read.csv("per_summary_numerical.csv",sep=",")


#### Select the factors that were listed as important for adoption according to:
# - data availability
# - systematic evidence map
# - meta-analysis
# - Dessart et al 2018
# - Context document (Peru)

per_variables_list<-c(unique(per_summary_categorical$column_name_new2),unique(per_summary_numerical$column_name_new))
per_variables_list

per_data_analysis<- per_data_clean%>%
  dplyr::select(all_of(per_variables_list))

dim(per_data_analysis) #[1] 200 323 #200 farmers; 323 variables evaluated

#############################################################    
########## DATA TYPE CONVERSION #####-----
#############################################################

###### --- CATEGORICAL AND BINARY VARIABLES -----
#### Convert categorical and binary to factor
columns_categorical <- intersect(per_summary_categorical$column_name_new2, colnames(per_data_analysis))

print(columns_categorical)  # Check if it holds expected values

per_data_analysis<- per_data_analysis%>%
  mutate(across(all_of(columns_categorical), as.factor))
sort(unique(per_data_analysis$dfs_adoption_binary))

###### --- CATEGORICAL ORDINAL AND BINARY -----
#### Convert categorical and binary to factor
#columns_categorical_nominal <- intersect(per_summary_categorical$column_name_new2[per_summary_categorical$categorical_type!="nominal" ], colnames(per_data_analysis))
#print(columns_categorical_nominal)  # Check if it holds expected values

#per_data_analysis<- per_data_analysis%>%
# mutate(across(all_of(columns_categorical_nominal), as.factor))
#sort(unique(per_data_analysis$dfs_adoption_binary))
#str(per_data_analysis)
###### --- CATEGORICAL NOMINAL -----
#### Convert categorical and binary to factor
#columns_categorical_nominal <- intersect(per_summary_categorical$column_name_new2[per_summary_categorical$categorical_type=="nominal" ], colnames(per_data_analysis))
#print(columns_categorical_nominal)  # Check if it holds expected values

#per_data_analysis<- per_data_analysis%>%
#  mutate(across(all_of(columns_categorical_nominal), as.factor))
#sort(unique(per_data_analysis$dfs_adoption_binary))

###### --- NUMERICAL VARIABLES -----
#### Convert continuous variables to numeric
columns_continuous <- intersect(per_summary_numerical$column_name_new, colnames(per_data_analysis))
print(columns_continuous)  # Check if it holds expected values

per_data_analysis<- per_data_analysis%>%
  mutate(across(all_of(columns_continuous), as.numeric))

#############################################################    
############# CREATING DUMMY VARIABLES -----
#############################################################
### Only for categorical nominal...
columns_categorical_nominal <- intersect(per_summary_categorical$column_name_new2[per_summary_categorical$categorical_type=="nominal" ], colnames(per_data_analysis))
print(columns_categorical_nominal)  # Check if it holds expected values
formula <- as.formula(paste("~", paste(columns_categorical_nominal, collapse = " + ")))

dummies <- dummyVars(formula, data = per_data_analysis)

dummy_names <- colnames(predict(dummies, newdata = per_data_analysis))
print(dummy_names)

# Get the factor names and count their corresponding dummy columns
dummy_count <- sapply(columns_categorical_nominal, function(col) {
  sum(grepl(paste0("^", col, "\\."), dummy_names))
})
print(dummy_count)

binary_factors <- names(dummy_count[dummy_count == 2])
print(binary_factors)

dummies <- predict(dummies, newdata = per_data_analysis)%>%
  as.data.frame() 

per_data_Binary <- per_data_analysis%>%
  cbind( dummies)%>%
  select(-all_of(columns_categorical_nominal))


cols_to_remove <- sapply(binary_factors, function(col) {
  # Get the two dummy columns for the binary factor
  cols <- grep(paste0("^", col, "\\."), dummy_names, value = TRUE)
  
  if (length(cols) == 2) {
    # Count the number of "1"s in each column
    count_1_col1 <- sum(per_data_Binary[[cols[1]]] == 1, na.rm = TRUE)
    count_1_col2 <- sum(per_data_Binary[[cols[2]]] == 1, na.rm = TRUE)
    
    # Remove the column with fewer "1"s
    if (count_1_col1 > count_1_col2) {
      return(cols[2])  # Remove the second column if the first has more "1"s
    } else {
      return(cols[1])  # Remove the first column if the second has more "1"s
    }
  } else {
    return(NA)
  }
})
cols_to_remove

per_data_Binary <- per_data_Binary %>%
  select(-all_of(cols_to_remove))


dim(per_data_Binary) #[1] 200 337 #200 farmers; 337 variables evaluated

#############################################################    
#############################################################
a<-as.data.frame(c(colnames(per_data_Binary)))%>%
  rename("column_name_new"="c(colnames(per_data_Binary))")%>%
  left_join(factors_list%>%select(category_1,column_name_new), by="column_name_new")%>%
  group_by(category_1) %>%
  mutate(column_name_new_count = n()) %>%
  tally()%>%filter(category_1!="xxx")

ggplot(data=a, aes(x=n, y=category_1, fill= category_1)) +
  geom_bar(stat="identity")+
  geom_text(aes(label = n), 
            hjust = -0.2, # Adjust position of the label to the right of the bar
            size = 4) +
  theme_minimal() +
  labs(x = "Number of factors", y = "Category") +
  theme(legend.position = "none")

dim(per_data_Binary) #[1] 200 337 #200 farmers; 337 variables retained


#############################################################    
############# ZERO AND NEAR ZERO VARIANCE PREDICTORS -----
#############################################################
#In some situations, the data generating mechanism can create predictors that only have a 
#single unique value (i.e. a “zero-variance predictor”). For many models (excluding tree-based models),
#this may cause the model to crash or the fit to be unstable.
#Similarly, predictors might have only a handful of unique values that occur with very low frequencies.
## frequency ratio: would be near one for well-behaved predictors and very large for highly-unbalanced data.
## percent of unique values: is the number of unique values divided by the total number of samples (times 100)
#that approaches zero as the granularity of the data increases
nzv <- nearZeroVar(per_data_Binary, saveMetrics= TRUE) 
nzv # the variables with nzv== TRUE should be remove

nzv_list <- nearZeroVar(per_data_Binary)

nzv_factors<- per_data_Binary[, nzv_list]
print(nzv_factors)
view(dfSummary(nzv_factors))

## Remove nzv variables from data
per_data_analysis_Filterednzv<- per_data_Binary[, -nzv_list]

dim(per_data_analysis_Filterednzv) #[1] 200 237 #200 farmers; 237 variables retained

b<-as.data.frame(c(colnames(per_data_analysis_Filterednzv)))%>%
  rename("column_name_new"="c(colnames(per_data_analysis_Filterednzv))")%>%
  left_join(factors_list%>%select(category_1,column_name_new), by="column_name_new")%>%
  group_by(category_1) %>%
  mutate(column_name_new_count = n()) %>%
  tally()%>%filter(category_1!="xxx")

ggplot(data=b, aes(x=n, y=category_1, fill= category_1)) +
  geom_bar(stat="identity")+
  geom_text(aes(label = n), 
            hjust = -0.2, # Adjust position of the label to the right of the bar
            size = 4) +
  theme_minimal() +
  labs(x = "Number of factors", y = "Category") +
  theme(legend.position = "none")

dim(per_data_analysis_Filterednzv) #[1] 200 235 #200 farmers; 235 variables retained

#############################################################    
############# IDENTIFYING CORRELATED FACTORS -----
#############################################################
###### --- Pearson Correlation -----
#only for continuous variables
columns_continuous <- intersect(per_summary_numerical$column_name_new, colnames(per_data_analysis_Filterednzv))
print(columns_continuous)
per_data_analysis_cont<- per_data_analysis_Filterednzv%>%
  select(all_of(columns_continuous))
per_data_analysis_cont

pearson.cor<- dedup(per_data_analysis_cont, threshold= 0.8,    debug = TRUE)
pearson.cor
#income_amount_onfarm is collinear with income_amount_total, dropping income_amount_total with COR = 0.9924
#sfp_total_area is collinear with dfs_total_area, dropping dfs_total_area with COR = 0.9922
#distance_main_road is collinear with distance_public_transport, dropping distance_public_transport with COR = 0.9160
#num_adults_total is collinear with num_adults_wa, dropping num_adults_wa with COR = 0.8298
#sfs_monoculture_perennial_area is collinear with total_main_crops_cropland_area, dropping total_main_crops_cropland_area with COR = 0.8077
#sfs_monoculture_perennial_area is collinear with cropland_area, dropping cropland_area with COR = 0.8940
#land_tenure_own_area is collinear with male_land_tenure_own_area, dropping male_land_tenure_own_area with COR = 0.8764
#land_tenure_own_area is collinear with female_land_tenure_own_area, dropping female_land_tenure_own_area with COR = 0.8405
#male_land_tenure_own_proportion is collinear with female_land_tenure_own_proportion, dropping female_land_tenure_own_proportion with COR = -0.8777
pearson.highCor_factors <- setdiff(colnames(per_data_analysis_cont), colnames(pearson.cor))
pearson.highCor_factors 

colnames(pearson.cor)
#9 removed factors
#[1] "income_amount_total"               "num_adults_wa"                     "total_main_crops_cropland_area"    "cropland_area"                    
#[5] "dfs_total_area"                    "male_land_tenure_own_area"         "female_land_tenure_own_area"       "female_land_tenure_own_proportion"
#[9] "distance_public_transport"        

pearson.cor_matrix <- cor(per_data_analysis_cont, method = "pearson", use = "pairwise.complete.obs")
pearson.cor_matrix[abs(pearson.cor_matrix) < 0.8] <- NA
diag(pearson.cor_matrix) <- NA
person.highCor_matrix <- pearson.cor_matrix[rowSums(!is.na(pearson.cor_matrix)) > 0, 
                                            colSums(!is.na(pearson.cor_matrix)) > 0]
diag(person.highCor_matrix) <- 1
person.highCor_matrix

corrplot(person.highCor_matrix, method = "color", type = "upper", tl.cex = 0.7, tl.col = "black", na.label = " ")

str(pearson.highCor_factors)
per_data_analysis_FilteredPerCor <- per_data_analysis_Filterednzv[, setdiff(names(per_data_analysis_Filterednzv), pearson.highCor_factors)]

c<-as.data.frame(c(colnames(per_data_analysis_FilteredPerCor)))%>%
  rename("column_name_new"="c(colnames(per_data_analysis_FilteredPerCor))")%>%
  left_join(factors_list%>%select(category_1,column_name_new), by="column_name_new")%>%
  group_by(category_1) %>%
  mutate(column_name_new_count = n()) %>%
  tally()%>%filter(category_1!="xxx")

ggplot(data=c, aes(x=n, y=category_1, fill= category_1)) +
  geom_bar(stat="identity")+
  geom_text(aes(label = n), 
            hjust = -0.2, # Adjust position of the label to the right of the bar
            size = 4) +
  theme_minimal() +
  labs(x = "Number of factors", y = "Category") +
  theme(legend.position = "none")

dim(per_data_analysis_FilteredPerCor) #[1] 200 241 #200 farmers; 241 variables retained

###### --- Spearman Correlation -----
# Numerical and categorical ordinal variables
str(per_data_analysis_Filterednzv)
per_data_analysis_numeric<- per_data_analysis_Filterednzv%>%
  mutate(across(all_of(colnames(.)), as.numeric))

spearman.cor <- cor(per_data_analysis_numeric, method = "spearman", use = "pairwise.complete.obs")
high_corr_ord <- findCorrelation(spearman.cor, cutoff = 0.8)

spearman.highCor_factors <- colnames(per_data_analysis_numeric)[high_corr_ord]
print(spearman.highCor_factors)
#39 Factors with high Spearman correlation

spearman.highCor_matrix <- spearman.cor[spearman.highCor_factors, spearman.highCor_factors]
spearman.highCor_matrix
spearman.highCor_matrix[abs(spearman.highCor_matrix) >= 0.8] <- NA
diag(spearman.highCor_matrix) <- NA
spearman.highCor_matrix <- spearman.highCor_matrix[rowSums(!is.na(spearman.highCor_matrix)) > 0, 
                                            colSums(!is.na(spearman.highCor_matrix)) > 0]
diag(spearman.highCor_matrix) <- 1
spearman.highCor_matrix


corrplot(spearman.highCor_matrix, method = "color", type = "upper", tl.cex = 0.7, tl.col = "black")

# Find row and column indices of high correlation pairs
spearman.highCor_factors_pairs <- as.data.frame(which(abs(spearman.cor) >= 0.8, arr.ind = TRUE))%>%
  filter(row != col) %>%
  mutate(factor1 = rownames(spearman.cor)[row],
         factor2 = rownames(spearman.cor)[col]) %>%
  select(factor1, factor2) %>%
  distinct()%>%
  rowwise() %>%
  mutate(factor1 = min(factor1, factor2),
         factor2 = max(factor1, factor2)) %>%
  ungroup() %>%
  distinct()%>%
  filter(factor1 != factor2)%>%
  left_join(factors_list%>%select(category_1,column_name_new),by=c("factor1"="column_name_new"))%>%
  left_join(factors_list%>%select(category_1,column_name_new),by=c("factor2"="column_name_new"))%>%
  mutate(correlated= ifelse(category_1.x==category_1.y,"yes","no"))

sort(unique(spearman.highCor_factors_pairs$factor1))
sort(unique(spearman.highCor_factors_pairs$factor2))
#REMOVE 1
remove1<-data.frame(data1=c(
  "cropland_area", #"sfs_monoculture_perennial_area"
  "income_amount_onfarm", #"income_amount_total"
  "sfp_total_area"
))

highCor1<-read_excel("factors_list.xlsx",sheet="high_correlated")%>%
  select(data1)%>%
  rbind(remove1)

#not remove 1
#"income_sources.livestock"         
#"influence_nr_frequency"            "land_tenure_hold_status"           "livestock_count_tlu"               "livestock_exotic_local"           
# "livestockland_area"                                                               
# "num_sales_channel_livestock"             "sfs_monoculture_annual_adoption"  
#"accessibility_waste_collection"
#"credit_access"
#"crop_type.cacao"                   "crop_type.camucamu"                "crop_type.frutales"     
#"dfs_adoption_binary" "dfs_total_area" 
#"district.dist_1"                   "district.dist_3" 
#"fair_price_livestock"              "farm_products.Livestock"           "farm_size"                         "full_time_farmer"  

#REMOVE 2
remove2<-data.frame(data2=c(
   "sfs_monoculture_perennial_area", #"cropland_area",
   "income_amount_total",#"income_amount_onfarm"
   "sfp_total_area"
))

highCor2<-read_excel("factors_list.xlsx",sheet="high_correlated")%>%
  select(data2)%>%
  rbind(remove2)

#############################################################    
############# CENTERING AND SCALING -----
#############################################################
#Only for continuous variables
columns_continuous <- intersect(per_summary_numerical$column_name_new, colnames(per_data_analysis_Filterednzv))
print(columns_continuous)
per_data_analysis_cont<- per_data_analysis_Filterednzv%>%
  select(all_of(columns_continuous))

set.seed(96)

# Define the preprocessing function
preProcessParams <- preProcess(per_data_analysis_cont, method = c("center", "scale"))

# Apply centering and scaling
data_scaled <- predict(preProcessParams, per_data_analysis_cont)

# Scale to have standard deviation of 0.5
data_scaled <- data_scaled * 0.5

# Check results
summary(data_scaled)


per_data_analysis_CenteredScaled <- per_data_analysis_Filterednzv[, setdiff(names(per_data_analysis_Filterednzv), data_scaled)]

dim(per_data_analysis_CenteredScaled) #[1] 200 248 #200 farmers; 248 variables retained
names(per_data_analysis_CenteredScaled)
############################################################# 
adoption_binary <- per_data_analysis_CenteredScaled$dfs_adoption_binary

factors_binary <- per_data_analysis_CenteredScaled[ , !(names(per_data_analysis_CenteredScaled) %in% c("dfs_adoption_binary"))] # Remove target from feature matrix
factors_binary <- as.matrix(do.call(cbind, factors_binary))
factors_binary <- as.data.frame(factors_binary)%>%
  select(-dfs_total_area)%>%
  select(-all_of(unique(highCor1$data1)))%>%
  select(-"dfs_agroforestry_adoption",
         -"dfs_homegarden_adoption"  ,       
          -"dfs_intercropping_adoption")

# Create a data frame with target and predictors
data <- data.frame(target = adoption_binary, factors_binary)

############################################################# 
library(BayesSubsets)
library(rstanarm)

# Convert to matrix or data frame (depending on the structure of 'factors')
#factors <- as.matrix(do.call(cbind, factors)) # If all numeric


# Fit Bayesian logistic model with horseshoe prior
set.seed(123) 
fit <- stan_glm(target ~ ., 
                data = data, 
                family = binomial(link = "logit"), 
                prior = hs(),
                chains = 4,
                iter = 4000,
                adapt_delta = 0.99)
                
                #QR=TRUE)

                 # Total number of MCMC iterations
                warmup = 5000) 



summarySingleLevelModel <- summary(fit) 
print(summarySingleLevelModel)

# Extract the posterior predictive draws and lpd:
temp = post_predict(post_y_hat = tcrossprod(fit$beta, X),
                    post_sigma = fit$sigma,
                    yy = y)
post_y_pred = temp$post_y_pred
post_lpd = temp$post_lpd




fit <- stan_glm(dfs_adoption_binary ~ ., 
                data= per_data_analysis_CenteredScaled,
                family = binomial(link = "logit"), #logistic regression model where the log-odds are modeled as a linear function of the predictors
                prior = hs(df = 1, global_scale = 0.1), # Horseshoe prior
                chains = 4, 
                iter = 15000)   # 15,000 iterations to save 10,000 post warm-up
                warmup = 5000,  # Burn-in period
                QR = TRUE)      # Helps handle rank-deficiency (p > n)
fit
data = mtcars
str(data)

x<-mpg / 10

fit <- stan_glm(mpg / 10 ~ ., data = mtcars, QR = TRUE,
                algorithm = "fullrank")



per_data_analysis_FilteredSperCor <- per_data_analysis_FilteredPerCor[, high_corr_ord]
print(per_data_analysis_FilteredSperCor)
view(dfSummary(per_data_analysis_FilteredSperCor))




final_data <- per_data_analysis_FilteredCorr[, 
                                             setdiff(colnames(per_data_analysis_FilteredCorr), 
                                                     c(pearson.highCor_factors, spearman.highCor_factors))]
dim(final_data) #[1] 200 241 #200 farmers; 241 variables retained


###### --- SES and MMPC -----
library(MXM)
library("glmnet")
library("hash")
library("MASS")
library("VGAM")
library("stats")
library("ROCR")
library("Hmisc")
library("TunePareto")
library("methods")
library("utils")
library("parallel")
library("survival")




bc_dataset <- data[, -which(names(data) == "target")] # Remove target from feature matrix
bc_target <- as.factor(data$target)

ses <- SES(
  target = bc_target,  # Reference as column in the data
  dataset = bc_dataset, 
  max_k = 5, 
  threshold = 0.2, 
  test = "testIndLogistic",
  hash = TRUE,
  hashObject = NULL
)
ses@selectedVars
ses@signatures
z<- data[, ses@selectedVars]
names(z)
#[1] "sfs_monoculture_annual_adoption"           "support_provider.community_leaders"        "support_provider_governmental_institution" "vegetation_diversity_wetland"             
#[5] "soil_pH_mean"                              "num_main_crops_herb"                       "num_nhlabour_permanent_total"              "household_shock_strategy_count" 

hashObj <- ses@hashObject
hashObj

ses2 <- SES(bc_target, bc_dataset, max_k = 2, threshold = 0.01, 
            test = "testIndLogistic",
            hash = TRUE, hashObject = hashObj) 
ses2
ses2@selectedVars
ses2@signatures

z2<- data[, ses2@selectedVars]
names(z2)
plot(ses2, mode = "all")

cv.ses.object <- cv.ses(bc_target, bc_dataset, kfolds = 10, task = "C",
                        alphas = c(0.1, 0.05, 0.01),
                        max_ks = c(2:5))



glm.mxm(train_target, sign_data, sign_test, wei = NULL)
 
 


############
EXP <- function(dataset_name, dataset, target, test = NULL, task = "C", metric = NULL, modeler = NULL, lasso_metric = NULL, lasso_modeler = NULL) {
  
  ## task = "C" for classification, "R" for regresion, "S" for Survival
  ## Choose model and metric functions due to task for SES and LASSO
  if (task == "C") {
    
    ## Classification task (logistic regression)
    if (is.null(metric)) {
      metricFunction <- auc.mxm
    } else {
      metricFunction <- metric
    }
    
    if (is.null(modeler)) {
      modelerFunction <- glm.mxm
    } else {
      modelerFunction <- modeler
    }
    
    if (is.null(test)) {
      test <- "testIndLogistic"
    } else {
      test <- test
    }
    
  } else if (task == "R") {
    
    ## Regression task (linear regression)
    if (is.null(metric)) {
      metricFunction <- mse.mxm
    } else {
      metricFunction <- metric
    }
    
    if (is.null(modeler)) {
      modelerFunction <- lm.mxm
    } else {
      modelerFunction <- modeler
    }
    
    if (is.null(test)) {
      test <- "testIndFisher"
    } else {
      test <- test
    }
    
  } else if (task == "S") {
    
    #cox survival analysis (cox regression)
    if (is.null(metric)) {
      metricFunction <- ci.mxm
    } else {
      metricFunction <- metric
    }
    
    if (is.null(modeler)) {
      modelerFunction <- coxph.mxm
    } else {
      modelerFunction <- modeler
    }
    
    if (is.null(test)) {
      test <- "censIndLR"
    } else {
      test <- test
    }
    
  } else {
    stop("Please provide a valid task argument 'C'-classification, 'R'-regression, 'S'-survival.")
  }
  
  ## SES configurations
  alphas <- c(0.1, 0.05, 0.01) 
  max_ks <- c(2:5) 
  
  ##############################################################
  # 1. partition the dataset to 50% training and 50% hold-out. #
  ##############################################################
  
  k <- 10
  
  ## stratified cross validation
  if (survival::is.Surv(target)) {
    folds <- TunePareto::generateCVRuns(target[,2], ntimes = 1, nfold = k, leaveOneOut = FALSE, stratified = TRUE)
  } else {
    folds <- TunePareto::generateCVRuns(target, ntimes = 1, nfold = k, leaveOneOut = FALSE, stratified = TRUE)
  }
  #create train instances vector
  train_samples <- c()
  for (i in which(c(1:k) != k)) {
    train_samples <- c(train_samples, folds[[1]][[i]])
  }
  train_set <- dataset[train_samples,]
  train_target <- target[train_samples]
  hold_out <- dataset[folds[[1]][[k]],]
  hold_out_target <- target[folds[[1]][[k]]]
  
  #create the 10 folds on the training set for the cross validation run
  if (survival::is.Surv(target))
  {
    cv_folds <- TunePareto::generateCVRuns(train_target[,2], ntimes = 1, nfold = 10, leaveOneOut = FALSE, stratified = TRUE)
  }else
  {
    cv_folds <- TunePareto::generateCVRuns(train_target, ntimes = 1, nfold = 10, leaveOneOut = FALSE, stratified = TRUE)
  }
  
  ########################################################################
  # 2. Cross validation on the training to get the best hyper-parameters #
  # for SES and LASSO with the cv.ses function                           #
  ########################################################################
  
  cv_time <- system.time({
    best_model <- cv.ses(target = train_target, dataset = train_set, kfolds = 10, folds = cv_folds[[1]], alphas = alphas, max_ks = max_ks, task = task,
                         metric = metric, modeler = modeler, ses_test = test)
  })
  best_a <- best_model$best_configuration$a
  best_max_k <- best_model$best_configuration$max_k
  
  ## save(best_model, file = "safety.RData")
  
  ## get the set of multiple signatures on the training set
  SES_res <- SES(train_target, train_set, max_k = best_max_k, threshold=best_a, test = test)
  summary(SES_res)
  ref_signature <- SES_res@selectedVars
  signatures <- as.matrix(SES_res@signatures)
  
  ## save(SES_res, file = "safety.RData")
  
  ############################################################################
  # 3. Train a model for each signature on the training data using the best  #
  # hyper-parameters from step 2.                                            #
  ############################################################################
  
  SES_performance <- vector("numeric", dim(signatures)[1])
  SES_preds <- vector("list", dim(signatures)[1])
  
  for (i in 1:dim(signatures)[1])
  {
    #if (any(is.na(signatures[i,])) == TRUE)
    cur_sign = signatures[i,]
    sign_data = matrix(nrow = dim(train_set)[1] , ncol = dim(signatures)[2])
    sign_test = matrix(nrow = dim(hold_out)[1] , ncol = dim(signatures)[2])
    for (j in 1:length(cur_sign))
    {
      sign_data[,j] = train_set[,cur_sign[j]]
      sign_test[,j] = hold_out[,cur_sign[j]]
    }
    
    #regression task (linear model)
    preds<-modelerFunction(train_target, sign_data, sign_test, wei = NULL)
    print(class(preds)) 
    str(preds)
    
    performance = metricFunction(preds$preds, hold_out_target)
    
    SES_performance[i] = performance
    SES_preds[[i]] = preds
  }
  
  #the result list to be returned
  RES <- NULL
  #RES$dataset_name = dataset_name
  RES$task = task
  RES$test = test
  RES$folds = folds
  RES$cv_folds = cv_folds
  RES$best_model = best_model
  RES$cv_time = cv_time
  
  RES$SES_res = SES_res
  RES$signatures = signatures
  RES$SES_performance = SES_performance #all the signature perfs on the holdout
  #RES$SES_preds = SES_preds #all the predictions for each signature
  
  return(RES)
}

res <- EXP("dataset_name", bc_dataset, bc_target, test = "testIndLogistic", task = "C")
res

# set up
rm(list = ls())
time <- proc.time()
repetitions <- 500
nCores <- 3


#######################
cv.ses.object



summary(sesObject2)

summary(ses2)

bc_dataset <- per_data_analysis_Filterednzv[ , !(names(per_data_analysis_Filterednzv) %in% c("dfs_adoption_binary"))] # Remove target from feature matrix
bc_target <- as.factor(per_data_analysis_Filterednzv$dfs_adoption_binary)

mmmb.testIndLogistic <- mmmb(
  target = bc_target,  # Reference as column in the data
  dataset = bc_dataset, 
  max_k = 3, 
  threshold = 0.05, 
  test = "testIndLogistic",
  ncores=1
)
mmmb.testIndLogistic



system.time({sesObject <- SES(bc_target, bc_dataset, max_k = 5, 
                              threshold = 0.2, test = "testIndLogistic", hash = TRUE, 
                              hashObject = NULL)})



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

predictors <- data[ , !(names(data) %in% c("target"))] # Remove target from feature matrix
target <- as.factor(data$target)
ncolPredictors<-ncol(predictors)


set.seed(123)  # For reproducibility

# Define control parameters for RFE
rfctrl <- rfeControl(
  functions = rfFuncs,     # Use Random Forest for feature selection
  method = "repeatedcv",   # Use repeated k-fold cross-validation
  number = 4,             # 10-fold cross-validation
  repeats = 10,             # Repeat X times for stability
  verbose = TRUE
)


set.seed(123)  # Ensure reproducibility

# Run Recursive Feature Elimination
rfe_result <- rfe(
  x = predictors, 
  y = target, 
  sizes = c(1:50 ),  # Number of features to evaluate
  rfeControl = rfctrl
)

# View the results
print(rfe_result)
predictors(rfe_result)
#[1] "crop_type.cacao"                            "district.dist_1"                            "agroecol_perspective_13"                   
#[4] "household_shock_recover_capacity"           "energy_tillage_haverst_type"                "nearest_farmer_adopted"                    
#[7] "influence_nr_frequency"                     "support_provider_cooperative_organizations" "num_crops_grown"                           
#[10] "sfs_monoculture_perennial_area"             "nearest_distance_dfs_km"                    "distance_main_road"                        
#[13] "num_info_exchange_extension"          
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