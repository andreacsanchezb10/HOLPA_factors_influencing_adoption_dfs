library(dplyr)
library(readxl)
library(caret)
library(reshape2)
library(summarytools)
library(bnlearn)
library(corrplot)


factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")
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

dim(per_data_analysis) #[1] 200 342 #200 farmers; 342 variables evaluated

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


dim(per_data_Binary) #[1] 200 358 #200 farmers; 358 variables evaluated

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

dim(per_data_Binary) #[1] 200 358 #200 farmers; 358 variables retained


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

dim(per_data_analysis_Filterednzv) #[1] 200 248 #200 farmers; 248 variables retained

#############################################################    
############# IDENTIFYING CORRELATED FACTORS -----
#############################################################
###### --- Pearson Correlation -----
#only for continuous variables
columns_continuous <- intersect(per_summary_numerical$column_name_new, colnames(per_data_analysis_Filterednzv))
print(columns_continuous)
per_data_analysis_cont<- per_data_analysis_Filterednzv%>%
  select(all_of(columns_continuous))

pearson.cor<- dedup(per_data_analysis_cont,
            threshold= 0.8,
            debug = TRUE)
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
#9 removed factors
#[1] "income_amount_total"               "num_adults_wa"                     "total_main_crops_cropland_area"    "cropland_area"                    
#[5] "dfs_total_area"                    "male_land_tenure_own_area"         "female_land_tenure_own_area"       "female_land_tenure_own_proportion"
#[9] "distance_public_transport"        

pearson.cor_matrix <- cor(per_data_analysis_cont, method = "pearson", use = "pairwise.complete.obs")
person.highCor_matrix <- pearson.cor_matrix[pearson.highCor_factors, pearson.highCor_factors]

# Plot the correlation matrix
corrplot(person.highCor_matrix, method = "color", type = "upper", tl.cex = 0.7, tl.col = "black")


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
#47 Factors with high Spearman correlation

spearman.highCor_matrix <- spearman.cor[spearman.highCor_factors, spearman.highCor_factors]

corrplot(spearman.highCor_matrix, method = "color", type = "upper", tl.cex = 0.7, tl.col = "black")

# Find row and column indices of high correlation pairs
spearman.highCor_factors_pairs <- as.data.frame(which(abs(spearman.cor) > 0.8, arr.ind = TRUE))%>%
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
library(BayesSubsets)

str(factors)
factors <- per_data_analysis_CenteredScaled[ , !(names(per_data_analysis_CenteredScaled) %in% c("dfs_adoption_binary"))] # Remove target from feature matrix

target <- per_data_analysis_CenteredScaled$dfs_adoption_binary


factors <- as.matrix(do.call(cbind, factors))

factors <- as.data.frame(factors)

data <- data.frame(target = target, factors)
# Convert to matrix or data frame (depending on the structure of 'factors')
factors <- as.matrix(do.call(cbind, factors)) # If all numeric
# OR
factors <- as.data.frame(factors) # If mixed types

# Create a data frame with target and predictors
data <- data.frame(target = target, factors)

# Fit Bayesian logistic model with horseshoe prior
set.seed(123) 
fit <- stan_glm(target ~ ., 
                data = data, 
                family = binomial(link = "logit"), 
                prior = hs(),
                chains = 1,
                QR=TRUE) 

                chains = 4) 
                iter = 15000, # Total number of MCMC iterations
                warmup = 5000) 
                QR = TRUE)

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


###### --- LINEAR DEPENDENCIES -----
### Only for continuous variables...

columns_continuous <- intersect(per_summary_numerical$column_name_new, colnames(per_data_analysis_FilteredCorr))
print(columns_continuous)
per_data_analysis_cont<- per_data_analysis_FilteredCorr%>%
  select(all_of(columns_continuous))

dependencies<- findLinearCombos(per_data_analysis_cont)
dependencies$remove

dependent_factors <- per_data_analysis_cont[, unlist(dependencies$linearCombos), drop = FALSE]
print(dependent_factors)
view(dfSummary(dependent_factors))
per_data_analysis_cont[, unlist(dependencies$remove), drop = FALSE]
#REMOVE
#num_people

per_data_analysis_FilteredDepend <- per_data_analysis_FilteredCorr[, -unlist(dependencies$remove), drop = FALSE]

d<-as.data.frame(c(colnames(per_data_analysis_FilteredDepend)))%>%
  rename("column_name_new"="c(colnames(per_data_analysis_FilteredDepend))")%>%
  left_join(factors_list%>%select(category_1,column_name_new), by="column_name_new")%>%
  group_by(category_1) %>%
  mutate(column_name_new_count = n()) %>%
  tally()%>%filter(category_1!="xxx")

ggplot(data=d, aes(x=n, y=category_1, fill= category_1)) +
  geom_bar(stat="identity")+
  geom_text(aes(label = n), 
            hjust = -0.2, # Adjust position of the label to the right of the bar
            size = 4) +
  theme_minimal() +
  labs(x = "Number of factors", y = "Category") +
  theme(legend.position = "none")

dim(per_data_analysis_FilteredDepend) #[1] 200 240 #200 farmers; 240 variables retained

###### --- IMPUTING MISSING DATA ----
na_columns <- colnames(per_data_analysis_FilteredCorr)[colSums(is.na(per_data_analysis_FilteredCorr)) > 0]
print(na_columns)
na_columns

problems2<- per_data_analysis_Filterednzv%>%
  select(all_of(na_columns))
y<-per_data_analysis%>%
  select(    "male_land_tenure_own_proportion" ,   "female_land_tenure_own_proportion", 
         "male_land_tenure_hold_proportion",   "female_land_tenure_hold_proportion")

per_data_analysis_Filterednzv<- per_data_analysis_Filterednzv%>%
  select(-all_of(na_columns))

colnames(problems2)


###### --- SES and MMPC -----
library(MXM)

bc_dataset <- per_data_analysis_Filterednzv[ , !(names(per_data_analysis_Filterednzv) %in% c("dfs_adoption_binary"))] # Remove target from feature matrix
bc_target <- as.factor(per_data_analysis_Filterednzv$dfs_adoption_binary)

ses.testIndLogistic <- SES(
  target = per_data_analysis_Filterednzv$dfs_adoption_binary,  # Reference as column in the data
  dataset = per_data_analysis_Filterednzv[, -which(names(per_data_analysis_Filterednzv) == "dfs_adoption_binary")], 
  max_k = 3, 
  threshold = 0.05, 
  test = "testIndLogistic"
)
ses.testIndLogistic


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


###### --- FBED -----
library(MXM)
dataset_name<-per_data_analysis_Filterednzv
bc_dataset <- per_data_analysis_Filterednzv[ , !(names(per_data_analysis_Filterednzv) %in% c("dfs_adoption_binary"))] # Remove target from feature matrix
bc_target <- as.factor(per_data_analysis_Filterednzv$dfs_adoption_binary)

alphas <- c(0.05, 0.1) #significance level α of the conditional independence test
test<- "testIndLogistic" # conditional independence test for outcome = binary, predictors= mixed

MXM::fbed.reg(target = bc_target, dataset = bc_dataset, test = "testIndLogistic")

fbed.reg(bc_target, bc_dataset, #id, kfolds = 10, 
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






