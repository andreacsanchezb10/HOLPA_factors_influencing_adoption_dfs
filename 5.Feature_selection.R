

#############################################################    
########## UPLOAD DATA #####-----
#############################################################

per_data_adoptionBinary1<- read.csv("per_data_adoptionBinary1.csv",sep=",")
per_adoptionBinary2<- read.csv("per_adoptionBinary2.csv",sep=",")


########### FUZZY FORESTS ALGORITHM ----
# Load libraries
library(WGCNA)
library(mvtnorm)
library(Matrix)
library(fuzzyforest)
library(party)
library(randomForest)
library(mt)

## Advantages
# - The fuzzy forests algorithm is an extension of random forests designed to obtain less bi-ased feature selection in the presence of correlated features. 
# - WGCNA takes in the matrix of features and uses the correlation structure to partition the features into distinct groups such that the 
#correlation between features in the same group is large and the correlation between features in separate groups is small.
# - Once features have been subdivided into distinct modules, fuzzy forests eliminates features in two steps:
# a screening step and a selection step. In the screening step, RFE-RF is used on each
#module to eliminate the least important features within each module. In the selection step, a ﬁnal RFE-RF is used on the surviving features.
# RFE-RF sequentially eliminates features with the lowest VIMs until a pre-speciﬁed percentage of features remain. 
# - The screening step of fuzzy forests achieves two goals. First, it reduces the number of features 
#that have to be analyzed at one time. Second, the ﬁnite sample bias caused by correlated 
#features is alleviated. In Nicodemus and Malley (2009), it is observed that unimportant 
#features that are correlated with an important feature are more likely to be chosen at the 
#root of tree than uncorrelated important features. The high importance of these unimportant 
#correlated features comes at the cost of the important uncorrelated features. When we analyze 
#each module separately, features in diﬀerent groups are no longer competing against one another.

## Disadvantage
#- Handle the presence of high correlated factors, but it is sensitive to it.
#https://bioinformaticsworkbook.org/tutorials/wgcna.html#gsc.tab=0

allowWGCNAThreads()
powers = c(c(1:10), seq(from = 12, to = 20, by = 2))

data_per_adoptionBinary1 <-per_data_adoptionBinary1%>%
  mutate(across(where(is.factor), ~ as.numeric(as.character(.))))

data_per_adoptionBinary1 = t(as.matrix(data_per_adoptionBinary1))
data_per_adoptionBinary1
str(data_per_adoptionBinary1)

sft_per_adoptionBinary1 = pickSoftThreshold(
  data_per_adoptionBinary1,             # <= Input data
  #blockSize = 30,
  powerVector = powers,
  verbose = 5
)

par(mfrow = c(1,2));
cex1 = 0.9;

plot(sft_per_adoptionBinary1$fitIndices[, 1],
     -sign(sft_per_adoptionBinary1$fitIndices[, 3]) * sft_per_adoptionBinary1$fitIndices[, 2],
     xlab = "Soft Threshold (power)",
     ylab = "Scale Free Topology Model Fit, signed R^2",
     main = paste("Scale independence")
)
text(sft_per_adoptionBinary1$fitIndices[, 1],
     -sign(sft_per_adoptionBinary1$fitIndices[, 3]) * sft_per_adoptionBinary1$fitIndices[, 2],
     labels = powers, cex = cex1, col = "red"
)
abline(h = 0.90, col = "red")
plot(sft_per_adoptionBinary1$fitIndices[, 1],
     sft_per_adoptionBinary1$fitIndices[, 5],
     xlab = "Soft Threshold (power)",
     ylab = "Mean Connectivity",
     type = "n",
     main = paste("Mean connectivity")
)
text(sft_per_adoptionBinary1$fitIndices[, 1],
     sft_per_adoptionBinary1$fitIndices[, 5],
     labels = powers,
     cex = cex1, col = "red")

picked_power = 8

# Number of simulations
sim_number <- 100
rf_mat <- matrix(NA, sim_number, 40)
ff_mat <- matrix(NA, sim_number, 40)
cif_mat <- matrix(NA, sim_number, 40)
ff_matWGCNA<- matrix(NA, sim_number, 40)
snr_mat<- matrix(NA, sim_number, 40)

# Set seed for reproducibility
set.seed(123)

X <- per_data_adoptionBinary1[, !(names(per_data_adoptionBinary1) %in% c("dfs_adoption_binary"))]
X <- X %>%mutate(across(where(is.factor), ~ as.numeric(as.character(.))))
X
# Binary outcome variable
y <- as.numeric(as.character(per_data_adoptionBinary1$dfs_adoption_binary))
y

# Sample size
n <- nrow(X)
p <- ncol(X)

# Run multiple simulations
for (k in 1:sim_number) {
  set.seed(123)  # Ensure reproducibility
  
  # === Random Forest ===
  rf <- randomForest(X, y, importance = TRUE, mtry = floor(sqrt(p)), ntree = 500)
  rf_list <- importance(rf, type = 1, scale = FALSE)
  rf_list <- rf_list[order(rf_list[, 1], decreasing = TRUE), ]
  rf_list <- rf_list[1:40] # Top 40 most important features
  rf_mat[k, ] <- names(rf_list)
  
  # === Fuzzy Forest (with WGCNA) ===
  WGCNA_params <- WGCNA_control(
    power = picked_power, 
    minModuleSize = 30,
    TOMType = "unsigned", 
    reassignThreshold = 0.05, 
    mergeCutHeight = 0.25, 
    numericLabels = TRUE, 
    pamRespectsDendro = FALSE)
  
  screen_paramsWGCNA <- screen_control(
    keep_fraction = 0.25,
    ntree_factor = 2,
    mtry_factor = 15,
    min_ntree = 500
  )
  
  select_paramsWGCNA <- select_control(
    number_selected = 40,
    drop_fraction = 0.1,
    ntree_factor = 2,
    mtry_factor = 15,
    min_ntree = 500
  )
  
  wff_fitWGCNA <- wff(X, y, WGCNA_params = WGCNA_params, select_params = select_paramsWGCNA, screen_params = screen_paramsWGCNA)
  ff_listWGCNA <- wff_fitWGCNA$feature_list[, 1]
  ff_matWGCNA[k, ] <- ff_listWGCNA
  
  # === Conditional Inference Forest ===
  if (n < 500) {
    cif <- cforest(y ~ ., data = as.data.frame(cbind(y, X)), controls = cforest_unbiased(ntree = 100, mtry = floor(sqrt(p))))
    cif_list <- varimp(cif, conditional = TRUE)
    cif_list <- sort(cif_list, decreasing = TRUE)
    cif_list <- names(cif_list)[1:40] # Top 40 features
    cif_mat[k, ] <- cif_list
  }
  # === Signal-to-Noise Ratio (SNR) ===
  snr <- fs.snr(x= X,y=y)
  snr_list <- snr$fs.order[1:40]
  snr_mat[k, ] <- snr_list
  
}

# Function to process matrices
process_mat <- function(mat, model_name) {
  as.data.frame(mat) %>%
    mutate(model = model_name, sim = row_number()) %>%
    pivot_longer(cols = -c(model, sim), names_to = "rank", values_to = "feature")
}

# Process all matrices using the function
combined_df_per_adoptionBinary1 <- bind_rows(
  process_mat(rf_mat, "Random Forest"),
  process_mat(ff_matWGCNA, "Fuzzy Forest with WGCNA"),
  process_mat(snr_mat, "Signal-to-Noise Ratio"),
  if (exists("cif_mat")) process_mat(cif_mat, "Conditional Inference Forest") else NULL
) %>%
  arrange(sim, model)%>%
  group_by(model,feature) %>%
  summarise(frequency = n(), .groups = 'drop')%>%
  mutate(frequency_per=(frequency/(sim_number))*100)%>%
  ungroup()%>%
  filter(frequency_per>=50)

# View result
head(combined_df_per_adoptionBinary1)

length(unique(combined_df_per_adoptionBinary1$feature[combined_df_per_adoptionBinary1$model=="Random Forest"])) #34 factors
length(unique(combined_df_per_adoptionBinary1$feature[combined_df_per_adoptionBinary1$model=="Conditional Inference Forest"])) #41 factors
length(unique(combined_df_per_adoptionBinary1$feature[combined_df_per_adoptionBinary1$model=="Fuzzy Forest with WGCNA"])) #32 factors
length(unique(combined_df_per_adoptionBinary1$feature[combined_df_per_adoptionBinary1$model=="Signal-to-Noise Ratio"])) #30 factors

# Extract the sets of retained factors by model
ff_factors_per_adoptionBinary1 <- unique(combined_df_per_adoptionBinary1$feature[combined_df_per_adoptionBinary1$model == "Fuzzy Forest with WGCNA"]) #40 factors
rf_factors_per_adoptionBinary1 <- unique(combined_df_per_adoptionBinary1$feature[combined_df_per_adoptionBinary1$model == "Random Forest"]) #38 factors
cif_factors_per_adoptionBinary1 <- unique(combined_df_per_adoptionBinary1$feature[combined_df_per_adoptionBinary1$model == "Conditional Inference Forest"]) #34 factors
cif_factors_per_adoptionBinary1
snr_factors_per_adoptionBinary1 <- unique(combined_df_per_adoptionBinary1$feature[combined_df_per_adoptionBinary1$model == "Signal-to-Noise Ratio"])
snr_factors_per_adoptionBinary1

# Jaccard function
jaccard <- function(set1, set2) {
  length(intersect(set1, set2)) / length(union(set1, set2))
}

### Compute Jaccard similarity
## Fuzzy Forest vs. Other Methods 
# → Lower Similarity
jaccard(ff_factors_per_adoptionBinary1, rf_factors_per_adoptionBinary1) #0.4107143
jaccard(ff_factors_per_adoptionBinary1, cif_factors_per_adoptionBinary1) #0.2962963
jaccard(ff_factors_per_adoptionBinary1, snr_factors_per_adoptionBinary1) #0.2741935

jaccard(rf_factors_per_adoptionBinary1, cif_factors_per_adoptionBinary1) #0.6511628
jaccard(rf_factors_per_adoptionBinary1, snr_factors_per_adoptionBinary1) #0.6

jaccard(cif_factors_per_adoptionBinary1, snr_factors_per_adoptionBinary1) #0.6904762


mismatch_rf_ff_per_adoptionBinary1 <- setdiff(rf_factors_per_adoptionBinary1, ff_factors_per_adoptionBinary1) # In RF but not in FF
mismatch_rf_ff_per_adoptionBinary1
mismatch_cif_ff_per_adoptionBinary1 <- setdiff(cif_factors_per_adoptionBinary1, ff_factors_per_adoptionBinary1) # In CIF but not in FF
mismatch_cif_ff_per_adoptionBinary1
mismatch_snr_ff_per_adoptionBinary1 <- setdiff(snr_factors_per_adoptionBinary1, ff_factors_per_adoptionBinary1) # In CIF but not in FF
mismatch_snr_ff_per_adoptionBinary1

#Selected features
ff_factors_per_adoptionBinary1
write.csv(ff_factors_per_adoptionBinary1,"per_factors_adoptionBinary1.csv",row.names=FALSE)





##############################################OLD#############################----------------------
###### --- RECURSIVE FEATURE SELECTION -----
library(caret)
library(mlbench)
library(Hmisc)
library(randomForest)
### Advantages of Recursive Feature Elimination (RFE)
# - Can handle high-dimensional datasets and identify the most important features.
# - Can be used with any supervised learning algorithm.

### Limitations of Recursive Feature Elimination (RFE)
# - Can be computationally expensive for large datasets.
# - May not be the best approach for datasets with many correlated features.
# - May not work well with noisy or irrelevant features.


predictors <- per_data_adoptionBinary1[ , !(names(per_data_adoptionBinary1) %in% c("dfs_adoption_binary"))] # Remove target from feature matrix
target <- as.factor(per_data_adoptionBinary1$dfs_adoption_binary)

set.seed(123)  # For reproducibility

# Define control parameters for RFE
rfctrl <- rfeControl(
  functions = rfFuncs,     # Use Random Forest for feature selection
  method = "repeatedcv",   # Use repeated k-fold cross-validation
  number = 4,             # 10-fold cross-validation
  repeats = 100,             # Repeat X times for stability
  verbose = TRUE
)



# Run Recursive Feature Elimination
rfe_result <- rfe(
  x = predictors, 
  y = target, 
  sizes = c(1:50),  # Number of features to evaluate
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




###### --- SES and MMPC -----
per_data_adoptionBinary1<- read.csv("per_data_adoptionBinary1.csv",sep=",")

dataset <- per_data_adoptionBinary1[, -which(names(per_data_adoptionBinary1) == "dfs_adoption_binary")] # Remove target from feature matrix
target <- as.factor(per_data_adoptionBinary1$dfs_adoption_binary)
target
best_model <- cv.ses(target, dataset, kfolds = 10, 
                      alphas = c(0.1, 0.05, 0.01), max_ks = c(3, 2), 
                      task="C", ncores = 1)

best_model


# set up
rm(list = ls())
time <- proc.time()
repetitions <- 2
nCores <- 3

# replicateMode = 1: Replicate the whole experiment of 500 iterations
#                    for each one of the three datasets that are
#                    refered in the article and produce the tables and
#                    figures of the paper.
# replicateMode = 2: [DEFAULT] Replicate the tables and figures by
#                    loading our results RData files (for saving time
#                    by not performing the foul analysis from scratch)
replicateMode <- 1

if (replicateMode == 1) {
  
  # cleaning memory
  rm(list = setdiff(ls(), c("time", "repetitions", "nCores", "replicateMode")))
  
  library("parallel")
  library("survival")
  library("Hmisc")
  library("glmnet") #for the lasso function
  library("hash")
  library("ROCR")
  library("TunePareto")
  library("foreach")
  library("MASS")
  library("VGAM")
  library("MXM")
  
  date()
  rm(list = setdiff(ls(), c("time", "repetitions", "nCores", "replicateMode")))
  
  # load the main pipeline of the experiments (EXP function)
  source("Experiment.R")
  
  # Initialize the cluster
  
  cl <- makeCluster(nCores)
  
  a <- function() {
    library("MXM")
  }
  tmp <- clusterCall(cl, a)
  
  varlist <- unique(c(ls(), ls(envir = .GlobalEnv), ls(envir = parent.env(environment()))))
  clusterExport(cl, varlist = varlist, envir = environment())
  clusterSetRNGStream(cl)
  wd <- getwd()
  clusterExport(cl, varlist = "wd", envir = environment())
  
  do.wrap <- function(iter, dataset_name, bc_dataset, bc_target) {
    if(iter <= 100) {
      set.seed(iter+10)
    }
    if(iter > 100 & iter <= 200) {
      set.seed((iter-100)*10+2)
    }
    if(iter > 200 & iter <= 300) {
      set.seed((iter-200)*8+136)
    }
    if(iter > 300 & iter <= 400) {
      set.seed(2*(iter-300)+38)
    }
    if(iter > 400 & iter <= 500) {
      set.seed(3*(iter-400)+77)
    }
    
    require("glmnet")
    require("hash")
    require("MASS")
    require("VGAM")
    require("stats")
    require("ROCR")
    require("Hmisc")
    require("TunePareto")
    require("methods")
    require("utils")
    require("MXM")
    
    res <- EXP(dataset_name, bc_dataset, bc_target, test = "testIndLogistic", task = "C")
    print(res)
    r <- vector("list", 1)
    
    r[[1]]$SES_queues <- length(res$SES_res@queues)
    r[[1]]$SES_vars <- length(unlist(res$SES_res@queues))
    r[[1]]$vs <- unique(unlist(res$SES_res@queues))
    #vl <- res$lasso_vars
    #r[[1]]$jaccard <- sum(!is.na(match(vl, vs)))/length(unique(c(vl, vs)))
    r[[1]]$SES_performance <- res$SES_performance
    r
  }
  
  data<- read.csv("per_data_adoptionBinary1.csv",sep=",")
  bc_dataset <- data[, -which(names(data) == "target")] # Remove target from feature matrix
  bc_target <- as.factor(data$target)
  
  print(bc_target)
  
  test <- "testIndLogistic"
  
  set.seed(123456789)
  
  results_breast_full <- clusterApply(cl, 1:repetitions, 
                                      do.wrap, dataset_name = "dataset_name", 
                                      bc_dataset = bc_dataset, bc_target = bc_target)
  
  # save the current dataset results
  save(results_breast_full, file = "breast.rda")
  
  date()
  
  stopCluster(cl)
}


# Breast Cancer (Classification) Results

if (replicateMode == 1) {
  load("C:/Users/andreasanchez/OneDrive - CGIAR/3_chapter_PhD/HOLPA_factors_influencing_adoption_dfs/breast.rda")
} else {
  load("C:/Users/andreasanchez/OneDrive - CGIAR/3_chapter_PhD/HOLPA_factors_influencing_adoption_dfs/v80i07-replication/original_results/breast.rda")
}

sesPerf <- vector("numeric", repetitions)
sesNVar <- vector("numeric", repetitions)
sesNSig <- vector("numeric", repetitions)

stdPerf <- vector("numeric", repetitions)
meanPerf <- vector("numeric", repetitions)

results <- results_breast_full
head(results)

for (i in 1:length(results)) {
  sesPerf[i] <- results[[i]][[1]]$SES_performance[1] # take the first performance or take the mean
  sesNVar[i] <- results[[i]][[1]]$SES_queues
  sesNSig[i] <- length(results[[i]][[1]]$SES_performance)
  #jaccard[i] <- results[[i]][[1]]$jaccard
  
  stdPerf[i] <- sd(results[[i]][[1]]$SES_performance)
  meanPerf[i] <- mean(results[[i]][[1]]$SES_performance)
}

coefvar <- stdPerf/abs(meanPerf)

jss_b <- NULL
jss_b$sesPerf <- sesPerf
jss_b$sesNVar <- sesNVar
jss_b$sesNSig <- sesNSig
jss_b$meanPerf <- meanPerf
jss_b$stdPerf <- stdPerf
jss_b$coefvar <- coefvar

rm(results_breast_full)

#  JSS paper TABLE 3        #
# Frequency of signature multiplicity. Each cell reports how many
# times j equivalent signatures are retrieved for its corresponding
# data set (row), where j is the number reported on top of the cell"s
# column. The notation 10+ indicates 10 or more signatures.

table3 = matrix(0, 1, 20)
rownames(table3) <- c("Adoption")
colnames(table3) <- c(1:19,"+20")
for (i in 1:19) {
  table3[1, i] = sum(jss_b$sesNSig == i)
}
table3[1, 20] = sum(jss_b$sesNSig >= 20)

print("")
print("Table 3")
print(table3)
print("")
unique(sapply(results, function(x) x[[1]]$SES_queues))

# Extract $vs where $SES_queues = 14
vs_for_queues <- lapply(results, function(x) {
  if (length(x) > 0 && "SES_queues" %in% names(x[[1]])) {
    if (x[[1]]$SES_queues == 8) {
      return(x[[1]]$vs)
    }
  }
  return(NULL)
})

# Remove NULL values (if any)
vs_for_queues <- Filter(Negate(is.null), vs_for_queues)

# Show the result
print(vs_for_queues)
str(results)
#  JSS paper TABLE 4      #

# Quantiles of the coefficient of variation (CV) of the SES performances. Results are reported separately for each data set (rows).

q1 = quantile(jss_b$coefvar, probs = c(0.025, 0.975), na.rm = TRUE)
m1 = mean(jss_b$coefvar, na.rm=T)

table4 = matrix(0, 1, 3)
table4[1, ] = c(q1[1], m1, q1[2])*100
rownames(table4) <- c("Adoption")
colnames(table4) <- c("2.5%", "Median", "97.5%")

print("")
print("Table 4")
print(table4)
###########
# Extract selected variables
all_vars <- sort(unique(unlist(lapply(results, function(x) x[[1]]$vs))))
all_vars

# Create a binary matrix for presence/absence of variables
binary_matrix <- t(sapply(results, function(x) as.integer(all_vars %in% x[[1]]$vs)))


# Step 3: Compute selection frequency for each variable
selection_frequency <- colMeans(binary_matrix)

# Step 4: Create a data frame of variable stability
stability_df <- data.frame(
  Variable = all_vars,
  Selection_Frequency = selection_frequency
)

# Step 5: Rank by stability (higher frequency = more stable)
stability_df <- stability_df[order(-stability_df$Selection_Frequency), ]

# Step 6 (Optional): Identify consensus set (e.g., > 25% threshold)
consensus_set <- stability_df[stability_df$Selection_Frequency >= 0.25, ]

# Show results
print(stability_df)
print("Consensus Set (Selected in > 50% of repetitions):")
print(consensus_set)

z<- data[, consensus_set$Variable]
names(z)
#[1] "household_shock_strategy_count"  "crop_type.frutales"              "soil_pH_mean"                    "sfs_monoculture_annual_adoption" "vegetation_diversity_wetland"


########################
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
ses
ses@selectedVars
ses@signatures
ses@queues
length(ses@queues)
unique(unlist(ses@queues))

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



############################################################# ----
#Bayesian subset selection and variable importance for interpretable prediction and classification
#EL CODIGO NO FUNCIONA!
#https://github.com/drkowal/BayesSubsets/blob/main/vignettes/Binary-target.Rmd
library(BayesSubsets)
library(rstanarm)

per_data1_CenteredScaled <- per_data1_CenteredScaled %>%
  mutate(across(where(is.factor), ~ as.numeric(as.character(.))))


# FITTING THE BAYESIAN LOGISTIC MODEL WITH HORSEHOE PRIOR
set.seed(123) 
library(bayeslm)

X <- per_data1_CenteredScaled[, !(names(per_data1_CenteredScaled) %in% c("dfs_adoption_binary"))]
class(X)
X<-X%>%
  select(1:20)
X<-as.matrix(X)
X
# Binary outcome variable
y <- as.numeric(per_data1_CenteredScaled$dfs_adoption_binary)


fit = bayeslm(y ~ X[,-1], # intercept already included
              prior = 'horseshoe',
              N = 10000, # MCMC samples to save
              burnin = 5000,
              singular = TRUE)

fit$beta
# Generate posterior predictive draws and log-predictive densities
temp = post_predict(post_y_hat = tcrossprod(fit$beta, X),
                    post_sigma = fit$sigma,
                    yy = y)

post_y_pred = temp$post_y_pred
post_lpd = temp$post_lpd

# Define the functional for binary outcome
tau = quantile(y, 0.9)
h = function(t){
  1.0*I(t >= tau)
}

# Posterior predictive draws of h:
post_h_pred = h(post_y_pred)
post_h_pred
h_bar = colMeans(post_h_pred)


# Make sure we do not have any zeros or ones:
h_bar[h_bar == 0] = min(h_bar[h_bar != 0]) # set to the non-zero min
h_bar[h_bar == 1] = max(h_bar[h_bar != 1]) # set to the non-one max

indicators = branch_and_bound(yy = log(h_bar/(1 - h_bar)), # response is the logit of h_bar
                              XX = X,            # covariates
                              wts = h_bar*(1 - h_bar), # weights for weighted least squares
                              n_best = 50        # restrict to the "best" 50 subsets of each size
)




indicators[1:5, 1:20]
table(rowSums(indicators)) # note: intercept always included
nrow(indicators)

accept_info = accept_family_binary(post_y_pred = post_h_pred,
                                   post_lpd = post_lpd,
                                   XX = X,
                                   indicators = indicators,
                                   loss_type = "cross-ent",
                                   yy = h(y))



# Smallest acceptable subset
beta_hat_small = accept_info$beta_hat_small
S_small = which(beta_hat_small != 0)

# Posterior predictive samples for smallest acceptable subset
post_beta_small = proj_posterior(post_y_pred = post_h_pred,
                                 XX = X,
                                 sub_x = S_small,
                                 use_ols = FALSE)

# 95% credible intervals for smallest acceptable subset
ci_small = t(apply(post_beta_small[,S_small], 2, quantile, c(0.05/2, 1 - 0.05/2)))

# Variable importance
vi_e = var_imp(indicators = indicators,
               all_accept = accept_info$all_accept)$vi_inc

# "Keystone covariates" that appear in *all* acceptable families:
which(vi_e == 1)


# Irrelevant covariates that appear in *no* acceptable families:
which(vi_e == 0) 

# Visualize:
barplot(vi_e[order(vi_e, (ncol(X):1))], # order...
        horiz = TRUE, 
        main = paste('Variable importance for the acceptable family'))
abline(v = 1)





###############################
fit <- stan_glm(target ~ ., 
                data = data, 
                family = binomial(link = "logit"), 
                prior = hs(),
                chains = 1,
                iter = 1000,
                adapt_delta = 0.99)
QR=TRUE)

summarySingleLevelModel <- summary(fit) 
print(summarySingleLevelModel)

# Ensure factors_binary is numeric and aligned
factors_binary <- as.matrix(factors_binary)
factors_binary <- apply(factors_binary, 2, as.numeric)
factors_binary <- cbind(Intercept = 1, factors_binary)
factors_binary
# Extract posterior draws for beta


#UNCERTAINTY QUANTIFICATION FOR LINEAR COEFFICIENTS
# Extract the posterior predictive draws and lpd:

beta <- as.matrix(fit)
dim(beta)
dim(factors_binary)
post_y_hat <- tcrossprod(beta, factors_binary)
dim(post_y_hat)
# Create a dummy sigma value (fixed at 1 for logistic regression)
post_sigma <- rep(1, nrow(post_y_hat))
post_sigma
adoption_binary <- as.numeric(adoption_binary)
# Generate posterior predictions
temp <- post_predict(post_y_hat = post_y_hat,
                     post_sigma = post_sigma,
                     yy = adoption_binary)


temp

# Extract the posterior predictive draws and lpd:
post_y_pred = temp$post_y_pred
post_y_pred= na.omit(post_y_pred)
post_lpd = temp$post_lpd
post_lpd= na.omit(post_lpd)
class(post_lpd)

###### DEFINING THE FUNCTIONAL
tau = quantile(adoption_binary, 0.9)
h = function(t){
  1.0*I(t >= tau)
}

# Posterior predictive draws of h:
post_h_pred = h(post_y_pred)
post_h_pred
# Fitted values:
h_bar = colMeans(post_h_pred)
h_bar


##### BAYESIAN SUBSET SEARCH
# Make sure we do not have any zeros or ones:
h_bar[h_bar == 0] = min(h_bar[h_bar != 0]) # set to the non-zero min
h_bar[h_bar == 1] = max(h_bar[h_bar != 1]) # set to the non-one max

indicators = branch_and_bound(yy = log(h_bar/(1 - h_bar)), # response is the logit of h_bar
                              XX = X,            # covariates
                              wts = h_bar*(1 - h_bar), # weights for weighted least squares
                              n_best = 50        # restrict to the "best" 50 subsets of each size
)

# Inspect:
indicators[1:5, 1:10]


# Allowable covariates:
to_consider = prescreen(beta, num_to_keep = 30)

# Exclude the rest:
to_exclude = (1:ncol(factors_binary))[-to_consider]

indicators = branch_and_bound(yy = fitted(fit), # response is the fitted values
                              XX = factors_binary,            # covariates
                              n_best = 50,       # restrict to the "best" 15 subsets of each size
                              to_include = 1,    # keep the intercept always
                              to_exclude = to_exclude # pre-screened
)
indicators
#indicators <- indicators[complete.cases(indicators), ]

# Inspect:
indicators[1:5, 1:10]

dim(indicators)
#> [1] 1360  177

# Summarize the model sizes:
table(rowSums(indicators)) # note: intercept always included
table(adoption_binary)


# Compute the acceptable family:
dim(post_h_pred)
dim(post_lpd)
dim(factors_binary)
dim(adoption_binary)
anyNA(indicators)
adoption_binary <- as.numeric(adoption_binary)
dim(post_lpd)
class(post_lpd)
adoption_binary <- as.integer(adoption_binary)

dim(post_lpd) # Should now be (2000,)
accept_info = accept_family_binary(post_y_pred = post_h_pred,
                                   post_lpd = post_lpd,
                                   XX = factors_binary,
                                   indicators = indicators,
                                   loss_type = "cross-ent",
                                   yy = h(adoption_binary))

#accept_info = accept_family(post_y_pred = post_y_pred,
#                           post_lpd = post_lpd,
#                          XX = factors_binary,
#                         indicators = indicators,
#                        yy = adoption_binary,
#                       post_y_hat = tcrossprod(beta, factors_binary))

dim(tcrossprod(beta, factors_binary))
dim(post_y_pred) # Should be (S x n)
dim(post_lpd) # Should be (S)
dim(factors_binary) # Should be (n x p)
dim(beta) # Should be (S x p)

class(factors_binary)
class(adoption_binary)

class(indicators )

anyNA(post_y_pred)
anyNA(post_lpd)
anyNA(factors_binary)
anyNA(indicators)
anyNA(adoption_binary)
anyNA(beta)
class()



# Make sure we do not have any zeros or ones:
h_bar[h_bar == 0] = min(h_bar[h_bar != 0]) # set to the non-zero min
h_bar[h_bar == 1] = max(h_bar[h_bar != 1]) # set to the non-one max



indicators = branch_and_bound(yy = log(h_bar/(1 - h_bar)), # response is the logit of h_bar
                              XX = factors_binary,            # covariates
                              wts = h_bar*(1 - h_bar), # weights for weighted least squares
                              n_best = 50        # restrict to the "best" 50 subsets of each size
)

# Inspect:
indicators[1:5, 1:10]

# Dimensions:
dim(indicators)

# Summarize the model sizes:
table(rowSums(indicators)) # note: intercept always included




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


####### LASSO Logistic regression CV-----

X <- per_data1_CenteredScaled[, !(names(per_data1_CenteredScaled) %in% c("dfs_adoption_binary"))]
X <- X %>%mutate(across(where(is.factor), ~ as.numeric(as.character(.))))
X<-as.matrix(X)
# Binary outcome variable
y <- per_data1_CenteredScaled$dfs_adoption_binary
library(glmnet)
library(plotmo)

lam<- seq(from=0.1,to=10, by= 0.1)
glm.fit1<-  glm(dfs_adoption_binary~. ,
                family = binomial(link="logit"),
                data= per_data1_CenteredScaled)
summary(glm.fit1)

x_vars<-model.matrix(dfs_adoption_binary~.,per_data1_CenteredScaled)[,-1]
x_vars

lasso.fit1<-glmnet(x=x_vars,
                   y= y,
                   alpha=1,
                   family="binomial"
)


coef(lasso.fit1)
plot_glmnet(lasso.fit1)

lam_seq<-seq(from=0.1, to=10, by=0.1)
lam_seq
lasso.cv.fit1<-  cv.glmnet(x=x_vars,
                           y= y,
                           alpha=1,
                           family="binomial",
                           nfolds = 10,
                           lambda = lam_seq)

lam_best<- lasso.cv.fit1$lambda.1se
lam_best

lasso.fit1.best<- glmnet(x=x_vars,
                         y= y,
                         alpha=1,
                         family="binomial"
                         lambda = lam_best
)

coef(lasso.fit1.best)



# Convert coefficients to matrices to preserve row names
coef_lasso <- as.data.frame(as.matrix(coef(lasso.fit1.best)))

# Combine while keeping row names
factors_selected <- data.frame(
  glm = coef_glm[, 1],
  lasso = coef_lasso[, 1],
  row.names = rownames(coef_glm)
)

# Display the combined coefficients
head(factors_selected)


# Extract coefficients at the lambda.min as a vector

coef_selected

# Get the row names (which are the feature names)
selected_features <- rownames(coef(fit, s = fit$lambda.min))[coef_selected != 0]
selected_features
# Remove the intercept
selected_features <- setdiff(selected_features, "(Intercept)")
selected_features
# Filter out any NA values
selected_features <- na.omit(selected_features)

# Subset the matrix using valid column names
selected_features <- intersect(colnames(X), selected_features)

# Subset the original matrix
M <- X[, selected_features, drop = FALSE]

# Check selected features
print(selected_features)


cvfit <-cv.glmnet (M, y , family = "binomial",
                   nfolds=5, type.measure= "class" , alpha =.99)
cvfit