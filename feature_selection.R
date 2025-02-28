library(dplyr)
library(readxl)

factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")


per_data_clean<- read.csv("per_data_clean.csv",sep=",")
per_summary_categorical<-read.csv("per_summary_categorical.csv",sep=",")
per_summary_numerical<-read.csv("per_summary_numerical.csv",sep=",")

sort(unique(per_data_clean$temperature_change_perception))


############# DATA TYPE CONVERSION -----
#### Convert categorical and binary to factor
columns_categorical_nominal <- intersect(per_summary_categorical$column_name_new2, colnames(per_data_clean))

print(columns_categorical_nominal)  # Check if it holds expected values

per_data_clean<- per_data_clean%>%
  mutate(across(all_of(columns_categorical_nominal), as.factor))
sort(unique(per_data_clean$dfs_adoption_binary))

#### Convert continuous variables to numeric
columns_continuous <- intersect(per_summary_numerical$column_name_new, colnames(per_data_clean))
print(columns_continuous)  # Check if it holds expected values

per_data_clean<- per_data_clean%>%
  mutate(across(all_of(columns_continuous), as.numeric))

############# SELECT VARIABLE FOR ANALYSIS -----
#### Select the factors that were listed as important for adoption
per_variables_list<-c(unique(per_summary_categorical$column_name_new2))#,unique(per_summary_numerical$column_name_new))
per_variables_list

per_data_analysis<- per_data_clean%>%
  dplyr::select(all_of(per_variables_list))

str(per_data_analysis)
#### Remove columns with NA values TO CHECK: HOW I'M GOING TO DEAL WITH MISSING VALUES

#check the columns with NA
na_columns <- colnames(per_data_analysis)[colSums(is.na(per_data_analysis)) > 0]
print(na_columns)

per_data_analysis <- per_data_analysis%>%
  dplyr::select(!all_of(na_columns))


## CHECK remove the outcomes different than dfs_adoption_binary, 
excluded_for_binary<- c ( "dfs_agroforestry_adoption"  ,                    
                          "dfs_cover_crops_adoption" ,                       "dfs_crop_rotation_adoption" ,                    
                          "dfs_fallow_adoption"   ,                          "dfs_hedgerows_adoption" ,                        
                          "dfs_homegarden_adoption",                         "dfs_intercropping_adoption",                     
                          "dfs_strip_vegetation_adoption"  )
  
  
  
  "dfs_agroforestry_area" ,"dfs_agroforestry_area" ,
                        "dfs_cover_crops_area", "dfs_homegarden_area" ,   
                                                        
                        "dfs_crop_rotation_area" ,
                        "dfs_intercropping_area"    ,                      "dfs_fallow_area",                                
                        "dfs_strip_vegetation_area",                       "dfs_hedgerows_area",
                                      )

## CHECK remove the outcomes different than "dfs_total_area"
excluded_for_continuous<- c ( "dfs_agroforestry_area" ,"dfs_agroforestry_area" ,
                          "dfs_cover_crops_area", "dfs_homegarden_area" ,   
                          "dfs_adoption_binary",                                 
                          "dfs_crop_rotation_area" ,
                          "dfs_intercropping_area"    ,                      "dfs_fallow_area",                                
                          "dfs_strip_vegetation_area",                       "dfs_hedgerows_area",
                          "dfs_agroforestry_adoption"  ,                    
                          "dfs_cover_crops_adoption" ,                       "dfs_crop_rotation_adoption" ,                    
                          "dfs_fallow_adoption"   ,                          "dfs_hedgerows_adoption" ,                        
                          "dfs_homegarden_adoption",                         "dfs_intercropping_adoption",                     
                          "dfs_strip_vegetation_adoption",
                          "education_level","ethnicity","marital_status",
                          "rainfall_timing_change_perception.nochange",     
                          "rainfall_timing_change_perception.notsure" ,     
                          "rainfall_timing_change_perception.startearlier",
                          "rainfall_timing_change_perception.startlater" ,  
                          "rainfall_timing_change_perception.stopearlier" , 
                          "rainfall_timing_change_perception.stoplater" ,   
                          "rainfall_timing_change_perception.unpredictable",
                          "temperature_change_perception")


per_data_analysis.binary<- per_data_analysis%>%
  dplyr::select(!all_of(excluded_for_binary))%>%
  dplyr::select(where(~ !(is.factor(.) && nlevels(.) < 2)))%>%
  dplyr::select(-energy_type,-fair_price_livestock,-human_wellbeing_5 )

write.csv(per_data_analysis.binary,"per_data_analysis.binary.csv",row.names=FALSE)



  -temperature_change_perception,-rainfall_timing_change_perception.nochange )

summary(per_data_analysis.binary)
names(per_data_analysis)

str(per_data_analysis)

sort(unique(per_data_analysis.binary$temperature_change_perception ))

#################

str(per_data_analysis.binary$dfs_adoption_binary)
library("MXM")
library(survival)


set.seed(12345678)


## Identifying the best combination of SES hyperparameters

per.cs.ses<- cv.ses(per_data_analysis.binary$dfs_adoption_binary,  # Reference as column in the data
       dataset = per_data_analysis.binary[, -which(names(per_data_analysis.binary) == "dfs_adoption_binary")], 
       wei = NULL, kfolds = 4,  
       alphas = c(0.1, 0.05, 0.01), max_ks = c(5,4,3,2), task = "C", 
       metric = NULL, modeler = NULL, ses_test = NULL, ncores = 1)

per.cs.ses
per.cs.ses$best_performance
per.cs.ses$best_configuration



### testIndLogistic 
# - Outcome: Binary 
# - Predictors: Mixed; 
# - Regression: Logistic regression; 
# - Robust option: No
str(per_data_analysis.binary$dfs_adoption_binary)

ses.testIndLogistic <- SES(
  target = per_data_analysis.binary$dfs_adoption_binary,  # Reference as column in the data
  dataset = per_data_analysis.binary[, -which(names(per_data_analysis.binary) == "dfs_adoption_binary")], 
  max_k = 3, 
  threshold = 0.05, 
  test = "testIndLogistic"
)
ses.testIndLogistic


plot(ses.testIndLogistic, mode = "all")

hashObj <- ses.testIndLogistic@hashObject
hashObj
ses.testIndLogistic2 <- SES(
  target = per_data_analysis.binary$dfs_adoption_binary,  # Reference as column in the data
  dataset = per_data_analysis.binary[, -which(names(per_data_analysis.binary) == "dfs_adoption_binary")], 
  max_k = 3, 
  threshold = 0.05, 
  test = "testIndLogistic",
  hash = TRUE
  #hashObject = hashObj
)
ses.testIndLogistic2
summary(ses.testIndLogistic$selectedVars)




### testIndSpeedglm ,
# - Outcome: Continuous binary or counts
# - Predictors:Mixed Linear, logistic and
# - Regression: Poisson regression
# - Robust option: No

ses.testIndSpeedglm <- SES(
  target = per_data_analysis.binary$dfs_adoption_binary,  # Reference as column in the data
  dataset = per_data_analysis.binary[, -which(names(per_data_analysis.binary) == "dfs_adoption_binary")], 
  max_k = 3, 
  threshold = 0.05, 
  #test = "testIndSpeedglm"
)
ses.testIndSpeedglm



MXM::gomp(target = per_data_analysis.binary$dfs_adoption_binary,
              dataset = per_data_analysis.binary[, -which(names(per_data_analysis.binary) == "dfs_adoption_binary")],
              test = "testIndLogistic")

MXM::MMPC(target = per_data_analysis.binary$dfs_adoption_binary,
          dataset = per_data_analysis.binary[, -which(names(per_data_analysis.binary) == "dfs_adoption_binary")],
          test = "testIndLogistic")



sesObject2 <- SES(dfs_adoption_binary, per_data_analysis.binary,
                  max_k = 2, threshold = 0.01, test = "testIndFisher")



# get the run time
sesObject@runtime;
sesObject2@runtime;

# MMPC algorithm 
mmpcObject <- MMPC(target, dataset, max_k = 3, threshold = 0.05, test="testIndFisher");
mmpcObject@selectedVars
mmpcObject@runtime



#########################################################################################################################################################################################
############# DISCRETIZATION or whatever I'm going to do here-----
#########################################################################################################################
install.packages("discretize")
library("discretize")



# Install and load required packages
library(bnstruct)
library(infotheo)


per_data_analysis.continuous<- per_data_analysis%>%
  select(all_of(unique(per_summary_numerical$column_name_new)))
  select(age,years_in_community)
  select(-num_occupation_secondary_list,-num_adults_old,-num_training_topics,-num_adults_wa,age,years_in_community)
summary(per_data_analysis.continuous)
names(per_data_analysis.continuous)

str(per_data_analysis.continuous)
discrete<- discretize(per_data_analysis.continuous, method='hartemink',breaks = 2,ibreaks = 10, debug = FALSE)
discrete

data(gaussian.test)

d = discretize(gaussian.test, method = 'hartemink', breaks = 4, ordered=TRUE,ibreaks = 20)
plot(hc(d))

#########################################################################################################################################################################################
############# FEATURE SELECTION FOR BAYESIAN STRUCTURAL LEARNING -----
#########################################################################################################################
library(bnlearn)
library(Rgraphviz)
library(caret)


#### see important references
#Causal Learner: A Toolbox for Causal Structure and Markov Blanket Learning

##### Embedded methods for feature selection

#### Method: Markov Blanket----
##bnlearn
#Grow-Shrink (gs): non missin values, 
#Incremental Association (iamb):
#Fast Incremental Association (fast.iamb):
#Interleaved Incremental Association (inter.iamb):

#### Method: Global causal structurure learning----
###Algorithms
##package: bnlearn
#PC (pc.stable): non missing values, continuous, categorical, mixed


##package: pclag
#GES
#FCI
#PC method= "stable"

######### FEATURE SELECTION FOR CATEGORICAL DATA ############----
set.seed(123)  # For reproducibility

# ----- PARAMETERS -----


# Define the dataset and target variable.
# 'per_data_analysis.binary' is the full dataset and the target is "dfs_adoption_binary".

# ----- FEATURE SELECTION FUNCTION -----
get_selected_features <- function(bn_model, target = "dfs_adoption_binary") {
  # Get all ancestors of the target node.
  anc <- ancestors(bn_model, target)
  
  # Convert the arcs of the network into a data frame.
  arcs_df <- as.data.frame(bn_model$arcs)
  
  # Extract direct predictors: nodes where the arc points to the target.
  if (nrow(arcs_df) > 0 && "to" %in% names(arcs_df)) {
    preds <- as.character(arcs_df[arcs_df$to == target, "from"])
  } else {
    preds <- character(0)
  }
  
  # Combine ancestors, direct predictors, and the target itself.
  causal_factors <- unique(c(anc, preds, target))
  return(causal_factors)
}

# ----- ALGORITHMS AND PARAMETERS GRID -----
alphas <- c(0.05, 0.01)
tests <- c("mi")
algorithms <- c("pc.stable", "gs", "iamb", "fast.iamb", "inter.iamb", "iamb.fdr", "mmhc")
n_rep <- 10   # number of CV repetitions
k <- 10      # 10-fold CV

# Initialize a list to store selected features for each algorithm-parameter combination.
# We will store one feature set per fold across all repetitions.
selected_features <- list()
for (alg in algorithms) {
  for (alpha in alphas) {
    for (test in tests) {
      key <- paste(alg, "alpha", alpha, "test", test, sep = "_")
      selected_features[[key]] <- list()
    }
  }
}


# ----- REPEATED 10-FOLD CROSS-VALIDATION -----
rep_index <- 1  # to index each fold across repetitions
for (r in 1:n_rep) {
  cat("Repetition:", r, "\n")
  
  # Create 10 stratified folds for the current repetition.
  folds <- createFolds(per_data_analysis.binary$dfs_adoption_binary, k = k, returnTrain = TRUE)
  
  for (fold_index in 1:k) {
    cat("  Fold:", fold_index, "\n")
    # Get training indices for this fold.
    train_idx <- folds[[fold_index]]
    train_fold <- per_data_analysis.binary[train_idx, ]
    
    # For each combination of algorithm, alpha, and test, learn the BN and extract features.
    for (alg in algorithms) {
      for (alpha in alphas) {
        for (test in tests) {
          key <- paste(alg, "alpha", alpha, "test", test, sep = "_")
          
          # Learn the Bayesian network on the training fold.
          if (alg == "pc.stable") {
            bn_model <- pc.stable(train_fold, test = test, alpha = alpha)
          } else if (alg == "gs") {
            bn_model <- gs(train_fold, test = test, alpha = alpha)
          } else if (alg == "iamb") {
            bn_model <- iamb(train_fold, test = test, alpha = alpha)
          } else if (alg == "fast.iamb") {
            bn_model <- fast.iamb(train_fold, test = test, alpha = alpha)
          } else if (alg == "inter.iamb") {
            bn_model <- inter.iamb(train_fold, test = test, alpha = alpha)
          } else if (alg == "iamb.fdr") {
            bn_model <- iamb.fdr(train_fold, test = test, alpha = alpha)
          } else if (alg == "mmhc") {
            bn_model <- mmhc(train_fold)
          }
          
          # Extract the selected features for the target variable.
          feats <- get_selected_features(bn_model, target = "dfs_adoption_binary")
          # Store the result using a global index across all folds.
          selected_features[[key]][[rep_index]] <- feats
          
          cat("    ", key, "- selected features:", paste(feats, collapse = ", "), "\n")
        }
      }
    }
    rep_index <- rep_index + 1
  }
  cat("\n")
}


# Convert selected_features into a dataframe with separate repetition and fold
results_df <- do.call(rbind, lapply(names(selected_features), function(key) {
  # Extract subset, algorithm, alpha, and test correctly
  key_parts <- unlist(strsplit(key, "_alpha_|_test_"))  # Split by "alpha_" and "test_"
  
  subset_algo <- key_parts[1]  # Contains subset + algorithm (need to split further)
  subset_algo_parts <- unlist(strsplit(subset_algo, "_"))
  
  subset_name <- paste(subset_algo_parts[1:(length(subset_algo_parts)-1)], collapse = "_")  # Everything except last
  algorithm <- subset_algo_parts[length(subset_algo_parts)]  # Last part is algorithm
  
  alpha_value <- as.numeric(key_parts[2])  # Extract alpha
  test_type <- key_parts[3]  # Extract test
  
  # Iterate over repetitions and folds
  do.call(rbind, lapply(seq_along(selected_features[[key]]), function(rep_fold_index) {
    rep_number <- ceiling(rep_fold_index / k)  # Calculate repetition index
    fold_number <- (rep_fold_index - 1) %% k + 1  # Calculate fold index within the repetition
    
    data.frame(
      #Subset = subset_name,
      Algorithm = algorithm,
      Alpha = alpha_value,
      Test = test_type,
      Repetition = rep_number,  # Now explicitly separating repetition
      Fold = fold_number,  # Now explicitly separating fold
      Selected_Factors = paste(selected_features[[key]][[rep_fold_index]], collapse = ", "),
      stringsAsFactors = FALSE
    )
  }))
}))

# Display results as dataframe
print(results_df)

# ----- STABILITY ASSESSMENT: Compute Pairwise Jaccard Similarity -----
jaccard <- function(set1, set2) {
  length(intersect(set1, set2)) / length(union(set1, set2))
}

stability_scores <- list()
for (alg in algorithms) {
  for (alpha in alphas) {
    for (test in tests) {
      key <- paste(alg, "alpha", alpha, "test", test, sep = "_")
      feats_list <- selected_features[[key]]
      sims <- c()
      num_folds <- length(feats_list)
      
      # Compute pairwise Jaccard similarity for all fold combinations.
      for (i in 1:(num_folds - 1)) {
        for (j in (i + 1):num_folds) {
          sims <- c(sims, jaccard(feats_list[[i]], feats_list[[j]]))
        }
      }
      stability_scores[[key]] <- mean(sims)
    }
  }
}

cat("\nStability Scores (average Jaccard similarity):\n")
for (key in names(stability_scores)) {
  cat(key, ":", stability_scores[[key]], "\n")
}

# ----- IDENTIFY STABLE FEATURES -----
# Define a threshold for stability (e.g., features appearing in at least 80% of folds).
stable_features <- list()
threshold <- 0.2
for (alg in algorithms) {
  for (alpha in alphas) {
    for (test in tests) {
      key <- paste(alg, "alpha", alpha, "test", test, sep = "_")
      feats_list <- selected_features[[key]]
      n_folds <- length(feats_list)
      
      # Count frequency of each feature across all folds.
      feature_freq <- table(unlist(feats_list))
      
      # A feature is stable if it appears in at least threshold * n_folds folds.
      min_occurrences <- ceiling(threshold * n_folds)
      stable_features[[key]] <- names(feature_freq)[feature_freq >= min_occurrences]
    }
  }
}

cat("\nStable Features (appearing in at least", threshold*100, "% of folds) for each combination:\n")
for (key in names(stable_features)) {
  cat(key, ":", paste(stable_features[[key]], collapse = ", "), "\n")
}

# Convert the stable_features list to a data frame.
stable_df <- do.call(rbind, lapply(names(stable_features), function(key) {
  data.frame(Combination = key,
             Selected_Factors = paste(stable_features[[key]], collapse = ", "),
             stringsAsFactors = FALSE)
}))

# View the data frame
print(stable_df)

























#####################################
# ----- STEP 1: Create Stratified k-Fold Splits -----
# 'per_data_analysis.binary' is the full dataset
# and that the target variable is "dfs_adoption_binary".
k <- 4  # number of folds
folds <- createFolds(per_data_analysis.binary$dfs_adoption_binary, k = k, returnTrain = TRUE)


# ----- STEP 2: Define the Feature Selection Procedure -----
# For each BN, we will extract:
# - the ancestors of "dfs_adoption_binary" (all nodes that have a directed path to it)
# - the direct predictors (nodes that have an arc pointing directly to the target)
# - and then include the target itself i.e., "dfs_adoption_binary".
get_selected_features <- function(bn_model, target = "dfs_adoption_binary") {
  # Get all ancestors of the target node.
  anc <- ancestors(bn_model, target)
  
  # Convert the arcs of the network into a data frame.
  arcs_df <- as.data.frame(bn_model$arcs)
  
  # Extract direct predictors: nodes where the arc points to the target.
  if (nrow(arcs_df) > 0 && "to" %in% names(arcs_df)) {
    preds <- as.character(arcs_df[arcs_df$to == target, "from"])
  } else {
    preds <- character(0)
  }
  
  # Combine ancestors, direct predictors, and the target itself.
  causal_factors <- unique(c(anc, preds, target))
  return(causal_factors)
}

# List of algorithms to test.
algorithms <- c("pc.stable", "gs", "aimb", "fast.iamb","inter.iamb","iamb.fdr","hc")

# Initialize a list to store selected features for each algorithm across folds.
selected_features <- list()
for (alg in algorithms) {
  selected_features[[alg]] <- list()
}



# ----- STEP 3: Run Stratified CV and Feature Selection -----
for (alg in algorithms) {
  cat("Algorithm:", alg, "\n")
  for (fold_index in 1:k) {
    # Get training indices for this fold.
    train_idx <- folds[[fold_index]]
    train_fold <- per_data_analysis.binary[train_idx, ]
    
    # Learn the Bayesian network using the specified algorithm.
    if (alg == "pc.stable") {
      bn_model <- pc.stable(train_fold)
    } else if (alg == "gs") {
      bn_model <- gs(train_fold)
    } else if (alg == "aiam") {
      bn_model <- aimb(train_fold)
    } else if (alg == "fast.iamb") {
      bn_model <- fast.iamb(train_fold)
    } else if (alg == "inter.iamb") {
      bn_model <- inter.iamb(train_fold)
    } else if (alg == "iamb.fdr") {
      bn_model <- iamb.fdr(train_fold)
    } else if (alg == "hc") {
      bn_model <- hc(train_fold)
    }
    
    # Extract the selected features for dfs_adoption_binary.
    feats <- get_selected_features(bn_model, target = "dfs_adoption_binary")
    selected_features[[alg]][[fold_index]] <- feats
    
    cat(" Fold", fold_index, "selected features:", paste(feats, collapse = ", "), "\n")
  }
}

# ----- STEP 4: Compute Stability via Pairwise Jaccard Similarity -----
# Define a function to compute the Jaccard similarity between two sets.
jaccard <- function(set1, set2) {
  length(intersect(set1, set2)) / length(union(set1, set2))
}

# For each algorithm, compute the average pairwise Jaccard similarity across the folds.
stability_scores <- list()
for (alg in algorithms) {
  feats_list <- selected_features[[alg]]
  sims <- c()
  num_folds <- length(feats_list)
  
  for (i in 1:(num_folds - 1)) {
    for (j in (i + 1):num_folds) {
      sims <- c(sims, jaccard(feats_list[[i]], feats_list[[j]]))
    }
  }
  stability_scores[[alg]] <- mean(sims)
}

# ----- STEP 5: Report the Stability Scores -----
cat("\nStability Scores (average Jaccard similarity):\n")
for (alg in algorithms) {
  cat(alg, ":", stability_scores[[alg]], "\n")
}


# ----- STEP 6: Compute the List of Stable Features for Each Method with 80% Threshold -----
stable_features <- list()
threshold <- 0.5  # Define the threshold (80% of folds)

for (alg in algorithms) {
  # Get the list of feature sets from each fold for this algorithm.
  feats_list <- selected_features[[alg]]
  n_folds <- length(feats_list)
  
  # Count frequency of each feature across folds.
  feature_freq <- table(unlist(feats_list))
  
  # A feature is stable if it appears in at least 80% of the folds.
  min_occurrences <- ceiling(threshold * n_folds)
  stable_features[[alg]] <- names(feature_freq)[feature_freq >= min_occurrences]
}

# Print out the stable features for each algorithm.
cat("\nStable Features (appearing in at least 80% of folds) for each method:\n")
for (alg in algorithms) {
  cat(alg, ":", paste(stable_features[[alg]], collapse = ", "), "\n")
}





###############################################################
######### FEATURE SELECTION FOR MIXED DATA ############----

library(MXM)



#https://www.datacamp.com/tutorial/feature-selection-R-boruta
library("Boruta")
library("mlbench")

#The Boruta algorithm is a wrapper built around the random forest classification algorithm.
#It tries to capture all the important, interesting features you might have in your dataset with 
#respect to an outcome variable.
# Does not accept NA values
# Focus on Strong Direct Effects

str(per_data_analysis.binary)
set.seed(1)
Boruta <- Boruta(dfs_adoption_binary ~ .,  data= per_data_analysis.binary, doTrace = 2, ntree = 500,pValue=0.01)
Boruta
getConfirmedFormula(Boruta)
plot(Boruta)

x<- as.data.frame(attStats(Boruta))%>%
  filter(decision=="Confirmed")
x
plot(Boruta)
confirmed_factors <- x[, 1]
Boruta.Short <- Boruta(dfs_adoption_binary ~ ., data = per_data_analysis.binary, maxRuns = 500)
Boruta.Short
plot(Boruta.Short)


TentativeRoughFix(Boruta.Short)





highlight_list <- list(nodes = "dfs_adoption_binary", col = "red", fill = "yellow")

## Hybrid Learning Algorithms
mmhc <- gs(per_data_analysis.binary)
mmhc
plot(mmhc, main = "mmhc",highlight= "dfs_adoption_binary")
graphviz.plot(mmhc, main = "mmhc", highlight = highlight_list)

ancestors <- ancestors(mmhc, "dfs_adoption_binary")
ancestors

arcs_mmhc <- as.data.frame(mmhc$arcs)  # Convert to data frame
predictors.mmhc <- arcs_mmhc[arcs_mmhc$to == "dfs_adoption_binary", ]  
predictors.mmhc

causal_factors<-c(ancestors,predictors.mmhc$from)
causal_factors

# Include the target itself.
nodes_to_plot <- c(unique(causal_factors), "dfs_adoption_binary")
nodes_to_plot

# Create the induced subgraph for these nodes.
bn_sub <- subgraph(mmhc, nodes = nodes_to_plot)
bn_sub <- cextend(bn_sub)

# Plot the subgraph, highlighting dfs_adoption_binary.
graphviz.plot(bn_sub, main = "Ancestors of dfs_adoption_binary")

data_sub <- per_data_analysis.binary[, nodes(bn_sub)]


# Evaluate the BN structure using BIC and AIC.
bic_score <- score(bn_sub, data_sub, type = "bic")
aic_score <- score(bn_sub, data_sub, type = "aic")

cat("BIC score for the subgraph:", bic_score, "\n")
cat("AIC score for the subgraph:", aic_score, "\n")



arcs_mmhc <- as.data.frame(mmhc$arcs)  # Convert to data frame
predictors.mmhc <- arcs_mmhc[arcs_mmhc$to == "dfs_adoption_binary", ]  
predictors.mmhc

h2pc <- h2pc(per_data_analysis.binary)
h2pc
graphviz.plot(h2pc, main = "mmhc", highlight = highlight_list)

ancestors(h2pc, "dfs_adoption_binary")

arcs_h2pc <- as.data.frame(h2pc$arcs)  # Convert to data frame
predictors.h2pc <- arcs_h2pc[arcs_h2pc$to == "dfs_adoption_binary", ]  
predictors.h2pc

## Constraint base methods
hpc <- hpc(per_data_analysis.binary, alpha= 0.05,test="mi-cg")
hpc
graphviz.plot(hpc, main = "mmhc", highlight = highlight_list)

ancestors(hpc, "dfs_adoption_binary")

arcs_hpc <- as.data.frame(hpc$arcs)  # Convert to data frame
predictors.hpc <- arcs_hpc[arcs_hpc$to == "dfs_adoption_binary", ]  
predictors.hpc

si.hiton.pc <- si.hiton.pc(per_data_analysis.binary, alpha= 0.05,test="mi-cg")
si.hiton.pc
graphviz.plot(si.hiton.pc, main = "mmhc", highlight = highlight_list)

ancestors(si.hiton.pc, "dfs_adoption_binary")

arcs_si.hiton.pc <- as.data.frame(si.hiton.pc$arcs)  # Convert to data frame
predictors.si.hiton.pc<- arcs_si.hiton.pc[arcs_si.hiton.pc$to == "dfs_adoption_binary", ]  
predictors.si.hiton.pc



#######
x<- as.data.frame(attStats(Boruta))%>%
  filter(decision=="Confirmed")
x
confirmed_factors <- rownames(x)


factors.selected <-per_data_analysis.binary%>%
  select(all_of(confirmed_factors))

mmhc2 <- mmhc(factors.selected)
mmhc2
causal_factors2 <- ancestors(mmhc2, "dfs_adoption_binary")
causal_factors2


arcs_mmhc2 <- as.data.frame(mmhc2$arcs)  # Convert to data frame
predictors.mmhc2 <- arcs_mmhc2[arcs_mmhc2$to == "dfs_adoption_binary", ]  
predictors.mmhc2

hpc2 <- hpc(factors.selected)
hpc2

ancestors(hpc2, "dfs_adoption_binary")

arcs_hpc2 <- as.data.frame(hpc2$arcs)  # Convert to data frame
predictors.hpc2 <- arcs_hpc2[arcs_hpc2$to == "dfs_adoption_binary", ]  
predictors.hpc2




######### FEATURE SELECTION FOR CONTINUOUS DATA ############----
## Constraint-based structure learning algorithms
# "pc.stable", "gs", "iamb", "fast.iamb","inter.iamb","iamb.fdr","mmpc","si.hiton.pc","hpc"
## Conditional independence test
# "mi-g", "mc-mi-g"

fun_feature_selection_cont <- function(data, target_var) {
  # Define settings
  tests <- c("mi-cg")  # Mutual information tests
  alphas <- c(0.05, 0.01)
  methods <- c("pc.stable", "gs", "iamb", "fast.iamb","inter.iamb","iamb.fdr","mmpc","si.hiton.pc","hpc")
  
  results <- list()
  
  # Loop through all combinations
  for (method in methods) {
    for (test in tests) {
      for (alpha in alphas) {
        
        cat("\nRunning", method, "with test =", test, "and alpha =", alpha, "...\n")
        
        # Run the structure learning algorithm
        model <- do.call(method, list(data, test = test, alpha = alpha))
        
        # Extract ancestors of the target variable
        ancestors_set <- ancestors(model, target_var)
        
        # Extract all arcs
        all_arcs<- as.data.frame(model$arcs)
        
        # Get all directed and undirected arcs to the target variable
        directed_undirected_paths<- all_arcs[all_arcs$to == target_var, ]
        
        
        # Save results
        key <- paste(method, test, "alpha", alpha, sep = "_")
        results[[key]] <- list(
          model = model,
          ancestors = ancestors_set,
          directed_undirected_paths= directed_undirected_paths
        )
        
        # Print Ancestors of
        cat("Ancestors of", target_var, ":\n")
        print(ancestors_set)
        
        #Print direct and undirected paths
        cat("directed and undirected", target_var, ":\n")
        print(paste(directed_undirected_paths$from, "→", directed_undirected_paths$to))
        
      }
    }
  }
  
  return(results)
}

# Run the function
results <- fun_feature_selection_cont(per_data_analysis.binary, "dfs_adoption_binary")

# Print results
results

# Function to extract arcs where "dfs_adoption_binary" is the child (predictors)
get_adoption_arcs <- function(model, target_var) {
  arcs_df <- as.data.frame(model$arcs)  # Convert to data frame
  predictors <- arcs_df[arcs_df$to == target_var, ]  # Keep only parent nodes
  return(predictors)
}

# Apply to all models
adoption_arcs_list <- lapply(results, function(x) get_adoption_arcs(x$model, "dfs_adoption_binary"))
adoption_arcs_list

# Count occurrences of each predictor across models
adoption_feature_counts <- table(unlist(adoption_arcs_list))
adoption_feature_counts
# Print frequency of predictors
print("Predictors of DFS Adoption (Appearing in Multiple Models):")
print(adoption_feature_counts)

# Select features appearing in at least 2 models
selected_adoption_features <- names(adoption_feature_counts[adoption_feature_counts >= 2])

print("Final Selected Predictors for DFS Adoption:")
print(selected_adoption_features)



######### FEATURE SELECTION FOR CATEGORICAL DATA ############----
## Constraint-based structure learning algorithms
# "pc.stable", "gs", "iamb", "fast.iamb","inter.iamb","iamb.fdr","mmpc","si.hiton.pc","hpc"
## Conditional independence test
# "mi and mi-adf", "mc-mi","smc-mi

fun_feature_selection_categ <- function(data, target_var) {
  # Define settings
  tests <- c("mi and mi-adf","mc-mi","smc-mi","sp-mi")  # Mutual information tests
  alphas <- c(0.05, 0.01)
  methods <- c("pc.stable", "gs", "iamb", "fast.iamb","inter.iamb","iamb.fdr","mmpc","si.hiton.pc","hpc")
  
  results <- list()
  
  # Loop through all combinations
  for (method in methods) {
    for (test in tests) {
      for (alpha in alphas) {
        
        cat("\nRunning", method, "with test =", test, "and alpha =", alpha, "...\n")
        
        # Run the structure learning algorithm
        model <- do.call(method, list(data, test = test, alpha = alpha))
        
        # Extract ancestors of the target variable
        ancestors_set <- ancestors(model, target_var)
        
        # Extract all arcs
        all_arcs<- as.data.frame(model$arcs)
        
        # Get all directed and undirected arcs to the target variable
        directed_undirected_paths<- all_arcs[all_arcs$to == target_var, ]
        
        
        # Save results
        key <- paste(method, test, "alpha", alpha, sep = "_")
        results[[key]] <- list(
          model = model,
          ancestors = ancestors_set,
          directed_undirected_paths= directed_undirected_paths
        )
        
        # Print Ancestors of
        cat("Ancestors of", target_var, ":\n")
        print(ancestors_set)
        
        #Print direct and undirected paths
        cat("directed and undirected", target_var, ":\n")
        print(paste(directed_undirected_paths$from, "→", directed_undirected_paths$to))
        
      }
    }
  }
  
  return(results)
}

# Run the function
results <- fun_feature_selection_cont(per_data_analysis, "dfs_adoption_binary")

# Print results
results

################




library(bnlearn)

pc.mig5<- fast.iamb(per_data_analysis, alpha = 0.01, test="mi-g")
pc.mig5

plot(pc.mig5, main = "pc.mi-g",highlight= "dfs_adoption_binary")
graphviz.plot(pc.mig5, main = "pc.mig5", highlight = highlight_list)

# Extract all arcs (edges) from the Bayesian Network
all_arcs <- as.data.frame(pc.mig5$arcs)
all_arcs
# Step 1: Extract **directed direct paths** (one-step)
directed_direct_paths <- all_arcs[all_arcs$to == "dfs_adoption_binary", ]
directed_direct_paths
# Step 2: Extract **directed indirect paths** (multi-step)
directed_ancestors <- ancestors(pc.mig5, "dfs_adoption_binary")
directed_ancestors
directed_indirect_paths <- all_arcs[all_arcs$from %in% directed_ancestors & all_arcs$to != "dfs_adoption_binary", ]
directed_indirect_paths

# Format outputs
directed_direct_paths_str <- paste(directed_direct_paths$from, "→", directed_direct_paths$to)
directed_indirect_paths_str <- paste(directed_indirect_paths$from, "→", directed_indirect_paths$to)

# Print results
print("🔹 Fully Directed Direct Paths to DFS Adoption:")
print(directed_direct_paths_str)

print("🔹 Fully Directed Indirect Paths to DFS Adoption:")
print(directed_indirect_paths_str)





plot(pc.mig5, main = "pc.mi-g",highlight= "dfs_adoption_binary")
graphviz.plot(pc.mig5, main = "pc.mig5", highlight = highlight_list)


# Extract ancestors of "dfs_adoption_binary"
pc_predictors <- ancestors(pc.mig5, "dfs_adoption_binary")
pc_predictors

# Step 1: Extract the subgraph containing only the ancestors and the target variable
subgraph_nodes <- c(pc_predictors, "dfs_adoption_binary")

# Step 2: Create the subgraph from the learned Bayesian Network
pc_subgraph <- subgraph(pc.mig5, nodes = subgraph_nodes)

# Step 3: Highlight the target variable for better visualization
highlight_list <- list(nodes = "dfs_adoption_binary", col = "red", fill = "yellow")

# Step 4: Plot the subgraph
graphviz.plot(pc_subgraph, main = "Ancestors of DFS Adoption", highlight = highlight_list)



get_all_adoption_arcs <- function(model, target_var) {
  arcs_df <- as.data.frame(model$arcs)  # Convert arcs to a data frame
  related_nodes <- arcs_df[arcs_df$to == target_var | arcs_df$from == target_var, ]  # Keep connected nodes
  return(unique(c(related_nodes$from, related_nodes$to)))  # Capture all nodes in the link
}

all_related_nodes <- get_all_adoption_arcs(pc.mig5, "dfs_adoption_binary")
print("Nodes Connected to DFS Adoption (Directed & Undirected):")
print(all_related_nodes)
pc_predictors


all_influences <- unique(c(pc_predictors, all_related_nodes))
all_influences




library(Rgraphviz)





# Define nodes to keep (Markov Blanket + Target Variable)
subgraph_nodes <- c(mb_predictors, "dfs_adoption_binary")

# Extract subgraph containing only the Markov Blanket
pc_subgraph <- subgraph(pc.mig5, nodes = subgraph_nodes)

# Properly format highlight argument as a list
highlight_list <- list(nodes = "dfs_adoption_binary", col = "red", fill = "yellow")

# Plot the Markov Blanket subgraph
graphviz.plot(pc_subgraph, main = "Markov Blanket of DFS Adoption", highlight = highlight_list)


############
# Extract direct predictors (Parents of DFS adoption)
direct_predictors <- parents(pc.mig5, "dfs_adoption_binary")
direct_predictors
# Extract indirect predictors (Ancestors of DFS adoption)
indirect_predictors <- ancestors(pc.mig5, "dfs_adoption_binary")
indirect_predictors
# Merge both sets of predictors
all_predictors <- unique(c(direct_predictors, indirect_predictors))
all_predictors
print("Direct Predictors of DFS Adoption:")
print(direct_predictors)

print("Indirect Predictors of DFS Adoption:")
print(indirect_predictors)

print("All Predictors (Direct + Indirect):")
print(all_predictors)

library(Rgraphviz)

# Define nodes to keep (Direct + Indirect Predictors + Target Variable)
subgraph_nodes <- c(all_predictors, "dfs_adoption_binary")

# Extract subgraph from the full Bayesian Network
pc_subgraph <- subgraph(pc.mig5, nodes = subgraph_nodes)
pc_subgraph

highlight_list <- list(nodes = "dfs_adoption_binary", col = "red", fill = "yellow")


# Plot the subgraph
graphviz.plot(pc_subgraph, main = "Direct & Indirect Predictors of DFS Adoption", highlight = highlight_list)





from                  to
22       soil_slope_perception dfs_adoption_binary
44 num_info_exchange_extension dfs_adoption_binary



