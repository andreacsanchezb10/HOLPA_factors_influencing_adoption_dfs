library(summarytools)
library(readxl)
library(dplyr)
library(caret)
library(ggplot2)

#############################################################    
########## UPLOAD DATA #####-----
#############################################################
factors_list_analysis<-read_excel("factors_list.xlsx",sheet = "factors_list_analysis")
sort(unique(factors_list_analysis$category_1))

per_data_indirect<-read.csv("per_data_logistic_regression_direct.csv",sep=",")
rownames(per_data_indirect) <- per_data_indirect$X
per_data_indirect<-per_data_indirect

per_data_analysis<-  read.csv("per_data_Binary.csv",sep=",")
rownames(per_data_analysis) <- per_data_analysis$X
per_data_analysis<- per_data_analysis%>%
  dplyr::select(-X)%>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

dim(per_data_analysis) #200 farmers; 18 outcomes; 269 factors
#[1] 200 287

per_data_analysis<-per_data_analysis
  cbind(per_data_indirect)
dim(per_data_analysis) #200 farmers; 18 outcomes; 269 factors
#[1] 200 287

#############################################################    
########### FACTOR SELECTION ----
#############################################################
##=== STEP 1: REMOVE IRRELEVANT FACTORS ======
##=== STEP 2: REMOVE ZERO AND NEAR ZERO VARIANCE FACTORS ======
##=== STEP 3: REMOVE REDUNDANT FACTORS ======

feature_selection <- function(factors_list_analysis,remove_colum, data_analysis) {
  ##=== STEP 1: REMOVE IRRELEVANT FACTORS ======
  irrelevant_list<- intersect(factors_list_analysis$column_name_new[factors_list_analysis[[remove_colum]] %in%c("irrelevant")],colnames(data_analysis))
  
  data_irrelevantFiltered<- data_analysis%>%
    dplyr::select(-all_of(irrelevant_list))
  
  ##=== STEP 2: REMOVE ZERO AND NEAR ZERO VARIANCE FACTORS ======
  #In some situations, the data generating mechanism can create predictors that only have a 
  #single unique value (i.e. a “zero-variance predictor”). For many models (excluding tree-based models),
  #this may cause the model to crash or the fit to be unstable.
  #Similarly, predictors might have only a handful of unique values that occur with very low frequencies.
  ## frequency ratio: would be near one for well-behaved predictors and very large for highly-unbalanced data.
  ## percent of unique values: is the number of unique values divided by the total number of samples (times 100)
  #that approaches zero as the granularity of the data increases
  nzv <- caret::nearZeroVar(data_irrelevantFiltered, saveMetrics= TRUE) 
  nzv_list <- nearZeroVar(data_irrelevantFiltered)
  nzv_factors<- data_irrelevantFiltered[, nzv_list]

  nzv_factors<-as.data.frame(c(colnames(nzv_factors)))%>%
    rename("column_name_new"="c(colnames(nzv_factors))")%>%
    left_join(factors_list_analysis%>%select(category_1,column_name_new,constructs,constructs_type), by="column_name_new")
  ## Remove nzv variables from data
  data_nzvFiltered<- data_irrelevantFiltered[, -nzv_list]

  ##=== STEP 3: REMOVE REDUNDANT FACTORS ======
  redundant_list<- intersect(factors_list_analysis$column_name_new[factors_list_analysis[[remove_colum]] %in%c( "redundant" )],colnames(data_nzvFiltered))

  data_redundantFiltered<- data_nzvFiltered%>%
    dplyr::select(-all_of(redundant_list))
  

  return(data_redundantFiltered)
  
  }
##=== Run household_shock_recover_capacity ----
per_household_shock_recover_capacity_redundantFiltered<-feature_selection(factors_list_analysis, "peru_remove_household_shock_recover_capacity",per_data_analysis )
dim(per_household_shock_recover_capacity_redundantFiltered)#200 farmers; 1 outcomes, 104 factors retained
#[1] 200 105
names(per_household_shock_recover_capacity_redundantFiltered)


##=== Run for governance_capacity ====
per_governance_capacity_redundantFiltered<-feature_selection(factors_list_analysis, "peru_remove_governance_capacity",per_data_analysis )
dim(per_governance_capacity_redundantFiltered)#200 farmers; 1 outcomes, 79 factors retained
#[1] 200  80
names(per_governance_capacity_redundantFiltered)

##=== Run for training_participation ====
per_training_participation_redundantFiltered<-feature_selection(factors_list_analysis, "peru_remove_training_participation",per_data_analysis )
  select(-governance_capacity)
dim(per_training_participation_redundantFiltered)#200 farmers; 1 outcomes, 29 factors retained
#[1] 200  30
names(per_training_participation_redundantFiltered)

##=== STEP 4: CHECK FOR CORRELATION ACROSS FACTORS ======
# Function to calculate Spearman's correlation
create_cor_df <- function(data, factors_list_analysis) {
  cor_matrix <- cor(data %>% mutate(across(everything(), as.numeric)),
                    method = "spearman", use = "pairwise.complete.obs")
  
  cor_df <- as.data.frame(cor_matrix) %>%
    rownames_to_column("factor1") %>%
    pivot_longer(-factor1, names_to = "factor2", values_to = "spearman_correlation") %>%
    left_join(factors_list_analysis %>% select(column_name_new, category_1), by = c("factor1" = "column_name_new")) %>%
    rename(category_1.factor1 = category_1) %>%
    left_join(factors_list_analysis %>% select(column_name_new, category_1), by = c("factor2" = "column_name_new")) %>%
    rename(category_1.factor2 = category_1)
  
  return(cor_df)
}

# Function to plot correlated factors between categories
plot_correlation_betw_category <- function(cor_df) {
  categories <- unique(c(cor_df$category_1.factor1, cor_df$category_1.factor2))
  plots <- list()
  
  # All unique combinations of categories (including cross-category)
  category_pairs <- combn(categories, 2, simplify = FALSE)
  
  for (pair in category_pairs) {
    cat1 <- pair[1]
    cat2 <- pair[2]
    
    # Get all correlations where both variables are from cat1 or cat2
    cat_data <- cor_df %>%
      filter(
        (category_1.factor1 %in% c(cat1, cat2)) &
          (category_1.factor2 %in% c(cat1, cat2))
      )
    
    if (nrow(cat_data) > 0) {
      plot_title <- paste("Category:", cat1, "&", cat2, "- All Correlations")
      
      plots[[plot_title]] <- ggplot(cat_data, aes(x = factor1, y = factor2, fill = spearman_correlation)) +
        geom_tile(color = "white") +
        geom_text(data = cat_data %>% filter(abs(spearman_correlation) >= 0.8),
                  aes(label = round(spearman_correlation, 2)), size = 5) +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                             limits = c(-1, 1), name = "Spearman\nCorrelation") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.text.y = element_text(size = 8),
              plot.title = element_text(hjust = 0.5)) +
        labs(title = plot_title, x = NULL, y = NULL)
    }
  }
  
  return(plots)
}

##--- Run for household_shock_recover_capacity----
per_household_shock_recover_capacity_factors_list <- as.data.frame(colnames(per_household_shock_recover_capacity_redundantFiltered))%>%
  rename("column_name_new"= "colnames(per_household_shock_recover_capacity_redundantFiltered)")%>%
  left_join(factors_list_analysis%>%select(column_name_new, category_1),by="column_name_new")%>%
  filter(category_1!="outcome")

per_household_shock_recover_capacity_redundantFiltered_cor<-create_cor_df(per_household_shock_recover_capacity_redundantFiltered,per_household_shock_recover_capacity_factors_list)
str(per_household_shock_recover_capacity_redundantFiltered_cor)

plot_correlation_betw_category(per_household_shock_recover_capacity_redundantFiltered_cor)


##--- Run for governance_capacity----
per_governance_capacity_factors_list <- as.data.frame(colnames(per_governance_capacity_redundantFiltered))%>%
  rename("column_name_new"= "colnames(per_governance_capacity_redundantFiltered)")%>%
  left_join(factors_list_analysis%>%select(column_name_new, category_1),by="column_name_new")%>%
  filter(category_1!="outcome")

per_per_governance_redundantFiltered_cor<-create_cor_df(per_governance_capacity_redundantFiltered,per_per_governance_capacity_factors_list)
str(per_per_governance_redundantFiltered_cor)

plot_correlation_betw_category(per_per_governance_redundantFiltered_cor)

##--- Run for training_participation ====
per_training_participation_factors_list <- as.data.frame(colnames(per_training_participation_redundantFiltered))%>%
  rename("column_name_new"= "colnames(per_training_participation_redundantFiltered)")%>%
  left_join(factors_list_analysis%>%select(column_name_new, category_1),by="column_name_new")%>%
  filter(category_1!="outcome")

per_training_participation_redundantFiltered_cor<-create_cor_df(per_training_participation_redundantFiltered,per_training_participation_factors_list)
str(per_training_participation_redundantFiltered_cor)

plot_correlation_betw_category(per_training_participation_redundantFiltered_cor)

##=== STEP 5: FUZZY FOREST FACTOR SELECTION ======
## Advantages
# - The fuzzy forests algorithm is an extension of random forests designed to obtain less biased feature selection in the presence of correlated features. 
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

# Load libraries
library(mvtnorm)
library(Matrix)
library(mt)
library(WGCNA)


# Reusable function to convert factors to numeric and transpose
prepare_numeric_matrix <- function(df) {
  df %>%
    mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
    as.matrix() %>%
    t()
}

# Function to run WGCNA soft threshold selection
run_soft_threshold <- function(data_numeric, powers = c(1:10, seq(12, 20, 2)), dataset_name = "Dataset") {
  sft <- pickSoftThreshold(data_numeric, powerVector = powers, verbose = 5)
  par(mfrow = c(1, 2))
  cex1 <- 0.9
  
  plot(sft$fitIndices[, 1],
       -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2],
       xlab = "Soft Threshold (power)",
       ylab = "Scale Free Topology Model Fit, signed R^2",
       main = paste(dataset_name, " - Scale Independence"))
  text(sft$fitIndices[, 1],
       -sign(sft$fitIndices[, 3]) * sft$fitIndices[, 2],
       labels = powers, cex = cex1, col = "red")
  abline(h = 0.90, col = "red")
  
  plot(sft$fitIndices[, 1],
       sft$fitIndices[, 5],
       xlab = "Soft Threshold (power)",
       ylab = "Mean Connectivity",
       type = "n",
       main = paste(dataset_name, " - Mean Connectivity"))
  text(sft$fitIndices[, 1],
       sft$fitIndices[, 5],
       labels = powers, cex = cex1, col = "red")
  
  return(sft)
}

library("Metrics")
# Function to run feature selection algotithms

feature_selection_continuous_algorithms <- function( factors, Outcome, picked_power, file_name = "dataset") {
  library(e1071)
  library(caret)
  library(WGCNA)
  library(randomForest)
  library(party)
  library(fuzzyforest)
  library(tidyr)
  library(tibble)
  library(Metrics)
  
  feature_nums <- c(1:20)
  times <- 20 #number of runs
  
  acc_ff <- matrix(0, nrow = times, ncol = length(feature_nums))
  acc_rf <- matrix(0, nrow = times, ncol = length(feature_nums))
  acc_cf <- matrix(0, nrow = times, ncol = length(feature_nums))
  
  selected_ff <- list()
  selected_rf <- list()
  selected_cf <- list()
  
  for (j in seq_along(feature_nums)) {
    feats_ff <- c()
    feats_rf <- c()
    feats_cf <- c()
    
    for (i in 1:times) {
      set.seed(sample(1:1000, 1))
      train_index <- createDataPartition(Outcome, p = 0.7,   list = FALSE)
      train_data <- factors[train_index, ]
      test_data <- factors[-train_index, ]
      y_train <- Outcome[train_index]
      y_test <- Outcome[-train_index]
      
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
      
      ff_model <- wff(X = train_data, y = y_train,
                      WGCNA_params = WGCNA_params,
                      select_params = select_paramsWGCNA,
                      screen_params = screen_paramsWGCNA)
      feats_ff_i_all <- ff_model$feature_list[[1]]  # first component
      feats_ff_i <- feats_ff_i_all[1:min(length(feats_ff_i_all), feature_nums[j])]
      svm_ff_model <- svm(x = train_data[, feats_ff_i], y = y_train, kernel = "linear")
      pred_ff <- predict(svm_ff_model, newdata = test_data[, feats_ff_i])
      acc_ff[i, j] <- rmse(y_test, pred_ff)
      
      # === Random Forest ===
      rf_model <- randomForest(x = train_data, y = y_train, importance = TRUE, mtry = floor(sqrt(ncol(train_data))), ntree = 500)
      imp_rf <- importance(rf_model, type = 1, scale = FALSE)
      feats_rf_i <- rownames(head(imp_rf[order(imp_rf, decreasing = TRUE), ,drop = FALSE], feature_nums[j]))
      svm_rf_model <- svm(x = train_data[, feats_rf_i], y = y_train, kernel = "linear")
      pred_rf <- predict(svm_rf_model, newdata = test_data[, feats_rf_i])
      acc_rf[i, j] <- rmse(y_test, pred_rf)
      
      # === Conditional inference Forest ===
      cf_model <- cforest(y_train ~ ., data = data.frame(train_data, y_train),
                           controls = cforest_unbiased(ntree = 100, mtry = floor(sqrt(ncol(train_data)))))
      varimp_cf <- varimp(cf_model, conditional = TRUE)
      feats_cf_i <- names(sort(varimp_cf, decreasing = TRUE))[1:feature_nums[j]]
      svm_cf_model <- svm(x = train_data[, feats_cf_i], y = y_train, kernel = "linear")
      pred_cf <- predict(svm_cf_model, newdata = test_data[, feats_cf_i])
      acc_cf[i, j] <-  rmse(y_test, pred_cf)
      
      feats_ff <- c(feats_ff, paste(feats_ff_i, collapse = ","))
      feats_rf <- c(feats_rf, paste(feats_rf_i, collapse = ","))
      feats_cf <- c(feats_cf, paste(feats_cf_i, collapse = ","))
    }
    
    selected_ff[[paste0("featNum", feature_nums[j])]] <- feats_ff
    selected_rf[[paste0("featNum", feature_nums[j])]] <- feats_rf
    selected_cf[[paste0("featNum", feature_nums[j])]] <- feats_cf
  }
  
  acc_ff_df <- as.data.frame(rbind(acc_ff, colMeans(acc_ff)))
  acc_rf_df <- as.data.frame(rbind(acc_rf, colMeans(acc_rf)))
  acc_cf_df <- as.data.frame(rbind(acc_cf, colMeans(acc_cf)))
  colnames(acc_ff_df) <- colnames(acc_rf_df) <-colnames(acc_cf_df) <- paste0("featNum", feature_nums)
  rownames(acc_ff_df) <- rownames(acc_rf_df) <- rownames(acc_cf_df) <- c(paste0("acc", 1:times), "acc_mean")
  
  # Write to CSV
  write.csv(acc_ff_df, paste0("results/",file_name, "_accValAllFuzzyForest.csv"))
  write.csv(acc_rf_df, paste0("results/",file_name, "_accValAllRandomForest.csv"))
  write.csv(acc_cf_df, paste0("results/",file_name, "_accValAllCForest.csv"))
  write.csv(as.data.frame(selected_ff), paste0("results/",file_name, "_featureSelectedFuzzyForest.csv"))
  write.csv(as.data.frame(selected_rf), paste0("results/",file_name, "_featureSelectedRandomForest.csv"))
  write.csv(as.data.frame(selected_cf), paste0("results/",file_name, "_featureSelectedCForest.csv"))
  
  # Return data frames for plotting if needed
  list(
    acc_ff_df = acc_ff_df,
    acc_rf_df = acc_rf_df,
    acc_cf_df = acc_cf_df,
    selected_ff = selected_ff,
    selected_rf = selected_rf,
    selected_cf = selected_cf
  )
}

feature_selection_binary_algorithms <- function(factors, Outcome, picked_power, file_name = "dataset") {
  library(e1071)
  library(caret)
  library(WGCNA)
  library(randomForest)
  library(party)
  library(fuzzyforest)
  library(tidyr)
  library(tibble)
  
  feature_nums <- c(1:20)
  times <- 20 #number of runs
  
  acc_ff <- matrix(0, nrow = times, ncol = length(feature_nums))
  acc_rf <- matrix(0, nrow = times, ncol = length(feature_nums))
  acc_cf <- matrix(0, nrow = times, ncol = length(feature_nums))
  
  selected_ff <- list()
  selected_rf <- list()
  selected_cf <- list()
  
  for (j in seq_along(feature_nums)) {
    feats_ff <- c()
    feats_rf <- c()
    feats_cf <- c()
    
    for (i in 1:times) {
      set.seed(sample(1:1000, 1))
      train_index <- createDataPartition(Outcome, p = 0.7, list = FALSE)
      train_data <- factors[train_index, ]
      test_data <- factors[-train_index, ]
      y_train <- Outcome[train_index]
      y_test <- Outcome[-train_index]
      
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
      
      ff_model <- wff(X = train_data, y = as.factor(y_train),
                      WGCNA_params = WGCNA_params,
                      select_params = select_paramsWGCNA,
                      screen_params = screen_paramsWGCNA)
      feats_ff_i_all <- ff_model$feature_list[[1]]  # first component
      feats_ff_i <- feats_ff_i_all[1:min(length(feats_ff_i_all), feature_nums[j])]
      svm_ff_model <- svm(x = train_data[, feats_ff_i], y = as.factor(y_train), kernel = "linear")
      pred_ff <- predict(svm_ff_model, newdata = test_data[, feats_ff_i])
      acc_ff[i, j] <- mean(pred_ff == as.factor(y_test))
      
      # === Random Forest ===
      rf_model <- randomForest(x = train_data, y = as.factor(y_train), importance = TRUE, mtry = floor(sqrt(ncol(train_data))), ntree = 500)
      imp_rf <- importance(rf_model, type = 1, scale = FALSE)
      summary(imp_rf)  # View NAs
      which(is.na(imp_rf))  # List variables with NA importance
      feats_rf_i <- rownames(head(imp_rf[order(imp_rf, decreasing = TRUE), , drop = FALSE], feature_nums[j]))
      svm_rf_model <- svm(x = train_data[, feats_rf_i], y = as.factor(y_train), kernel = "linear")
      pred_rf <- predict(svm_rf_model, newdata = test_data[, feats_rf_i])
      acc_rf[i, j] <- mean(pred_rf == as.factor(y_test))
      
      # === Conditional inference Forest ===
      cf_model <- cforest(as.factor(y_train) ~ ., data = data.frame(train_data, y_train),
                          controls = cforest_unbiased(ntree = 100, mtry = floor(sqrt(ncol(train_data)))))
      varimp_cf <- varimp(cf_model, conditional = TRUE)
      feats_cf_i <- names(sort(varimp_cf, decreasing = TRUE))[1:feature_nums[j]]
      svm_cf_model <- svm(x = train_data[, feats_cf_i], y = as.factor(y_train), kernel = "linear")
      pred_cf <- predict(svm_cf_model, newdata = test_data[, feats_cf_i])
      acc_cf[i, j] <- mean(pred_cf == as.factor(y_test))
      
      feats_ff <- c(feats_ff, paste(feats_ff_i, collapse = ","))
      feats_rf <- c(feats_rf, paste(feats_rf_i, collapse = ","))
      feats_cf <- c(feats_cf, paste(feats_cf_i, collapse = ","))
    }
    
    selected_ff[[paste0("featNum", feature_nums[j])]] <- feats_ff
    selected_rf[[paste0("featNum", feature_nums[j])]] <- feats_rf
    selected_cf[[paste0("featNum", feature_nums[j])]] <- feats_cf
  }
  
  acc_ff_df <- as.data.frame(rbind(acc_ff, colMeans(acc_ff)))
  acc_rf_df <- as.data.frame(rbind(acc_rf, colMeans(acc_rf)))
  acc_cf_df <- as.data.frame(rbind(acc_cf, colMeans(acc_cf)))
  colnames(acc_ff_df) <- colnames(acc_rf_df) <- colnames(acc_cf_df) <- paste0("featNum", feature_nums)
  rownames(acc_ff_df) <- rownames(acc_rf_df) <- rownames(acc_cf_df) <- c(paste0("acc", 1:times), "acc_mean")
  
  # Write to CSV
  write.csv(acc_ff_df, paste0("results/",file_name, "_accValAllFuzzyForest.csv"))
  write.csv(acc_rf_df, paste0("results/",file_name, "_accValAllRandomForest.csv"))
  write.csv(acc_cf_df, paste0("results/",file_name, "_accValAllCForest.csv"))
  write.csv(as.data.frame(selected_ff), paste0("results/",file_name, "_featureSelectedFuzzyForest.csv"))
  write.csv(as.data.frame(selected_rf), paste0("results/",file_name, "_featureSelectedRandomForest.csv"))
  write.csv(as.data.frame(selected_cf), paste0("results/",file_name, "_featureSelectedCForest.csv"))
  
  # Return data frames for plotting if needed
  list(
    acc_ff_df = acc_ff_df,
    acc_rf_df = acc_rf_df,
    acc_cf_df = acc_cf_df,
    selected_ff = selected_ff,
    selected_rf = selected_rf,
    selected_cf = selected_cf
  )
}

# Function to plot accuracy vs number of selected features
plot_accuracy_vs_features <- function(acc_ff_df, acc_rf_df,acc_cf_df,method_name = "Model",xmax,xmin) {
  process_df <- function(df, algo_name) {
    df %>%
      rename("Run"="X")%>%
      filter(Run == "acc_mean") %>%
      tidyr::pivot_longer(-Run, names_to = "NumFeatures", values_to = "Accuracy") %>%
      mutate(
        NumFeatures = as.numeric(gsub("featNum", "", NumFeatures)),
        algorithm = algo_name
      )
  }
  
  acc_long <- bind_rows(
    process_df(acc_ff_df, "Fuzzy Forest"),
    process_df(acc_rf_df, "Random Forest"),
    process_df(acc_cf_df, "Conditional Inference Forest")
  )
  acc_long_mean<- acc_long%>%
    group_by(Run, NumFeatures)%>%
    summarise(Accuracy= mean(Accuracy))%>%
    ungroup()%>%
    mutate(algorithm="Mean")%>%
    rbind(acc_long)
  
  max_point <- acc_long_mean %>%
    filter(NumFeatures>=xmin)%>%
    filter(algorithm == "Mean") %>%
    slice_max(order_by = Accuracy, n = 1)
  
  ggplot(acc_long_mean, aes(x = NumFeatures, y = Accuracy, color = algorithm)) +
    geom_vline(xintercept = xmin,  color = "grey", size = 2) +
    geom_vline(xintercept = 20,  color = "grey", size = 2) +
    geom_line(size = 1) +
    geom_point(size = 3) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = c("#377EB8", "#4DAF4A","#E41A1C","#984EA3"))+
    geom_hline(yintercept = max_point$Accuracy, linetype = "dotted", color = "black", size = 1) +
    geom_vline(xintercept = xmax, linetype = "dotted", color = "black", size = 1) +
<<<<<<< HEAD
    scale_x_continuous(limits = c(1, 20),breaks = pretty(1:20, n = 5),expand = c(0.01, 0))+
=======
    geom_vline(xintercept = xmin,  color = "black", size = 1) +
    geom_vline(xintercept = 20,  color = "black", size = 1) +
    scale_x_continuous(limits = c(1, 20),breaks = pretty(1:20, n = 10),expand = c(0.01, 0))+
>>>>>>> 44048cadbdcd9d3bba90f08b5356b42a5751f9f2
    
    labs(
      title = method_name,
      x = "Number of selected factors",
      y = "Mean classification accuracy (%)",
      color = "Feature selection algorithm"
    ) +
    
    theme(plot.background = element_rect(fill = "White", color = "White"),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major  = element_line(color = "grey85",size = 0.6),
          axis.title = element_text(color="black",size=16, family = "sans", face = "bold",vjust = -1),
          axis.text =element_text(color="black",size=14, family = "sans"),
          legend.text = element_text(color="black",size=14, family = "sans"),
          legend.title = element_text(color="black",size=14, family = "sans", face = "bold"),
          legend.position = c(0.85, 0.9),
          plot.margin = unit(c(t=0.5,r=0.5,b=0.5,l=0.5), "cm"))
}

# Function to extract selected factors frequency
selected_factors_freq <- function(select_factors_cf,
  select_factors_ff, select_factors_rf) {
  process_df <- function(df, algo_name) {
    df %>%
      rename("Run"="X")%>%
      tidyr::pivot_longer(-Run, names_to = "NumFeatures", values_to = "selected_factors") %>%
      mutate(algorithm = algo_name)%>%
      separate_rows(selected_factors, sep = ",")
  }
  
  selected_factors <- bind_rows(
    process_df(select_factors_cf, "Conditional Inference Forest"),
    process_df(select_factors_ff, "Fuzzy Forest"),
    process_df(select_factors_rf, "Random Forest")
  )
  
  selected_factors_freq<- selected_factors%>%
    group_by(NumFeatures,selected_factors) %>%
    summarise(frequency = n(), .groups = 'drop')%>%
    ungroup()
  return(selected_factors_freq)
  
}

##=== Run for household_shock_recover_capacity ====
per_data_household_shock_recover_capacity_numeric <- prepare_numeric_matrix(per_household_shock_recover_capacity_redundantFiltered)
sft_data_household_shock_recover_capacity <- run_soft_threshold(per_data_household_shock_recover_capacity_numeric, dataset_name = "per_data_nzvFiltered")
per_data_household_shock_recover_capacity_picked_power <- 7  # Optionally automate this later

per_household_shock_recover_capacity <- per_household_shock_recover_capacity_redundantFiltered$household_shock_recover_capacity
per_household_shock_recover_capacity
per_household_shock_recover_capacity_factors <- per_household_shock_recover_capacity_redundantFiltered %>% select(-household_shock_recover_capacity)
per_household_shock_recover_capacity_factors

per_household_shock_recover_capacity_results <- feature_selection_continuous_algorithms(
  per_household_shock_recover_capacity_factors, per_household_shock_recover_capacity,
  per_data_household_shock_recover_capacity_picked_power, file_name = "indirect/per/per_household_shock_recover_capacity")

# Plot accuracy vs number of selected factors
per_household_shock_recover_capacity_acc_ff<- read.csv("results/indirect/per/per_household_shock_recover_capacity_accValAllFuzzyForest.csv",sep=",") 
per_household_shock_recover_capacity_acc_rf<- read.csv("results/indirect/per/per_household_shock_recover_capacity_accValAllRandomForest.csv",sep=",") 
per_household_shock_recover_capacity_acc_cf<- read.csv("results/indirect/per/per_household_shock_recover_capacity_accValAllCForest.csv",sep=",") 

plot_accuracy_vs_features(per_household_shock_recover_capacity_acc_ff,
                          per_household_shock_recover_capacity_acc_rf, 
                          per_household_shock_recover_capacity_acc_cf,
                          method_name = "A) Peru: Household recovery recovery capacity from shocks",13,12)
#1600*1000

per_household_shock_recover_capacity_selectFactors_cf<- read.csv("results/indirect/per/per_household_shock_recover_capacity_featureSelectedCForest.csv",sep=",") 
per_household_shock_recover_capacity_selectFactors_ff<- read.csv("results/indirect/per/per_household_shock_recover_capacity_featureSelectedFuzzyForest.csv",sep=",") 
per_household_shock_recover_capacity_selectFactors_rf<- read.csv("results/indirect/per/per_household_shock_recover_capacity_featureSelectedRandomForest.csv",sep=",") 

per_household_shock_recover_capacity_selectedFactors_freq<-selected_factors_freq(per_household_shock_recover_capacity_selectFactors_cf,
                                                                                 per_household_shock_recover_capacity_selectFactors_ff,
                                                                                 per_household_shock_recover_capacity_selectFactors_rf)
write.csv(per_household_shock_recover_capacity_selectedFactors_freq, "results/indirect/per/per_household_shock_recover_capacity_selectedFactors_freq.csv")

## Extract the best 5 factors
per_household_shock_recover_capacity_selectedFactors<-per_household_shock_recover_capacity_selectedFactors_freq%>%
  filter(NumFeatures=="featNum10")%>%
  slice_max(order_by = frequency, n = 10)%>%
  left_join(factors_list_analysis%>%select(category_1,factor,description,column_name_new),by=c("selected_factors"="column_name_new"))

write.csv(per_household_shock_recover_capacity_selectedFactors, "results/indirect/per/per_household_shock_recover_capacity_selectedFactors.csv")

# Select only the selected factors from database

per_data_household_shock_recover_capacity_selectedFactors<- per_data_analysis%>%
  select(dfs_adoption_binary,
         all_of(per_household_shock_recover_capacity_selectedFactors$selected_factors))

dim(per_data_household_shock_recover_capacity_selectedFactors)#[1] 200   24; 7 factors

write.csv(per_household_shock_recover_capacity_selectedFactors, "results/indirect/per/per_data_household_shock_recover_capacity_selectedFactors.csv")

create_cor_df <- function(data,selected_factors) {
  data_num<-data %>% 
    mutate(across(everything(), as.numeric))%>%
    select(all_of(per_household_shock_recover_capacity_selectedFactors$selected_factors))
  
  cor_matrix <- cor(data_num,
                    method = "spearman", use = "pairwise.complete.obs")
  
  cor_df <- as.data.frame(cor_matrix) %>%
    rownames_to_column("factor1") %>%
    tidyr::pivot_longer(-factor1, names_to = "factor2", values_to = "spearman_correlation")
  
  return(cor_df)
}
per_data_household_shock_recover_capacity_selected_factors_cor<-create_cor_df(per_household_shock_recover_capacity_redundantFiltered,per_household_shock_recover_capacity_selectedFactors)


##=== Run for governance_capacity ====
per_data_governance_capacity_numeric <- prepare_numeric_matrix(per_governance_capacity_redundantFiltered)
sft_data_governance_capacity <- run_soft_threshold(per_data_governance_capacity_numeric, dataset_name = "per_data_nzvFiltered")
per_data_governance_capacity_picked_power <- 7  # Optionally automate this later

per_governance_capacity <- per_governance_capacity_redundantFiltered$influence_nr_frequency

per_governance_capacity
per_governance_capacity_factors <- per_governance_capacity_redundantFiltered %>% select(-influence_nr_frequency)
per_governance_capacity_factors

per_governance_capacity_results <- feature_selection_continuous_algorithms(
  per_governance_capacity_factors, per_governance_capacity,
  per_data_governance_capacity_picked_power, file_name = "indirect/per/per_influence_nr_frequency")

# Plot accuracy vs number of selected factors
per_governance_capacity_acc_ff<- read.csv("results/indirect/per/per_influence_nr_frequency_accValAllFuzzyForest.csv",sep=",") 
per_governance_capacity_acc_rf<- read.csv("results/indirect/per/per_influence_nr_frequency_accValAllRandomForest.csv",sep=",") 
per_governance_capacity_acc_cf<- read.csv("results/indirect/per/per_influence_nr_frequency_accValAllCForest.csv",sep=",") 

plot_accuracy_vs_features(per_governance_capacity_acc_ff,per_governance_capacity_acc_rf, per_governance_capacity_acc_cf,
                          method_name = "A) Peru: Governance capacity",6)
#1600*1000

per_governance_capacity_selectFactors_cf<- read.csv("results/indirect/per/per_influence_nr_frequency_featureSelectedCForest.csv",sep=",") 
per_governance_capacity_selectFactors_ff<- read.csv("results/indirect/per/per_influence_nr_frequency_featureSelectedFuzzyForest.csv",sep=",") 
per_governance_capacity_selectFactors_rf<- read.csv("results/indirect/per/per_influence_nr_frequency_featureSelectedRandomForest.csv",sep=",") 

per_governance_capacity_selectedFactors_freq<-selected_factors_freq(per_governance_capacity_selectFactors_cf,
                                                               per_governance_capacity_selectFactors_ff,
                                                               per_governance_capacity_selectFactors_rf)
write.csv(per_governance_capacity_selectedFactors_freq, "results/indirect/per/per_influence_nr_selectedFactors_freq.csv")

## Extract the best 60 factors
per_governance_capacity_selectedFactors<-per_governance_capacity_selectedFactors_freq%>%
  filter(NumFeatures=="featNum10")%>%
  slice_max(order_by = frequency, n =10)%>%
  left_join(factors_list_analysis%>%select(category_1,factor,description,column_name_new),by=c("selected_factors"="column_name_new"))

write.csv(per_governance_capacity_selectedFactors, "results/indirect/per/per_influence_nr_selectedFactors.csv")

# Select only the selected factors from database

per_data_governance_capacity_selectedFactors<- per_data_analysis%>%
  select(dfs_adoption_binary,
         all_of(per_adoptionBinary_selectedFactors$selected_factors))

dim(per_data_governance_capacity_selectedFactors)#[1] 200   24; 23 factors

write.csv(per_data_governance_capacity_selectedFactors, "results/indirect/per_data_influence_nr_selectedFactors.csv")

create_cor_df <- function(data,selected_factors) {
  data_num<-data %>% 
    mutate(across(everything(), as.numeric))%>%
    select(all_of(per_adoptionBinary_selectedFactors$selected_factors))
  
  cor_matrix <- cor(data_num,
                    method = "spearman", use = "pairwise.complete.obs")
  
  cor_df <- as.data.frame(cor_matrix) %>%
    rownames_to_column("factor1") %>%
    tidyr::pivot_longer(-factor1, names_to = "factor2", values_to = "spearman_correlation")
  
  return(cor_df)
}
per_data_selected_factors_cor<-create_cor_df(per_data_redundantFiltered,per_adoptionBinary_selectedFactors)

##=== Run for training_participation ====
per_data_training_participation_numeric <- prepare_numeric_matrix(per_training_participation_redundantFiltered)
sft_data_training_participation <- run_soft_threshold(per_data_training_participation_numeric, dataset_name = "per_data_nzvFiltered")
per_data_training_participation_picked_power <- 7  # Optionally automate this later

per_training_participation <- per_training_participation_redundantFiltered$training_participation
str(per_training_participation)
table(per_training_participation)

per_training_participation_factors <- per_training_participation_redundantFiltered %>% select(-training_participation)
str(per_training_participation_factors)

per_training_participation_results <- feature_selection_binary_algorithms(
  per_training_participation_factors, per_training_participation,
  per_data_training_participation_picked_power, file_name = "indirect/per/per_training_participation")

# Plot accuracy vs number of selected factors
per_training_participation_acc_ff<- read.csv("results/indirect/per/per_training_participation_accValAllFuzzyForest.csv",sep=",") 
per_training_participation_acc_rf<- read.csv("results/indirect/per/per_training_participation_accValAllRandomForest.csv",sep=",") 
per_training_participation_acc_cf<- read.csv("results/indirect/per/per_training_participation_accValAllCForest.csv",sep=",") 

plot_accuracy_vs_features(per_training_participation_acc_ff,per_training_participation_acc_rf, per_training_participation_acc_cf,
                          method_name = "A) Peru: Governance capacity",12,13)
#1600*1000

per_training_participation_selectFactors_cf<- read.csv("results/indirect/per_training_participation_featureSelectedCForest.csv",sep=",") 
per_training_participation_selectFactors_ff<- read.csv("results/indirect/per_training_participation_featureSelectedFuzzyForest.csv",sep=",") 
per_training_participation_selectFactors_rf<- read.csv("results/indirect/per_training_participation_featureSelectedRandomForest.csv",sep=",") 

per_training_participation_selectedFactors_freq<-selected_factors_freq(per_training_participation_selectFactors_cf,
  per_training_participation_selectFactors_ff,
  per_training_participation_selectFactors_rf)
write.csv(per_training_participation_selectedFactors_freq, "per_training_participation_selectedFactors_freq.csv")

## Extract the best 60 factors
per_training_participation_selectedFactors<-per_training_participation_selectedFactors_freq%>%
  filter(NumFeatures=="featNum22")%>%
  slice_max(order_by = frequency, n = 22)%>%
  left_join(factors_list_analysis%>%select(category_1,factor,description,column_name_new),by=c("selected_factors"="column_name_new"))

write.csv(per_training_participation_selectedFactors, "results/per_training_participation_selectedFactors.csv")

# Select only the selected factors from database

per_data_adoptionBinary_selectedFactors<- per_data_analysis%>%
  select(dfs_adoption_binary,
         all_of(per_adoptionBinary_selectedFactors$selected_factors))

dim(per_data_adoptionBinary_selectedFactors)#[1] 200   24; 23 factors

write.csv(per_data_adoptionBinary_selectedFactors, "results/per_data_adoptionBinary_selectedFactors.csv")

create_cor_df <- function(data,selected_factors) {
  data_num<-data %>% 
    mutate(across(everything(), as.numeric))%>%
    select(all_of(per_adoptionBinary_selectedFactors$selected_factors))
  
  cor_matrix <- cor(data_num,
                    method = "spearman", use = "pairwise.complete.obs")
  
  cor_df <- as.data.frame(cor_matrix) %>%
    rownames_to_column("factor1") %>%
    tidyr::pivot_longer(-factor1, names_to = "factor2", values_to = "spearman_correlation")
  
  return(cor_df)
}
per_data_selected_factors_cor<-create_cor_df(per_data_redundantFiltered,per_adoptionBinary_selectedFactors)

fills <- c("#f0c602","#F09319", "#ea6044","#d896ff","#6a57b8",  "#87CEEB", "#496491", "#92c46d", "#92c46d","#92c46d","#297d7d") #
 "#602058"
