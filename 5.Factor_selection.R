library(summarytools)
library(readxl)
library(dplyr)
library(caret)


#############################################################    
########## UPLOAD DATA #####-----
#############################################################
factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")
sort(unique(factors_list$category_1))

per_data_clean<- read.csv("per_data_Binary.csv",sep=",")
dim(per_data_clean) #200 farmers; 303 factors


#############################################################    
########### FACTOR SELECTION ----
############################################################# 

##=== STEP 1: REMOVE ZERO AND NEAR ZERO VARIANCE FACTORS ======
#In some situations, the data generating mechanism can create predictors that only have a 
#single unique value (i.e. a “zero-variance predictor”). For many models (excluding tree-based models),
#this may cause the model to crash or the fit to be unstable.
#Similarly, predictors might have only a handful of unique values that occur with very low frequencies.
## frequency ratio: would be near one for well-behaved predictors and very large for highly-unbalanced data.
## percent of unique values: is the number of unique values divided by the total number of samples (times 100)
#that approaches zero as the granularity of the data increases
nzv <- caret::nearZeroVar(per_data_clean, saveMetrics= TRUE) 
nzv # the variables with nzv== TRUE should be remove

nzv_list <- nearZeroVar(per_data_clean)

nzv_factors<- per_data_clean[, nzv_list]
view(dfSummary(nzv_factors))
nzv_factors<-as.data.frame(c(colnames(nzv_factors)))%>%
  rename("column_name_new"="c(colnames(nzv_factors))")%>%
  left_join(factors_list%>%select(category_1,column_name_new,constructs,constructs_type), by="column_name_new")

## Remove nzv variables from data
per_data_nzvFiltered<- per_data_clean[, -nzv_list]

dim(per_data_nzvFiltered) #200 farmers; 172 factors retained

b<-as.data.frame(c(colnames(per_data_nzvFiltered)))%>%
  rename("column_name_new"="c(colnames(per_data_nzvFiltered))")%>%
  left_join(factors_list%>%select(category_1,column_name_new,constructs,constructs_type), by="column_name_new")%>%
  mutate(category_1= case_when(
    column_name_new== "year_assessment.2023"~"biophysical_context",
    TRUE~category_1))%>%
  group_by(category_1) %>%
  mutate(column_name_new_count = n()) %>%
  tally()%>%
  filter(category_1!="xxx")

ggplot(data=b, aes(x=n, y=category_1, fill= category_1)) +
  geom_bar(stat="identity")+
  geom_text(aes(label = n), 
            hjust = -0.2, # Adjust position of the label to the right of the bar
            size = 4) +
  theme_minimal() +
  labs(x = "Number of factors", y = "Category") +
  theme(legend.position = "none")

dim(per_data_nzvFiltered) #200 farmers; 206 factors retained

##=== STEP 2: CHECK FOR CORRELATION ACROSS FACTORS ======
library(tidyverse)
library(corrplot)
# Function to calculate Spearman's correlation
create_cor_df <- function(data, factors_list) {
  cor_matrix <- cor(data %>% mutate(across(everything(), as.numeric)),
                    method = "spearman", use = "pairwise.complete.obs")
  
  cor_df <- as.data.frame(cor_matrix) %>%
    rownames_to_column("factor1") %>%
    pivot_longer(-factor1, names_to = "factor2", values_to = "spearman_correlation") %>%
    left_join(factors_list %>% select(column_name_new, category_1), by = c("factor1" = "column_name_new")) %>%
    rename(category_1.factor1 = category_1) %>%
    left_join(factors_list %>% select(column_name_new, category_1), by = c("factor2" = "column_name_new")) %>%
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

per_factors_list <- as.data.frame(colnames(per_data_nzvFiltered))%>%
  rename("column_name_new"= "colnames(per_data_nzvFiltered)")%>%
  left_join(factors_list%>%select(column_name_new, category_1),by="column_name_new")%>%
  filter(category_1!="outcome")

per_data_nzvFiltered_cor<-create_cor_df(per_data_nzvFiltered,per_factors_list)
str(per_data_nzvFiltered_cor)

plot_correlation_betw_category(per_data_nzvFiltered_cor)

##=== STEP 3: REMOVE REDUNDANT AND IRRELEVANT FACTORS ======
per_remove_list<- intersect(factors_list$column_name_new[factors_list$peru_remove %in%c("irrelevant", "redundant",  "remove"  )],colnames(per_data_nzvFiltered))
per_remove_list

per_data_redundantFiltered<- per_data_nzvFiltered%>%
  dplyr::select(-all_of(per_remove_list))

dim(per_data_redundantFiltered) #200 farmers; 163 factors retained
names(per_data_redundantFiltered)

##=== STEP 4: CHECK FOR CORRELATION ACROSS FACTORS ======
per_factors_list2 <- as.data.frame(colnames(per_data_redundantFiltered))%>%
  rename("column_name_new"= "colnames(per_data_redundantFiltered)")%>%
  left_join(factors_list%>%select(column_name_new, category_1),by="column_name_new")%>%
  filter(category_1!="outcome")

per_data_redundantFiltered_cor<-create_cor_df(per_data_redundantFiltered,per_factors_list2)
str(per_data_redundantFiltered_cor)

plot_correlation_betw_category(per_data_redundantFiltered_cor)

##=== STEP 5: SEPARATE RELEVANT CORRELATED FACTORS INTO DIFFERENT DATASETS ======
per_data1<- per_data_redundantFiltered%>%
  select(-c(
    #== Human capital ==
    full_time_farmer, #num_occupation_secondary_list
    
    #== Farm management characteristics ==
    sfs_monoculture_annual_adoption, #keep	sfs_monoculture_annual_area
    soil_fertility_management_ecol_practices, #keep num_soil_fertility_ecol_practices
    
    #== Financial capital ==
    income_access_nonfarm, #keep income_amount_nonfarm
    
    #== P&I context_knowledge ==
    access_info_exchange_consumers, #keep num_info_exchange_consumers
    access_info_exchange_extension, #keep num_info_exchange_extension
    access_info_exchange_farmers, #keep num_info_exchange_farmers
    access_info_exchange_ngo, #keep num_info_exchange_ngo
    access_info_exchange_researchers, #keep num_info_exchange_researchers
    access_info_exchange_traders, #keep num_info_exchange_traders
    training_participation, #num_training_topics
    
    #== Social capital ==
    influence_nr_frequency, #keep participation_nr_frequency
    
    #== Between categories ==
    crop_type.cacao, #energy_tillage_haverst_type
    num_farm_products, #livestock_count_tlu
    district.dist_3, #months_count_water_accessibility_difficulty_flood_year
    cropland_area, #sfs_monoculture_perennial_area
  ))

per_factors_list3 <- as.data.frame(colnames(per_data1))%>%
  rename("column_name_new"= "colnames(per_data1)")%>%
  left_join(factors_list%>%select(column_name_new, category_1),by="column_name_new")%>%
  filter(category_1!="outcome")

per_data1_cor<-create_cor_df(per_data1,per_factors_list3)
plot_correlation_betw_category(per_data1_cor)

dim(per_data1) #200 farmers; 147 factors retained
any(is.na(per_data1)) #[1] FALSE
write.csv(per_data1,"per_data1.csv",row.names=FALSE)

per_data2<- per_data_redundantFiltered%>%
  select(-c(
    #== Human capital ==
    num_occupation_secondary_list, #full_time_farmer
    
    #== Farm management characteristics ==
    num_soil_fertility_ecol_practices, #keep soil_fertility_management_ecol_practices
    sfs_monoculture_annual_area, #keep	sfs_monoculture_annual_adoption
    
    #== Financial capital ==
    income_amount_nonfarm, #keep income_access_nonfarm
    
    #== P&I context_knowledge
    num_info_exchange_consumers, #keep access_info_exchange_consumers,
    num_info_exchange_extension,  #keep access_info_exchange_extension,
    num_info_exchange_farmers, #keep access_info_exchange_farmers
    num_info_exchange_ngo , #keep access_info_exchange_ngo
    num_info_exchange_researchers,  #keep access_info_exchange_researchers
    num_info_exchange_traders, #keep access_info_exchange_traders

    #== Social capital ==
    participation_nr_frequency, #keep influence_nr_frequency
    
    #== Between categories ==
    livestock_count_tlu, #num_farm_products
    months_count_water_accessibility_difficulty_flood_year, #district.dist_3
    farm_size, #labour_productivity #
    sfs_monoculture_perennial_area #cropland_area
  ))

per_factors_list4 <- as.data.frame(colnames(per_data2))%>%
  rename("column_name_new"= "colnames(per_data2)")%>%
  left_join(factors_list%>%select(column_name_new, category_1),by="column_name_new")%>%
  filter(category_1!="outcome")

per_data2_cor<-create_cor_df(per_data2,per_factors_list4)
plot_correlation_betw_category(per_data2_cor)

dim(per_data2) #200 farmers; 148 factors retained
any(is.na(per_data2)) #[1] FALSE
write.csv(per_data2,"per_data2.csv",row.names=FALSE)

##=== STEP 3: FACTORS PRIORITY BASED ON THEORY ======
per_data1_factor_highPriority<- intersect(unique(factors_list$column_name_new[str_starts(factors_list$priority, "high")]),colnames(per_data1))
per_data1_factor_highPriority

per_data1_highPriorityFiltered<-per_data1%>%
  select(dfs_total_area,dfs_adoption_binary,all_of(per_data1_factor_highPriority))
  
dim(per_data1_highPriorityFiltered) #200 farmers; 94 factors retained

per_data2_factor_highPriority<- intersect(unique(factors_list$column_name_new[str_starts(factors_list$priority, "high")]),colnames(per_data2))
per_data2_factor_highPriority

per_data2_highPriorityFiltered<-per_data2%>%
  select(dfs_total_area,dfs_adoption_binary,all_of(per_data2_factor_highPriority))

dim(per_data2_highPriorityFiltered) #200 farmers; 95 factors retained

##=== STEP 4: LASSO FACTOR SELECTION ADOPTION BINARY ======
#https://rpubs.com/Momo2019/1184100
library(glmnet)
library(pROC)

lasso_binary <- function(data, target_var, exclude_vars = NULL, n_iter = 100, lambda_seq = 10^seq(2, -2, by = -0.1)) {
  set.seed(123)
  # Prepare predictors
  exclude_vars <- c(target_var, exclude_vars)
  x_factors <- data[, !(names(data) %in% exclude_vars)]
  x_factors <- x_factors %>%
    mutate(across(where(is.factor), ~ as.numeric(as.character(.)))) %>%
    as.matrix()
  
  # Prepare binary outcome
  y_adoptionBinary <- as.numeric(as.character(data[[target_var]]))
  
  # Storage
  selected_vars_list <- list()
  auc_train_vec <- numeric(n_iter)
  auc_test_vec <- numeric(n_iter)
  
  for (i in 1:n_iter) {
  set.seed(100 + i)
  train_idx <- sample(1:nrow(x_factors), nrow(x_factors)/2)
  test_idx <- setdiff(1:nrow(x_factors), train_idx)
  
  # Cross-validation to select lambda
  cv_fit <- cv.glmnet(x_factors[train_idx,], y_adoptionBinary[train_idx],
                      alpha = 1,
                      lambda = lambda_seq,
                      nfolds = 10, #10-fold cross validation
                      family = "binomial",
                      standardize = TRUE) #scale and normalize the data when needed
  best_lambda <- cv_fit$lambda.min
  
  # Final model
  lasso_fit <- glmnet(x_factors[train_idx,], y_adoptionBinary[train_idx],
                      alpha = 1,
                      lambda = best_lambda,
                      family = "binomial",
                      standardize = TRUE) #scale and normalize the data when needed
  
  # Predict probabilities
  pred_train <- predict(lasso_fit, newx = x_factors[train_idx,], type = "response")
  pred_test <- predict(lasso_fit, newx = x_factors[test_idx,], type = "response")
  
  # Compute AUCs
  auc_train_vec[i] <- auc(roc(y_adoptionBinary[train_idx], pred_train))
  auc_test_vec[i] <- auc(roc(y_adoptionBinary[test_idx], pred_test))
  
  # Extract non-zero predictors
  coef_vec <- as.matrix(coef(lasso_fit))[-1, , drop = FALSE]  # exclude intercept
  selected_vars <- rownames(coef_vec)[coef_vec != 0]
  selected_vars_list[[i]] <- selected_vars
  }
  
  # Summarize
  all_selected <- unlist(selected_vars_list)
  selected_freq <- sort(table(all_selected), decreasing = TRUE)
  
  return(list(
    auc_train = auc_train_vec,
    auc_test = auc_test_vec,
    selected_freq = selected_freq
  ))
}

#per_data_1
lasso_results_per_data1 <- lasso_binary(per_data1, target_var = "dfs_adoption_binary", exclude_vars = "dfs_total_area")
per_data1_important_factors<-as.data.frame(head(lasso_results_per_data1$selected_freq, 40))
head(per_data1_important_factors)
cat("\nAverage Train AUC:", round(mean(lasso_results_per_data1$auc_train), 3), "\n")
cat("Average Test AUC:", round(mean(lasso_results_per_data1$auc_test), 3), "\n")

#per_data_2
lasso_results_per_data2 <- lasso_binary(per_data2, target_var = "dfs_adoption_binary", exclude_vars = "dfs_total_area")
per_data2_important_factors<-as.data.frame(head(lasso_results_per_data2$selected_freq, 40))
head(per_data2_important_factors)

cat("\nAverage Train AUC:", round(mean(lasso_results_per_data2$auc_train), 3), "\n")
cat("Average Test AUC:", round(mean(lasso_results_per_data2$auc_test), 3), "\n")


# high_importance per_data_1
lasso_results_per_highPriority_data1 <- lasso_binary(per_data1_highPriorityFiltered, target_var = "dfs_adoption_binary", exclude_vars = "dfs_total_area")
per_highPriority_data1_important_factors<-as.data.frame(head(lasso_results_per_highPriority_data1$selected_freq, 40))
head(per_highPriority_data1_important_factors)
cat("\nAverage Train AUC:", round(mean(lasso_results_per_highPriority_data1$auc_train), 3), "\n")
cat("Average Test AUC:", round(mean(lasso_results_per_highPriority_data1$auc_test), 3), "\n")

# Factors in per_highPriority_data1_important_factors but not in per_data1_important_factors
setdiff(per_highPriority_data1_important_factors$all_selected,per_data1_important_factors$all_selected)
setdiff(per_data1_important_factors$all_selected,per_highPriority_data1_important_factors$all_selected)

per_data1_selected_factors<-rbind(per_data1_important_factors,per_highPriority_data1_important_factors)%>%
  distinct(all_selected)
write.csv(per_data1_selected_factors,"per_data1_selected_factors.csv",row.names=FALSE)


# high_importance per_data_2
lasso_results_per_highPriority_data2 <- lasso_binary(per_data2_highPriorityFiltered, target_var = "dfs_adoption_binary", exclude_vars = "dfs_total_area")
per_highPriority_data2_important_factors<-as.data.frame(head(lasso_results_per_highPriority_data2$selected_freq, 40))
head(per_highPriority_data2_important_factors)
cat("\nAverage Train AUC:", round(mean(lasso_results_per_highPriority_data2$auc_train), 3), "\n")
cat("Average Test AUC:", round(mean(lasso_results_per_highPriority_data2$auc_test), 3), "\n")

# Factors in per_highPriority_data2_important_factors but not in per_data2_important_factors
setdiff(per_highPriority_data2_important_factors$all_selected,per_data2_important_factors$all_selected)
setdiff(per_data2_important_factors$all_selected,per_highPriority_data2_important_factors$all_selected)

per_data2_selected_factors<-rbind(per_data2_important_factors,per_highPriority_data2_important_factors)%>%
  distinct(all_selected)

write.csv(per_data2_selected_factors,"per_data2_selected_factors.csv",row.names=FALSE)


##=== STEP 4: LASSO FACTOR SELECTION ADOPTION INTENSITY ======
set.seed(123)

##--- Prepare the data 
x_vars <- per_data_nzvFiltered[, !(names(per_data_nzvFiltered) %in% c("dfs_adoption_binary"))]
x_vars <- x_vars %>%mutate(across(where(is.factor), ~ as.numeric(as.character(.))))%>%
  select(-dfs_total_area)
x_vars<-as.matrix(x_vars)
class(x_vars)
# Binary outcome variable
y_var <- as.numeric(as.character(per_data_nzvFiltered$dfs_adoption_binary))
y_var

# Parameters
n_iter <- 100
lambda_seq <- 10^seq(2, -2, by = -0.1)

# Store selected variables for each iteration
selected_vars_list <- list()
rsq_test_vec <- numeric(n_iter)
rsq_train_vec <- numeric(n_iter)



# R-squared function
rsq_fun <- function(actual, predicted) {
  rss <- sum((predicted - actual)^2)
  tss <- sum((actual - mean(actual))^2)
  1 - rss/tss
}

for (i in 1:n_iter) {
  set.seed(100 + i)
  train_idx <- sample(1:nrow(x_vars), nrow(x_vars)/2)
  test_idx <- setdiff(1:nrow(x_vars), train_idx)
  
  # Cross-validation to select lambda
  cv_fit <- cv.glmnet(x_vars[train_idx,], y_var[train_idx],
                      alpha = 1, lambda = lambda_seq,
                      family = "binomial",
                      standardize = TRUE)
  best_lambda <- cv_fit$lambda.min
  
  # Final model
  lasso_fit <- glmnet(x_vars[train_idx,], y_var[train_idx],
                      alpha = 1, lambda = best_lambda, family = "binomial")
  
  # Predict
  pred_train <- predict(lasso_fit, newx = x_vars[train_idx,], type = "response")
  pred_test <- predict(lasso_fit, newx = x_vars[test_idx,], type = "response")
  
  # Compute pseudo R² (for classification, use AUC or logloss instead if preferred)
  rsq_train_vec[i] <- rsq_fun(y_var[train_idx], pred_train)
  rsq_test_vec[i] <- rsq_fun(y_var[test_idx], pred_test)
  
 
  # Extract non-zero predictors
  coef_vec <- as.matrix(coef(lasso_fit))[-1, , drop = FALSE]  # exclude intercept
  selected_vars <- rownames(coef_vec)[coef_vec != 0]
  selected_vars_list[[i]] <- selected_vars
}

# Summary: variable selection frequency
all_selected <- unlist(selected_vars_list)
selected_freq <- sort(table(all_selected), decreasing = TRUE)

# Show top 20 frequently selected variables
print("Top selected variables across 10 iterations:")
print(head(selected_freq, 20))

# R-squared summary

cat("Average Train R-squared:", mean(rsq_train_vec), "\n")
cat("Average Test R-squared:", mean(rsq_test_vec), "\n")






