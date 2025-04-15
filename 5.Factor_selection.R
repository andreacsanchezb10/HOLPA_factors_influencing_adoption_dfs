library(summarytools)


#############################################################    
########## UPLOAD DATA #####-----
#############################################################
factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")
sort(unique(factors_list$category_1))

per_data_clean<- read.csv("per_data_Binary.csv",sep=",")
dim(per_data_clean) #200 farmers; 306 factors


#############################################################    
########### FACTOR SELECTION ----
############################################################# 
##=== STEP 1: REMOVE REDUNDANT AND IRRELEVANT FACTORS ======
per_remove_list<- intersect(factors_list$column_name_new[factors_list$peru_remove %in%c("irrelevant", "redundant",  "remove"  )],colnames(per_data_clean))
per_remove_list

per_data_redundantFiltered<- per_data_clean%>%
  dplyr::select(-all_of(per_remove_list))

dim(per_data_redundantFiltered) #200 farmers; 231 factors retained

##=== STEP 2: REMOVE ZERO AND NEAR ZERO VARIANCE FACTORS ======
#In some situations, the data generating mechanism can create predictors that only have a 
#single unique value (i.e. a “zero-variance predictor”). For many models (excluding tree-based models),
#this may cause the model to crash or the fit to be unstable.
#Similarly, predictors might have only a handful of unique values that occur with very low frequencies.
## frequency ratio: would be near one for well-behaved predictors and very large for highly-unbalanced data.
## percent of unique values: is the number of unique values divided by the total number of samples (times 100)
#that approaches zero as the granularity of the data increases
nzv <- caret::nearZeroVar(per_data_redundantFiltered, saveMetrics= TRUE) 
nzv # the variables with nzv== TRUE should be remove

nzv_list <- nearZeroVar(per_data_redundantFiltered)

nzv_factors<- per_data_redundantFiltered[, nzv_list]
view(dfSummary(nzv_factors))
nzv_factors<-as.data.frame(c(colnames(nzv_factors)))%>%
  rename("column_name_new"="c(colnames(nzv_factors))")%>%
  left_join(factors_list%>%select(category_1,column_name_new,constructs,constructs_type), by="column_name_new")

## Remove nzv variables from data
per_data_nzvFiltered<- per_data_redundantFiltered[, -nzv_list]

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

dim(per_data_nzvFiltered) #200 farmers; 172 factors retained


##=== STEP 3: FACTORS PRIORITY BASED ON THEORY ======
names(factors_list)
sort(unique(factors_list$priority))
per_factor_highPriority<- intersect(unique(factors_list$column_name_new[str_starts(factors_list$priority, "high")]),colnames(per_data_nzvFiltered))
per_factor_highPriority

per_data_highPriorityFiltered<-per_data_nzvFiltered%>%
  select(dfs_total_area,dfs_adoption_binary,all_of(per_factor_highPriority))
  
dim(per_data_highPriorityFiltered) #200 farmers; 94 factors retained

##=== STEP 4: CHECK FOR CORRELATION ACROSS FACTORS ======
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

per_factors_list <- as.data.frame(colnames(per_data_highPriorityFiltered))%>%
  rename("column_name_new"= "colnames(per_data_highPriorityFiltered)")%>%
  left_join(factors_list%>%select(column_name_new, category_1),by="column_name_new")%>%
  filter(category_1!="outcome")

per_data_cor<-create_cor_df(per_data_highPriorityFiltered,per_factors_list)
str(per_data_cor)

plot_correlation_betw_category(per_data_cor)


##=== STEP 4: LASSO FACTOR SELECTION ADOPTION BINARY ======
#https://rpubs.com/Momo2019/1184100
library(glmnet)
set.seed(123)
##--- Prepare the data 
x_factors <- per_data_highPriorityFiltered[, !(names(per_data_highPriorityFiltered) %in% c("dfs_adoption_binary"))]
x_factors <- x_factors %>%mutate(across(where(is.factor), ~ as.numeric(as.character(.))))%>%
  select(-dfs_total_area)
x_factors<-as.matrix(x_factors)
class(x_factors)
# Binary outcome variable
y_adoptionBinary <- as.numeric(as.character(per_data_highPriorityFiltered$dfs_adoption_binary))
y_adoptionBinary

##--- Set Parameters
n_iter <- 100 #100 iterations
lambda_seq <- 10^seq(2, -2, by = -0.1)

# Store selected variables and AUCs
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

# Summary: variable selection frequency
all_selected <- unlist(selected_vars_list)
selected_freq <- sort(table(all_selected), decreasing = TRUE)

# Output: top 20 selected predictors
cat("\nTop selected variables across", n_iter, "iterations:\n")
print(head(selected_freq, 30))

# AUC summary
cat("\nAverage Train AUC:", round(mean(auc_train_vec), 3), "\n")
cat("Average Test AUC:", round(mean(auc_test_vec), 3), "\n")


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






