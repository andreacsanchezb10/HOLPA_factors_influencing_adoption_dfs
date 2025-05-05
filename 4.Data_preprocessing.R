library(dplyr)
library(readxl)
library(reshape2)
library(summarytools)
library(corrplot)

#############################################################    
########## UPLOAD DATA #####-----
#############################################################
factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")
sort(unique(factors_list$category_1))

per_data_clean<- read.csv("per_data_clean.csv",sep=",")
sort(unique(per_data_clean$soil_erosion_perception))
per_summary_categorical<-read.csv("per_summary_categorical.csv",sep=",") #541
per_summary_numerical<-read.csv("per_summary_numerical.csv",sep=",")  #80 factors

factors_category<-per_summary_numerical%>%dplyr::select(column_name_new, category_1,sub_category)%>%
  rbind(per_summary_categorical%>%dplyr::select(column_name_new,category_1,sub_category))%>%
  distinct()

sort(unique(per_data_clean$province))

#### Select the factors that were listed as important for adoption according to:
# - data availability
# - systematic evidence map
# - meta-analysis
# - Dessart et al 2018
# - Context document (Peru)

per_variables_list<-c(unique(per_summary_categorical$column_name_new2),unique(per_summary_numerical$column_name_new))
per_variables_list

per_data_analysis<- per_data_clean%>%
  dplyr::select(kobo_farmer_id,all_of(per_variables_list))

rownames(per_data_analysis) <- per_data_analysis$kobo_farmer_id

per_data_analysis<- per_data_analysis%>%
  dplyr::select(-kobo_farmer_id)

dim(per_data_analysis) #200 farmers; 274 variables evaluated

a<-as.data.frame(c(colnames(per_data_analysis)))%>%
  rename("column_name_new"="c(colnames(per_data_analysis))")%>%
  left_join(factors_list%>%select(category_1,factor,column_name_new,constructs,constructs_type), by="column_name_new")%>%
  group_by(category_1) %>%
  mutate(column_name_new_count = n()) %>%
  tally()

ggplot(data=a, aes(x=n, y=category_1, fill= category_1)) +
  geom_bar(stat="identity")+
  geom_text(aes(label = n), 
            hjust = -0.2, # Adjust position of the label to the right of the bar
            size = 4) +
  theme_minimal() +
  labs(x = "Number of factors", y = "Category") +
  theme(legend.position = "none")

dim(per_data_analysis) #200 farmers; 18 outcomes; 256 factors; 
#[1] 200 274


#############################################################    
########## DATA TYPE CONVERSION #####-----
#############################################################

###### --- CATEGORICAL AND BINARY VARIABLES -----
#### Convert categorical and binary to factor
columns_categorical <- intersect(per_summary_categorical$column_name_new2, colnames(per_data_analysis))
print(columns_categorical)  # Check if it holds expected values

per_data_analysis<- per_data_analysis%>%
  mutate(across(all_of(columns_categorical), as.factor))
sort(unique(per_data_analysis$main_crops_annual ))

###### --- NUMERICAL VARIABLES -----
#### Convert continuous variables to numeric
columns_continuous <- intersect(per_summary_numerical$column_name_new, colnames(per_data_analysis))
print(columns_continuous)  # Check if it holds expected values

per_data_analysis<- per_data_analysis%>%
  mutate(across(all_of(columns_continuous), as.numeric))

table(per_data_analysis$crop_type)

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
  as.data.frame()%>%
  mutate(across(everything(), as.factor))  # not across_all() which is deprecated
str(dummies)
per_data_Binary <- per_data_analysis%>%
  cbind(dummies)%>%
  dplyr::select(-all_of(columns_categorical_nominal))


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
  dplyr::select(-all_of(cols_to_remove))

b<-as.data.frame(c(colnames(per_data_Binary)))%>%
  rename("column_name_new"="c(colnames(per_data_Binary))")%>%
  left_join(factors_list%>%
              dplyr::select(category_1,column_name_new), by="column_name_new")%>%
  mutate(category_1= case_when(
    column_name_new== "year_assessment.2023"~"biophysical_context",
    TRUE~category_1))%>%
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

dim(per_data_Binary) #200 farmers; 18 outcomes; 270 factors
#[1] 200 288

write.csv(per_data_Binary,"per_data_Binary.csv",row.names=TRUE)



#####################OLD########################

#############################################################    
############# DATA DISTRIBUTION EXAMINATION -----
#############################################################
library(e1071)
columns_continuous <- names(per_data_Binary)[sapply(per_data_Binary, is.numeric)]
columns_continuous

# === Identify factors with skewed, kurtoise, normal distribution ===
library(moments)

# Function to compute skewness, kurtosis, and K-S test, including Z-values
normality_summary_ks_z <- function(x) {
  x <- na.omit(x)
  n <- length(x)
  if (n < 8) return(rep(NA, 7))  # Z for kurtosis requires n ≥ 8
  
  # Central moments
  skew <- skewness(x)
  kurt <- kurtosis(x)
  
  # Z-score for skewness: under normality, SE = sqrt(6/n)
  z_skew <- skew / sqrt(6 / n)
  
  # Z-score for kurtosis: under normality, SE = sqrt(24/n)
  z_kurt <- (kurt - 3) / sqrt(24 / n)  # Excess kurtosis (kurtosis - 3)
  
  # K-S test against standard normal
  x_scaled <- scale(x)
  ks <- ks.test(x_scaled, "pnorm")
  
  return(c(Skewness = skew,
           Z_Skewness = z_skew,
           Kurtosis = kurt,
           Z_Kurtosis = z_kurt,
           KS_Statistic = ks$statistic,
           KS_P_value = ks$p.value,
           N = n))
}

# Apply to all numeric variables
ks_z_results <- sapply(per_data_Binary[columns_continuous], normality_summary_ks_z)

# Format as a data frame
ks_z_df <- as.data.frame(t(ks_z_results))
ks_z_df$Variable <- rownames(ks_z_df)
rownames(ks_z_df) <- NULL

# Arrange by most extreme Z_Skewness or Z_Kurtosis
ks_z_df <- ks_z_df %>%
  arrange(desc(abs(Z_Skewness)))

highly_skewed <- names(ks_z_df[abs(ks_z_df$Skewness) > 1])




## Histogram plots for each numerical variable
plot_list <- lapply(highly_skewed, function(col) {
  ggplot(per_data_Filterednzv, aes(x = .data[[col]])) +
    geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
    labs(title = col) +
    theme_minimal()
})

histogram_plot <- wrap_plots(plot_list) + plot_annotation(title = "Histograms of skewed Variables")
print(histogram_plot)

# === Log-transform variables with skewed distribution >1 ===

#Log transform the skewed variables (use log1p to avoid issues with 0s)
per_data_logTransformed <- per_data_Binary
per_data_logTransformed[highly_skewed] <- 
  lapply(per_data_logTransformed[highly_skewed], function(x) log1p(x))
str(per_data_logTransformed)

# === Scale data ===
# Center (mean = 0) and scale (sd = 1) the continuous variables:
per_data_scaled<-per_data_logTransformed
per_data_scaled[,columns_continuous] = scale(per_data_scaled[,columns_continuous])
dim(per_data_scaled) #[1] 200 297 #200 farmers; 297 variables retained



#############################################################    
############# SPEARMAN'S  CORRELATION -----
#############################################################
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

# Function to plot correlated factors within categories
plot_correlation_by_category <- function(cor_df) {
  unique_categories <- unique(cor_df$category_1.factor1)
  plots <- list()
  
  for (cat in unique_categories) {
    cat_data <- cor_df %>%
      filter(category_1.factor1 == cat, category_1.factor2 == cat)
    
    if (nrow(cat_data) > 0) {
      plots[[cat]] <- ggplot(cat_data, aes(x = factor1, y = factor2, fill = spearman_correlation)) +
        geom_tile(color = "white") +
        geom_text(data = cat_data %>% filter(abs(spearman_correlation) >= 0.8),
                  aes(label = round(spearman_correlation, 2)), size = 5) +
        scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                             limits = c(-1, 1), name = "Spearman\nCorrelation") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.text.y = element_text(size = 8),
              plot.title = element_text(hjust = 0.5)) +
        labs(title = paste("Category:", cat), x = NULL, y = NULL)
    }
  }
  return(plots)
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

per_factors_list <- as.data.frame(colnames(per_data_Filterednzv))%>%
  rename("column_name_new"= "colnames(per_data_Filterednzv)")%>%
  left_join(factors_list%>%select(column_name_new, category_1),by="column_name_new")%>%
  filter(category_1!="outcome")

per_data_cor<-create_cor_df(per_data_Filterednzv,per_factors_list)
str(per_data_cor)

plot_correlation_by_category(per_data_cor)
plot_correlation_betw_category(per_data_cor)

#############################################################
############################################################    
############# STANDARDIZATION OF CONTINUES VARIABLES -----
#############################################################
library(e1071)
columns_continuous <- names(per_data_Binary)[sapply(per_data_Binary, is.numeric)]
columns_continuous

# === Log-transform variables with skewned distribution ===
#Calculate skewness for each numeric column
skewed_vars <- sapply(per_data_Binary[columns_continuous], skewness, na.rm = TRUE)
skewed_vars[is.na(skewed_vars)]

#Identify variables with high skewness (absolute skewness > 1)
highly_skewed <- names(skewed_vars[abs(skewed_vars) > 1])
highly_skewed
highly_skewed <- highly_skewed[!is.na(highly_skewed)]

#sfs_burning_residues_area  error arreglar
#NaN

#Log transform the skewed variables (use log1p to avoid issues with 0s)
per_data_logTransformed <- per_data_Binary
per_data_logTransformed[highly_skewed] <- 
  lapply(per_data_logTransformed[highly_skewed], function(x) log1p(x))
str(per_data_logTransformed)

# === Scale data ===
# Center (mean = 0) and scale (sd = 1) the continuous variables:
per_data_scaled<-per_data_logTransformed
per_data_scaled[,columns_continuous] = scale(per_data_scaled[,columns_continuous])
dim(per_data_scaled) #[1] 200 297 #200 farmers; 297 variables retained


####----
agroecologycal_adherance<- read.csv("C:/Users/andreasanchez/OneDrive - CGIAR/Bioversity/AI/HOLPA/HOLPA_data/Peru/peru_data_clean/per/per_agroecology_score.csv")%>%
  mutate(score_agroecology_module=as.numeric(score_agroecology_module))%>%
names(agroecologycal_adherance)
str(agroecologycal_adherance)
agroecologycal_adherance<-agroecologycal_adherance%>%
  group_by(kobo_farmer_id,theme)%>%
  mutate(median_score=median(score_agroecology_module))%>%
  ungroup()
library(tidyr)
agroecology_principles <- agroecologycal_adherance %>%
  group_by(kobo_farmer_id, theme) %>%
  summarise(median_score = median(score_agroecology_module, na.rm = TRUE), .groups = "drop") %>%
  mutate(theme = paste0("agroecology_principle_", theme)) %>%
  pivot_wider(names_from = theme, values_from = median_score)%>%
  ungroup()%>%
  dplyr::select(-kobo_farmer_id)
agroecology_principles[is.na(agroecology_principles)] <- 0


# Optional: view the result
head(agroecology_wide)
length(agroecology_wide$kobo_farmer_id)
str(agroecology_wide)
res <- sparsewkm(X = agroecology_principles, centers = 2)
res$W[,1:5]
plot(res, what="weights.features")

plot(res, what="expl.var")
plot(res, what="w.expl.var")

table(per_data_sewkm$dfs_adoption_binary, res$cluster[,1])
table(per_data_sewkm$dfs_adoption_binary, res$cluster[,7])

table(per_data_sewkm$dfs_adoption_binary, res$cluster[,12])
length(which(res$W[,7] > 0)) #4 principles
which(res$W[,7] > 0)

agroecology_question <- agroecologycal_adherance %>%
  group_by(kobo_farmer_id, name_question_recla) %>%
  summarise(median_score = median(score_agroecology_module, na.rm = TRUE), .groups = "drop") %>%
  mutate(name_question_recla = paste0("agroecology_question", name_question_recla)) %>%
  pivot_wider(id_cols = kobo_farmer_id,names_from = name_question_recla, values_from = median_score)%>%
  ungroup()%>%
  dplyr::select(-kobo_farmer_id)
agroecology_question[is.na(agroecology_question)] <- 0
str(agroecology_question)

nzv_list2 <- caret::nearZeroVar(agroecology_question, saveMetrics = TRUE)



nzv_list2 <- nearZeroVar(agroecology_question)  # This returns a numeric vector
nzv_factors <- agroecology_question[, nzv_list2]

nzv_factors<- agroecology_question[, nzv_list2]
print(nzv_factors)
view(dfSummary(nzv_factors))

## Remove nzv variables from data
agroecology_question<- agroecology_question[, -nzv_list2]

res2 <- sparsewkm(X = agroecology_question, centers = 2)
res$W[,1:5]
plot(res, what="weights.features")

plot(res, what="expl.var")
plot(res, what="w.expl.var")

table(per_data_sewkm$dfs_adoption_binary, res$cluster[,1])
table(per_data_sewkm$dfs_adoption_binary, res$cluster[,7])

table(per_data_sewkm$dfs_adoption_binary, res$cluster[,12])
length(which(res$W[,7] > 0)) #4 principles
which(res$W[,7] > 0)

