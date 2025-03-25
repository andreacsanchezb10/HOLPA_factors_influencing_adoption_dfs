library(dplyr)
library(readxl)
library(caret)
library(reshape2)
library(summarytools)
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

dim(per_data_analysis) #[1] 200 295 #200 farmers; 295 variables evaluated

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


dim(per_data_Binary) #[1] 200 309 #200 farmers; 305 variables evaluated

#############################################################    
#############################################################
a<-as.data.frame(c(colnames(per_data_Binary)))%>%
  rename("column_name_new"="c(colnames(per_data_Binary))")%>%
  left_join(factors_list%>%select(category_1,column_name_new), by="column_name_new")%>%
  mutate(category_1= case_when(
    column_name_new== "year_assessment.2023"~"Biophysical context",
    TRUE~category_1))%>%
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

dim(per_data_Binary) #[1] 200 309 #200 farmers; 309 variables retained


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

dim(per_data_analysis_Filterednzv) #[1] 200 225 #200 farmers; 225 variables retained

#############################################################    
############# IDENTIFYING CORRELATED FACTORS -----
#############################################################
#cor1 The correlation metric between two continuous features: Defaults to pearson
#cor2 The correlation metric between one categorical feature and one cont feature: Defaults to biserial
#cor3 The correlation metric between two categorical features: Defaults to Cramers-V
library("TangledFeatures")
library(tibble)
library(tidyr)

per_cor_matrix<- GeneralCor(per_data_analysis_Filterednzv, cor1 = "pearson", cor2 = "polychoric", cor3 = "spearman")
per_cor_df<-as.data.frame(per_cor_matrix)
per_highCor_pairs <- which(abs(per_cor_matrix) >= 0.8 & lower.tri(per_cor_matrix), arr.ind = TRUE)


# Create a readable data frame with factor names and correlation values
per_highCor_df <- data.frame(
  factor1 = rownames(per_cor_matrix)[per_highCor_pairs[, 1]],
  factor2 = colnames(per_cor_matrix)[per_highCor_pairs[, 2]],
  correlation = per_cor_matrix[per_highCor_pairs])%>%
  left_join(factors_list%>%select(metric_type,column_name_new), by=c("factor1"="column_name_new"))%>%
  left_join(factors_list%>%select(metric_type,column_name_new), by=c("factor2"="column_name_new"))%>%
  mutate(metric_type.x= ifelse(is.na(metric_type.x),"binary",metric_type.x),
         metric_type.y= ifelse(is.na(metric_type.y),"binary",metric_type.y))
         
# === Pearson Correlation ===
pearson_highCor_matrix <- function(highCor_df) {
  
pearson_highCor_matrix <- highCor_df %>%
  filter(metric_type.x == "continuous" & metric_type.y == "continuous") %>%
  select(factor1, factor2, correlation) %>%
  pivot_wider(names_from = factor2, values_from = correlation) %>%
  column_to_rownames(var = "factor1") %>%
  as.matrix()

# Get all row and column names to make the matrix square
pearson_all_names <- union(rownames(pearson_highCor_matrix), colnames(pearson_highCor_matrix))

# Create a full square matrix filled with NA
pearson_full_square_matrix <- matrix(NA, nrow = length(pearson_all_names), ncol = length(pearson_all_names),dimnames = list(pearson_all_names, pearson_all_names))
pearson_full_square_matrix[rownames(pearson_highCor_matrix), colnames(pearson_highCor_matrix)] <- pearson_highCor_matrix
pearson_full_square_matrix[lower.tri(pearson_full_square_matrix)] <- t(pearson_full_square_matrix)[lower.tri(pearson_full_square_matrix)]
diag(pearson_full_square_matrix) <- 1
return(pearson_full_square_matrix)
}

# === Polychoric Correlation ===
polychoric_highCor_matrix <- function(highCor_df) {
polychoric_highCor_matrix <- highCor_df %>%
  filter(metric_type.x == "categorical" & metric_type.y == "continuous"|
           metric_type.x == "continuous" & metric_type.y == "categorical"|
           metric_type.x == "binary" & metric_type.y == "continuous"|
           metric_type.x == "continuous" & metric_type.y == "binary") %>%
  select(factor1, factor2, correlation) %>%
  pivot_wider(names_from = factor2, values_from = correlation) %>%
  column_to_rownames(var = "factor1") %>%
  as.matrix()

# Get all row and column names to make the matrix square
polychoric_all_names <- union(rownames(polychoric_highCor_matrix), colnames(polychoric_highCor_matrix))
polychoric_all_names
# Create a full square matrix filled with NA
polychoric_full_square_matrix <- matrix(NA, nrow = length(polychoric_all_names), ncol = length(polychoric_all_names),dimnames = list(polychoric_all_names, polychoric_all_names))
polychoric_full_square_matrix[rownames(polychoric_highCor_matrix), colnames(polychoric_highCor_matrix)] <- polychoric_highCor_matrix

polychoric_full_square_matrix[lower.tri(polychoric_full_square_matrix)] <- t(polychoric_full_square_matrix)[lower.tri(polychoric_full_square_matrix)]
diag(polychoric_full_square_matrix) <- 1
return(polychoric_full_square_matrix)
}

# === Spearman Correlation ===
spearman_highCor_matrix <- function(highCor_df) {
  
spearman_highCor_matrix <- highCor_df %>%
  filter(metric_type.x == "categorical" & metric_type.y == "binary"|
           metric_type.x == "binary" & metric_type.y == "categorical"|
           metric_type.x == "categorical" & metric_type.y == "categorical"|
           metric_type.x == "binary" & metric_type.y == "binary") %>%
  select(factor1, factor2, correlation) %>%
  pivot_wider(names_from = factor2, values_from = correlation) %>%
  column_to_rownames(var = "factor1") %>%
  as.matrix()

# Get all row and column names to make the matrix square
spearman_all_names <- union(rownames(spearman_highCor_matrix), colnames(spearman_highCor_matrix))
spearman_all_names
# Create a full square matrix filled with NA
spearman_full_square_matrix <- matrix(NA, nrow = length(spearman_all_names), ncol = length(spearman_all_names),dimnames = list(spearman_all_names, spearman_all_names))
spearman_full_square_matrix[rownames(spearman_highCor_matrix), colnames(spearman_highCor_matrix)] <- spearman_highCor_matrix

spearman_full_square_matrix[lower.tri(spearman_full_square_matrix)] <- t(spearman_full_square_matrix)[lower.tri(spearman_full_square_matrix)]
diag(spearman_full_square_matrix) <- 1

return(spearman_full_square_matrix)
}

# Plot using corrplot
per_pearson_full_square_matrix<-pearson_highCor_matrix(per_highCor_df)
jpeg("plots/per_pearson_highCor.jpg", width = 1000, height = 900, units = "px", res = 150)
corrplot(per_pearson_full_square_matrix, addCoef.col = 'black',method = "circle",type = "upper", 
         tl.cex = 0.7, tl.col = "black",na.label = "NA",order = 'alphabet')
dev.off()

per_polychoric_full_square_matrix<-polychoric_highCor_matrix(per_highCor_df)
jpeg("plots/per_polychoric_highCor.jpg", width = 2500, height = 2500, units = "px", res = 150)
corrplot(per_polychoric_full_square_matrix,method = "circle",type = "upper", 
         tl.cex = 1.1, tl.col = "black",na.label = " ",order = 'alphabet')
dev.off()

per_spearman_full_square_matrix<-spearman_highCor_matrix(per_highCor_df)
jpeg("plots/per_spearman_highCor.jpg", width = 2500, height = 2500, units = "px", res = 150)
corrplot(per_spearman_full_square_matrix,method = "circle",type = "upper", 
         tl.cex = 1.1, tl.col = "black",na.label = " ",order = 'alphabet')
dev.off()

#REMOVE REDUNDANT/HIGHLY CORRELATED FACTORS:
per_data_analysis_FilteredCor<-per_data_analysis_Filterednzv%>%
  select(-c(
    "district.dist_1" ,#keep crop_type.cacao
    "district.dist_2" ,#keep crop_type.frutales
    "district.dist_3", #keep accessibility_waste_collection and months_count_water_accessibility_difficulty_flood_year
    "sfp_total_area", ##TO CHECK IF I INCLUDE OR NOT THIS FACTOR keep dfs_adoption_binary
    "dfs_total_area",
    "distance_public_transport",#keep distance_main_road
    "income_amount_total", # keep income_amount_onfarm COR=0.9924182
    "land_tenure_hold_status",#keep male_land_tenure_hold_proportion
    "cropland_area" #keep sfs_monoculture_perennial_area, 
  ))

per_adoptionBinary1<- per_data_analysis_FilteredCor%>%
  select(-c(access_info_exchange_consumers, #keep num_info_exchange_consumers
            access_info_exchange_extension, #keep num_info_exchange_extension
            access_info_exchange_farmers, #keep num_info_exchange_farmers
            access_info_exchange_ngo, #keep num_info_exchange_ngo
            access_info_exchange_researchers, #keep num_info_exchange_researchers
            access_info_exchange_traders, #keep num_info_exchange_traders
            household_shock_strategy, #keep household_shock_strategy_count
            income_access_nonfarm, #keep income_amount_nonfarm
            vegetation_cover_bushland, #keep vegetation_diversity_bushland
            vegetation_cover_forest, #keep vegetation_diversity_forest
            vegetation_cover_wetland, #keep vegetation_diversity_wetland
            vegetation_cover_woodlots, #keep vegetation_diversity_woodlots
            soil_fertility_management_ecol_practices, #keep num_soil_fertility_ecol_practices
            organic_fertilizer_amount_ha, #keep soil_fertility_management_organic
            sfs_monoculture_annual_adoption, #keep	sfs_monoculture_annual_area
            influence_nr_frequency #keep participation_nr_frequency
            
  ))

dim(per_adoptionBinary1) #[1] 200 200 #200 farmers; 200 variables retained
any(is.na(per_adoptionBinary1))

per_adoptionBinary2<- per_data_analysis_FilteredCor%>%
  select(-c(num_info_exchange_consumers, #keep access_info_exchange_consumers,  
            num_info_exchange_extension,  #keep access_info_exchange_extension,
            num_info_exchange_farmers, #keep access_info_exchange_farmers
            num_info_exchange_ngo , #keep access_info_exchange_ngo
            num_info_exchange_researchers,  #keep access_info_exchange_researchers
            num_info_exchange_traders, #keep access_info_exchange_traders
            household_shock_strategy_count, #keep household_shock_strategy
            income_amount_nonfarm, #keep income_access_nonfarm
            vegetation_diversity_bushland, #keep vegetation_cover_bushland
            vegetation_diversity_forest, #keep vegetation_cover_forest
            vegetation_diversity_wetland, #keep vegetation_cover_wetland
            vegetation_diversity_woodlots, #keep vegetation_cover_woodlots
            num_soil_fertility_ecol_practices, #keep soil_fertility_management_ecol_practices
            soil_fertility_management_organic, #keep organic_fertilizer_amount_ha
            sfs_monoculture_annual_area, #keep	sfs_monoculture_annual_adoption
            participation_nr_frequency #keep influence_nr_frequency
            
  ))

dim(per_adoptionBinary2) #[1] 200 200 #200 farmers; 200 variables retained
any(is.na(per_adoptionBinary2))

##CHECK CORRELATION AGAIN
per_cor_matrix<- GeneralCor(per_adoptionBinary1, cor1 = "pearson", cor2 = "polychoric", cor3 = "spearman")
per_cor_df<-as.data.frame(per_cor_matrix)
per_highCor_pairs <- which(abs(per_cor_matrix) >= 0.8 & lower.tri(per_cor_matrix), arr.ind = TRUE)


# Create a readable data frame with factor names and correlation values
per_highCor_df <- data.frame(
  factor1 = rownames(per_cor_matrix)[per_highCor_pairs[, 1]],
  factor2 = colnames(per_cor_matrix)[per_highCor_pairs[, 2]],
  correlation = per_cor_matrix[per_highCor_pairs])%>%
  left_join(factors_list%>%select(metric_type,column_name_new), by=c("factor1"="column_name_new"))%>%
  left_join(factors_list%>%select(metric_type,column_name_new), by=c("factor2"="column_name_new"))%>%
  mutate(metric_type.x= ifelse(is.na(metric_type.x),"binary",metric_type.x),
         metric_type.y= ifelse(is.na(metric_type.y),"binary",metric_type.y))

# Plot using corrplot
per_pearson_full_square_matrix<-pearson_highCor_matrix(per_highCor_df)
jpeg("plots/per_pearson_highCor_adoptionBinary1.jpg", width = 1000, height = 900, units = "px", res = 150)
corrplot(per_pearson_full_square_matrix, addCoef.col = 'black',method = "circle",type = "upper", 
         tl.cex = 0.7, tl.col = "black",na.label = "NA",order = 'alphabet')
dev.off()

per_polychoric_full_square_matrix<-polychoric_highCor_matrix(per_highCor_df)
jpeg("plots/per_polychoric_highCor_adoptionBinary1.jpg", width = 2500, height = 2500, units = "px", res = 150)
corrplot(per_polychoric_full_square_matrix,method = "circle",type = "upper", 
         tl.cex = 1.1, tl.col = "black",na.label = " ",order = 'alphabet')
dev.off()

per_spearman_full_square_matrix<-spearman_highCor_matrix(per_highCor_df)
jpeg("plots/per_spearman_highCor_adoptionBinary1.jpg", width = 2500, height = 2500, units = "px", res = 150)
corrplot(per_spearman_full_square_matrix,method = "circle",type = "upper", 
         tl.cex = 1.1, tl.col = "black",na.label = " ",order = 'alphabet')
dev.off()

#############################################################    
############# CENTERING AND SCALING -----
#############################################################
#Only for continuous variables
#per_adoptionBinary1
columns_continuous <- intersect(per_summary_numerical$column_name_new, colnames(per_adoptionBinary1))
# Center (mean = 0) and scale (sd = 0.5) the continuous variables:
per_adoptionBinary1[,columns_continuous] = 0.5*scale(per_adoptionBinary1[,columns_continuous])
dim(per_adoptionBinary1) #[1] 200 200 #200 farmers; 200 variables retained
write.csv(per_adoptionBinary1,"per_adoptionBinary1.csv",row.names=FALSE)

#per_adoptionBinary2
columns_continuous <- intersect(per_summary_numerical$column_name_new, colnames(per_adoptionBinary2))
# Center (mean = 0) and scale (sd = 0.5) the continuous variables:
per_adoptionBinary2[,columns_continuous] = 0.5*scale(per_adoptionBinary2[,columns_continuous])
dim(per_adoptionBinary2) #[1] 200 200 #200 farmers; 200 variables retained
write.csv(per_adoptionBinary2,"per_adoptionBinary2.csv",row.names=FALSE)
