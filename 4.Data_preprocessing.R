library(dplyr)
library(readxl)
library(caret)
library(reshape2)
library(summarytools)
library(corrplot)

#############################################################    
########## UPLOAD DATA #####-----
#############################################################
factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")%>%
  filter(category_1!="xxx")
 
factors_list$category_1[grepl("^Farm management characteristics", factors_list$category_1)] <- "Farm management characteristics"
factors_list$category_1[grepl("^Financial capital", factors_list$category_1)] <- "Financial capital"
factors_list$category_1[grepl("^P&I context", factors_list$category_1)] <- "P&I context"
factors_list$category_1[grepl("^P&I context", factors_list$category_1)] <- paste0(
  factors_list$category_1[grepl("^P&I context", factors_list$category_1)],"_",  factors_list$category_2[grepl("^P&I context", factors_list$category_1)])
factors_list$category_1[grepl("^P&I context_knowledge", factors_list$category_1)] <- "P&I context_knowledge"

per_data_clean<- read.csv("per_data_clean.csv",sep=",")
per_summary_categorical<-read.csv("per_summary_categorical.csv",sep=",") #598
per_summary_numerical<-read.csv("per_summary_numerical.csv",sep=",")  #75 factors

factors_category<-per_summary_numerical%>%dplyr::select(column_name_new, category_1,category_2)%>%
  rbind(per_summary_categorical%>%dplyr::select(column_name_new,category_1,category_2))%>%
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

dim(per_data_analysis) #[1] 200 322 #200 farmers; 322 variables evaluated

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

a<-as.data.frame(c(colnames(per_data_Binary)))%>%
  rename("column_name_new"="c(colnames(per_data_Binary))")%>%
  left_join(factors_list%>%
              dplyr::select(category_1,column_name_new), by="column_name_new")%>%
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

dim(per_data_Binary) #[1] 200 336 #200 farmers; 336 variables retained

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
nzv <- caret::nearZeroVar(per_data_Binary, saveMetrics= TRUE) 
nzv # the variables with nzv== TRUE should be remove

nzv_list <- nearZeroVar(per_data_Binary)

nzv_factors<- per_data_Binary[, nzv_list]
print(nzv_factors)
view(dfSummary(nzv_factors))

## Remove nzv variables from data
per_data_Filterednzv<- per_data_Binary[, -nzv_list]

dim(per_data_Filterednzv) #[1] 200 251 #200 farmers; 251 variables retained

b<-as.data.frame(c(colnames(per_data_Filterednzv)))%>%
  rename("column_name_new"="c(colnames(per_data_Filterednzv))")%>%
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

dim(per_data_Filterednzv) #[1] 200 239 #200 farmers; 239 variables retained

#write.csv(per_data_Filterednzv,"per_data_Filterednzv.csv",row.names=FALSE)

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
  left_join(factors_category)

per_factors_list$category_1[grepl("^year_assessment.", per_factors_list$column_name_new)] <- "Biophysical context"
per_factors_list$category_1[grepl("^crop_type", per_factors_list$column_name_new)] <- "Farm management characteristics"
per_factors_list$category_1[grepl("^marital_status.", per_factors_list$column_name_new)] <- "Human capital"
per_factors_list$category_1[grepl("^read_write.", per_factors_list$column_name_new)] <- "Human capital"
per_factors_list$category_1[grepl("^gender_land_tenure.", per_factors_list$column_name_new)] <- "P&I context_land tenure"
per_factors_list$category_1[grepl("^district.dist", per_factors_list$column_name_new)] <- "P&I context_general"
per_factors_list$category_1[grepl("^province.prov", per_factors_list$column_name_new)] <- "P&I context_general"
per_factors_list <-per_factors_list%>%
  filter(category_1!="outcome")

per_data_cor<-create_cor_df(per_data_Filterednzv,per_factors_list)
str(per_data_cor)

plot_correlation_by_category(per_data_cor)
plot_correlation_betw_category(per_data_cor)

#############################################################
############# REMOVE REDUNDANT FACTORS -----
#############################################################
per_redundant_factors<- read_excel("factors_list.xlsx",sheet = "per_redundant_factors")%>%
  select(removed)
per_redundant_factors<-per_redundant_factors$removed
per_redundant_factors

per_data_FilteredRedundant<-per_data_Filterednzv%>%
  dplyr::select(-dplyr::any_of(per_redundant_factors))

per_data_FilteredRedundant_cor<-create_cor_df(per_data_FilteredRedundant,per_factors_list)
str(per_data_FilteredRedundant_cor)
sort(unique(per_data_FilteredRedundant_cor$factor1))

plot_correlation_by_category(per_data_FilteredRedundant_cor)

write.csv(per_data_FilteredRedundant,"per_data0.csv",row.names=FALSE)

#############################################################  
############# SEPARATE CORRELATED FACTORS INTO DIFFERENT DATASETS -----
#############################################################
per_data1<- per_data_FilteredRedundant%>%
  select(-c(
    #== Human capital ==
    full_time_farmer, #num_occupation_secondary_list
    
    #== Farm management characteristics ==
    livestock_sale, #use_percentage_crops_sales
    main_crops_shrub,#num_main_crops_shrub
    main_crops_herb, #num_main_crops_herb
    main_crops_annual,#num_main_crops_annual
    livestock_exotic_local, #num_farm_products
    soil_fertility_management_organic,#organic_fertilizer_amount_ha
    sfs_monoculture_annual_adoption, #keep	sfs_monoculture_annual_area
    soil_fertility_management_ecol_practices, #keep num_soil_fertility_ecol_practices
    
    #== Financial capital ==
    income_access_nonfarm, #keep income_amount_nonfarm
    farm_products.Livestock, #livestock_count_tlu
    
    #== Natural capital ==
    vegetation_cover_bushland, #keep vegetation_diversity_bushland
    vegetation_cover_forest, #keep vegetation_diversity_forest
    vegetation_cover_wetland, #keep vegetation_diversity_wetland
    vegetation_cover_woodlots, #keep vegetation_diversity_woodlots
    
    #== P&I context_knowledge ==
    access_info_exchange_consumers, #keep num_info_exchange_consumers
    access_info_exchange_extension, #keep num_info_exchange_extension
    access_info_exchange_farmers, #keep num_info_exchange_farmers
    access_info_exchange_ngo, #keep num_info_exchange_ngo
    access_info_exchange_researchers, #keep num_info_exchange_researchers
    access_info_exchange_traders, #keep num_info_exchange_traders
    access_info_exchange, #keep  num_info_exchange_sources
    training_participation, #num_training_topics
    
    #== P&I context_value chain ==
    fair_price_livestock, # num_sales_channel_livestock
    
    #== Social capital ==
    influence_nr_frequency, #keep participation_nr_frequency
    
    #== Vulnerability context ==
    household_shock_strategy, #keep household_shock_strategy_count
    
    #== Between categories ==
    crop_type.cacao, #energy_tillage_haverst_type
    credit_access, #credit_payment_hability
    num_farm_products, #livestock_count_tlu
    livestockland_area, #livestock_count_tlu
    use_percentage_livestock_sales, #num_sales_channel_livestock
    income_sources.livestock, #num_sales_channel_livestock
    district.dist_3, #months_count_water_accessibility_difficulty_flood_year
    labour_productivity, #farm_size
    cropland_area, #sfs_monoculture_perennial_area
    ))

per_data1_cor<-create_cor_df(per_data1,per_factors_list)
#plot_correlation_by_category(per_data1_cor)
plot_correlation_betw_category(per_data1_cor)

dim(per_data1) #[1] 200 196 #200 farmers; 196 variables retained
any(is.na(per_data1)) #[1] FALSE
write.csv(per_data1,"per_data1.csv",row.names=FALSE)


per_data2<- per_data_FilteredRedundant%>%
  select(-c(
    #== Human capital ==
    num_occupation_secondary_list, #full_time_farmer
    
    #== Farm management characteristics ==
    use_percentage_livestock_sales, #livestock_sale
    num_main_crops_shrub, #main_crops_shrub
    num_main_crops_herb, #main_crops_herb, #
    num_main_crops_annual, #main_crops_annual
    num_farm_products, #livestock_exotic_local
    organic_fertilizer_amount_ha, #soil_fertility_management_organic,#
    num_soil_fertility_ecol_practices, #keep soil_fertility_management_ecol_practices
    sfs_monoculture_annual_area, #keep	sfs_monoculture_annual_adoption
    
    #== Financial capital ==
    income_amount_nonfarm, #keep income_access_nonfarm
    farm_products.Livestock, #livestock_count_tlu
    
    #== Natural capital ==
    vegetation_diversity_bushland, #keep vegetation_cover_bushland
    vegetation_diversity_forest, #keep vegetation_cover_forest
    vegetation_diversity_wetland, #keep vegetation_cover_wetland
    vegetation_diversity_woodlots, #keep vegetation_cover_woodlots
    
    #== P&I context_knowledge
    num_info_exchange_consumers, #keep access_info_exchange_consumers,
    num_info_exchange_extension,  #keep access_info_exchange_extension,
    num_info_exchange_farmers, #keep access_info_exchange_farmers
    num_info_exchange_ngo , #keep access_info_exchange_ngo
    num_info_exchange_researchers,  #keep access_info_exchange_researchers
    num_info_exchange_traders, #keep access_info_exchange_traders
    num_info_exchange_sources, #keep access_info_exchange
    num_training_topics, #training_participation
    
    #== P&I context_value chain ==
    num_sales_channel_livestock, #fair_price_livestock
    
    #== Social capital ==
    participation_nr_frequency, #keep influence_nr_frequency
    
    #== Vulnerability context ==
    household_shock_strategy_count, #keep household_shock_strategy
    
    #== Between categories ==
    energy_tillage_haverst_type, #crop_type.cacao 
    credit_payment_hability, #credit_access
    livestock_count_tlu, #num_farm_products
    #livestock_count_tlu, #livestockland_area
    #num_sales_channel_livestock, #use_percentage_livestock_sales
    #num_sales_channel_livestock, #use_percentage_livestock_sales, 
    #num_sales_channel_livestock, #income_sources.livestock 
    months_count_water_accessibility_difficulty_flood_year, #district.dist_3
    accessibility_waste_collection, #district.dist_3
    farm_size, #labour_productivity #
    sfs_monoculture_perennial_area #cropland_area
    ))

per_data2_cor<-create_cor_df(per_data2,per_factors_list)
#plot_correlation_by_category(per_data2_cor)
plot_correlation_betw_category(per_data2_cor)

dim(per_data2) #[1] 200 198 #200 farmers; 198 variables retained
any(is.na(per_data2)) #[1] FALSE
write.csv(per_data2,"per_data2.csv",row.names=FALSE)



############################# OLD CODE ####################
############################################################
############# GET DIFFERENT DATABASES WITHOUT CORRELATED VARIABLES -----
#############################################################
create_uncorrelated_datasets <- function(data, cor_df, correlation_col = "spearman_correlation", threshold = 0.8) {
  # Filter for correlated pairs above threshold and exclude self-pairs
  cor_pairs <- cor_df %>%
    filter(factor1 != factor2) %>%
    filter(abs(!!sym(correlation_col)) >= threshold)%>%
    filter(category_1.factor1 == category_1.factor2)
  
  
  # Initialize a list of variable sets — starting with all variables
  base_vars <- colnames(data)
  datasets <- list()
  
  # Remove duplicated pairs (i.e., treat (A,B) same as (B,A))
  cor_pairs_unique <- cor_pairs %>%
    rowwise() %>%
    mutate(pair_id = paste(sort(c(factor1, factor2)), collapse = "_")) %>%
    distinct(pair_id, .keep_all = TRUE)
  
  # Generate all possible combinations by choosing one variable per pair to keep
  pair_options <- cor_pairs_unique %>%
    mutate(option1 = factor1,
           option2 = factor2)
  
  # Recursive function to generate combinations
  generate_sets <- function(pairs, current_vars = base_vars) {
    if (nrow(pairs) == 0) return(list(current_vars))
    
    this_pair <- pairs[1, ]
    rest_pairs <- pairs[-1, ]
    
    # Option 1: keep factor1, remove factor2
    opt1 <- setdiff(current_vars, this_pair$factor2)
    opt1_sets <- generate_sets(rest_pairs, opt1)
    
    # Option 2: keep factor2, remove factor1
    opt2 <- setdiff(current_vars, this_pair$factor1)
    opt2_sets <- generate_sets(rest_pairs, opt2)
    
    return(c(opt1_sets, opt2_sets))
  }
  
  # Generate all combinations of variables
  variable_sets <- generate_sets(pair_options)
  
  # Convert to unique list (some combinations may be duplicated)
  unique_variable_sets <- unique(variable_sets)
  
  # Build the datasets
  dataset_list <- map(unique_variable_sets, ~ data[, .x, drop = FALSE])
  names(dataset_list) <- paste0("uncorrelated_set_", seq_along(dataset_list))
  
  return(dataset_list)
}


uncorrelated_datasets <- create_uncorrelated_datasets(per_data_FilteredRedundant, per_data_FilteredRedundant_cor)
head(uncorrelated_datasets[[1]])



#############################################################    
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



#############################################################    
#############FEATURE SELECTION MIXOMICS -----
#############################################################
## install BiocManager if not installed 
library(mixOmics) # import the mixOmics library
X <- per_data_Filterednzv%>%
  dplyr::select(-dfs_adoption_binary,-kobo_farmer_id)%>%
mutate(across(everything(), as.numeric))
X<-as.matrix(X)
class(X)
Y <- per_data_Filterednzv$dfs_adoption_binary

Y <- as.numeric(as.character(per_data_Filterednzv$dfs_adoption_binary))
Y

#PLS-DA
result.plsda.srbct <- plsda(X, Y) # run the method
plotIndiv(result.plsda.srbct) # plot the samples
plotVar(result.plsda.srbct) # plot the variables
plotLoadings(result.plsda.srbct, method = 'mean', contrib = 'max')  

selectVar(result.plsda.srbct, comp = 2)$name 


#sPLS-DA
splsda.result <- splsda(X, Y, keepX = c(60,40)) # run the method
plotIndiv(splsda.result) # plot the samples
plotVar(splsda.result) # plot the variables

# extract the variables used to construct the first latent component
selectVar(splsda.result, comp = 2)$name 
# depict weight assigned to each of these variables
plotLoadings(splsda.result, method = 'mean', contrib = 'max')  

 




#############################################################    
############# EXAMINE DATA DIMENSION REDUCTION -----
#############################################################
# https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/115-famd-factor-analysis-of-mixed-data-in-r-essentials/
library("FactoMineR")
library("factoextra")

?MFA
# === Biophysical contexts ===
biophysical_context <- intersect(factors_category$column_name_new[factors_category$category_1=="Biophysical context"], colnames(per_data_analysis_Filterednzv))
res.famd.biophysical_context <- FAMD(per_data_analysis_Filterednzv%>%dplyr::select(all_of(biophysical_context)), graph = FALSE)

#Eigenvalues / Variances
#The proportion of variances retained by the different dimensions (axes) can be extracted using 
#the function get_eigenvalue() [factoextra package] as follow:
#get_eigenvalue(res.famd): Extract the eigenvalues/variances retained by each dimension (axis).
eig.val.biophysical_context <- get_eigenvalue(res.famd.biophysical_context)
head(eig.val.biophysical_context)

#The function fviz_eig() or fviz_screeplot() [factoextra package] can be used to draw the 
#scree plot (the percentages of inertia explained by each FAMD dimensions):
#fviz_eig(res.famd): Visualize the eigenvalues/variances.
fviz_screeplot(res.famd.biophysical_context)

#Graph of variables
#All variables
#The function get_mfa_var() [in factoextra] is used to extract the results for variables. 
#By default, this function returns a list containing the coordinates, the cos2 and the contribution of all variables:
#get_famd_var(res.famd): Extract the results for quantitative and qualitative variables.
var.biophysical_context <- get_famd_var(res.famd.biophysical_context)
var.biophysical_context
# Coordinates of variables: variable coordinates in the component space (like loadings)
(var.biophysical_context$coord)
# Cos2: quality of representation on the factore map
(var.biophysical_context$cos2)
# Contributions to the  dimensions: how much each variable contributes to each dimension (%)
(var.biophysical_context$contrib)

#The following figure shows the correlation between variables - both quantitative and qualitative variables -
#and the principal dimensions, as well as, the contribution of variables to the dimensions 1 and 2. 
#The following functions [in the factoextra package] are used:
#fviz_famd_var() to plot both quantitative and qualitative variables
#fviz_famd_ind(res.famd), fviz_famd_var(res.famd): Visualize the results for individuals and variables, respectively.
# Plot of variables
# Contribution to the  dimensions
fviz_famd_var(res.famd.biophysical_context, repel = TRUE)
fviz_contrib(res.famd.biophysical_context, "var", axes = 1)
fviz_contrib(res.famd.biophysical_context, "var", axes = 2)
fviz_contrib(res.famd.biophysical_context, "var", axes = 3)
fviz_contrib(res.famd.biophysical_context, "var", axes = 4)
fviz_famd_var(res.famd.biophysical_context, repel = TRUE, col.var = "contrib")

# === Farm management characteristics ===
farm_characteristics <- intersect(factors_category$column_name_new[grepl("^Farm management characteristics", factors_category$category_1)], colnames(per_data_analysis_Filterednzv))
res.famd.farm_characteristics<-FAMD(per_data_analysis_Filterednzv%>%dplyr::select(all_of(farm_characteristics)), graph = FALSE)

eig.val.farm_characteristics <- get_eigenvalue(res.famd.farm_characteristics)
head(eig.val.farm_characteristics)

fviz_screeplot(res.famd.farm_characteristics)

var.farm_characteristics <- get_famd_var(res.famd.farm_characteristics)
var.farm_characteristics
(var.farm_characteristics$coord)
(var.farm_characteristics$cos2)
(var.farm_characteristics$contrib)

fviz_famd_var(res.famd.farm_characteristics, repel = TRUE)
fviz_contrib(res.famd.farm_characteristics, "var", axes = 1)
fviz_contrib(res.famd.farm_characteristics, "var", axes = 2)
fviz_contrib(res.famd.farm_characteristics, "var", axes = 3)
fviz_contrib(res.famd.farm_characteristics, "var", axes = 4)
fviz_famd_var(res.famd.farm_characteristics, repel = TRUE, col.var = "contrib")

# === Farmer behaviour ===
farmer_behaviour <- intersect(factors_category$column_name_new[grepl("^Farmers' behaviour", factors_category$category_1)], colnames(per_data_analysis_Filterednzv))
res.famd.farmer_behaviour<-FAMD(per_data_analysis_Filterednzv%>%dplyr::select(all_of(farmer_behaviour)), graph = FALSE)

eig.val.farmer_behaviour <- get_eigenvalue(res.famd.farmer_behaviour)
head(eig.val.farmer_behaviour)

fviz_screeplot(res.famd.farmer_behaviour)

var.farmer_behaviour <- get_famd_var(res.famd.farmer_behaviour)
var.farmer_behaviour
(var.farmer_behaviour$coord)
(var.farmer_behaviour$cos2)
(var.farmer_behaviour$contrib)

fviz_famd_var(res.famd.farmer_behaviour, repel = TRUE)
fviz_contrib(res.famd.farmer_behaviour, "var", axes = 1)
fviz_contrib(res.famd.farmer_behaviour, "var", axes = 2)
fviz_contrib(res.famd.farmer_behaviour, "var", axes = 3)
fviz_contrib(res.famd.farmer_behaviour, "var", axes = 4)
fviz_famd_var(res.famd.farmer_behaviour, repel = TRUE, col.var = "contrib")

### Cognitive factors 
#-> perceived control: 
#agroecol_perspective_7: Power and freedom to solve problems collectively	5= Completely agree; 4= Somewhat agree; 3= Neutral; 2= Somewhat disagree; 1= Completely disagree; 0= I don't know	categorical	ordinal	
#->	perceived benefit (financial)	
#agroecol_perspective_12: "Perception of agroecological farming as a business decision	"	5= Completely agree; 4= Somewhat agree; 3= Neutral; 2= Somewhat disagree; 1= Completely disagree; 0= I don't know	
#->		perceived benefits (environmental)
#agroecol_perspective_2: Perceived benefit of being in nature	5= Completely agree; 4= Somewhat agree; 3= Neutral; 2= Somewhat disagree; 1= Completely disagree; 0= I don't know	

### Social factor 
#->	descriptive norm	
#agroecol_perspective_3: Perceived community care for nature	5= Completely agree; 4= Somewhat agree; 3= Neutral; 2= Somewhat disagree; 1= Completely disagree; 0= I don't know	

### Dispositional factors 
#->	environmental concern	
#agroecol_perspective_1: Care for nature	5= Completely agree; 4= Somewhat agree; 3= Neutral; 2= Somewhat disagree; 1= Completely disagree; 0= I don't know	
#->	farming objective (environmental)	
#agroecol_perspective_4: Care for land and nature on the farm	5= Completely agree; 4= Somewhat agree; 3= Neutral; 2= Somewhat disagree; 1= Completely disagree; 0= I don't know	
#->	identity	
#agroecol_perspective_5: Identification as an agroecological farmer	5= Completely agree; 4= Somewhat agree; 3= Neutral; 2= Somewhat disagree; 1= Completely disagree; 0= I don't know	
#->	moral concern	
#agroecol_perspective_10: Preference for locally produced food	5= Completely agree; 4= Somewhat agree; 3= Neutral; 2= Somewhat disagree; 1= Completely disagree; 0= I don't know	
#->	resistance to change	
#agroecol_perspective_13: Perception of current farming systems	5= Completely agree; 4= Somewhat agree; 3= Neutral; 2= Somewhat disagree; 1= Completely disagree; 0= I don't know	


# === Financial capital ===
financial_capital<- intersect(factors_category$column_name_new[grepl("^Financial capital", factors_category$category_1)], colnames(per_data_analysis_Filterednzv))
res.famd.financial_capital<-FAMD(per_data_analysis_Filterednzv%>%dplyr::select(all_of(financial_capital)), graph = FALSE)

eig.val.financial_capital <- get_eigenvalue(res.famd.financial_capital)
head(eig.val.financial_capital)

fviz_screeplot(res.famd.financial_capital)

var.financial_capital <- get_famd_var(res.famd.financial_capital)
var.financial_capital
(var.financial_capital$coord)
(var.financial_capital$cos2)
(var.financial_capital$contrib)

fviz_famd_var(res.famd.financial_capital, repel = TRUE)
fviz_contrib(res.famd.financial_capital, "var", axes = 1)
fviz_contrib(res.famd.financial_capital, "var", axes = 2)
fviz_contrib(res.famd.financial_capital, "var", axes = 3)
fviz_contrib(res.famd.financial_capital, "var", axes = 4)
fviz_famd_var(res.famd.financial_capital, repel = TRUE, col.var = "contrib")

### Income 
#->	sources	
#income_sources.casual_labour
#income_sources.livestock

# === Human capital ===
human_capital<- intersect(factors_category$column_name_new[grepl("^Human capital", factors_category$category_1)], colnames(per_data_analysis_Filterednzv))
res.famd.human_capital<-FAMD(per_data_analysis_Filterednzv%>%dplyr::select(all_of(human_capital)), graph = FALSE)

eig.val.human_capital <- get_eigenvalue(res.famd.human_capital)
head(eig.val.human_capital)


fviz_screeplot(res.famd.human_capital)

var.human_capital <- get_famd_var(res.famd.human_capital)
var.human_capital
(var.human_capital$coord)
(var.human_capital$cos2)
(var.human_capital$contrib)

fviz_famd_var(res.famd.human_capital, repel = TRUE)
fviz_contrib(res.famd.human_capital, "var", axes = 1)
fviz_contrib(res.famd.human_capital, "var", axes = 2)
fviz_contrib(res.famd.human_capital, "var", axes = 3)
fviz_contrib(res.famd.human_capital, "var", axes = 4)
fviz_famd_var(res.famd.human_capital, repel = TRUE, col.var = "contrib")

### Household
#-> demographic
#education_level_female_finished: Level of education of most household female	0= none or no female living the household; 1= primary; 2= secondary; 3= post-secondary	
#education_level_male_finished: Level of education of most household male	0= none or no male living in household; 1= primary; 2= secondary; 3= post-secondary	
#education_level_household_finished: Level of education of most household members	0= none; 1= primary; 2= secondary; 3= post-secondary	

#-> food security
#access_diversified_food: food security	Level of access to diversified food	5= Good access. 4= Fairly good. 3= Moderate access. 2= Limited access. 1= No access at all.	
#access_healthy_food: food security	Level of access to healthy food	5= Good access. 4= Fairly good. 3= Moderate access. 2= Limited access. 1= No access at all.	
#access_seasonal_food: food security	Level of access to seasonal food	5= Good access. 4= Fairly good. 3= Moderate access. 2= Limited access. 1= No access at all.	
#access_traditional_food: food security	Level of access to traditional food	5= Good access. 4= Fairly good. 3= Moderate access. 2= Limited access. 1= No access at all.	

# === Natural capital ===
natural_capital<- intersect(factors_category$column_name_new[grepl("^Natural capital", factors_category$category_1)], colnames(per_data_analysis_Filterednzv))
res.famd.natural_capital<-FAMD(per_data_analysis_Filterednzv%>%dplyr::select(all_of(natural_capital)), graph = FALSE)

eig.val.natural_capital <- get_eigenvalue(res.famd.natural_capital)
head(eig.val.natural_capital)

fviz_screeplot(res.famd.natural_capital)

var.natural_capital <- get_famd_var(res.famd.natural_capital)
var.natural_capital
(var.natural_capital$coord)
(var.natural_capital$cos2)
(var.natural_capital$contrib)

fviz_famd_var(res.famd.natural_capital, repel = TRUE)
fviz_contrib(res.famd.natural_capital, "var", axes = 1)
fviz_contrib(res.famd.natural_capital, "var", axes = 2)
fviz_contrib(res.famd.natural_capital, "var", axes = 3)
fviz_contrib(res.famd.natural_capital, "var", axes = 4)
fviz_famd_var(res.famd.natural_capital, repel = TRUE, col.var = "contrib")

per_data_analysis_FAMD <- per_data_analysis_Filterednzv %>%
  # === Biophysical context ===
  dplyr::mutate(
    rainfall_timing_perception_index = 
      as.numeric(as.character(rainfall_timing_change_perception.stopearlier)) +
      as.numeric(as.character(rainfall_timing_change_perception.stoplater)) +
      as.numeric(as.character(rainfall_timing_change_perception.startearlier)) +
      as.numeric(as.character(rainfall_timing_change_perception.startlater)))%>%
  select(-c(rainfall_timing_change_perception.stopearlier,
            rainfall_timing_change_perception.stoplater,
            rainfall_timing_change_perception.startearlier,
            rainfall_timing_change_perception.startlater))%>%
  # === Financial capital ===
  #Remove redundant factors: num_income_sources provide an summary for income sources
  select(-c(income_sources.casual_labour,
            income_sources.livestock))


dim(per_data_analysis_FAMD) #[1] 200 216 #200 farmers; 216 variables evaluated




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
            access_info_exchange, #keep  num_info_exchange_sources
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

dim(per_adoptionBinary1) #[1] 200 195 #200 farmers; 195 variables retained
any(is.na(per_adoptionBinary1))

per_adoptionBinary2<- per_data_analysis_FilteredCor%>%
  select(-c(num_info_exchange_consumers, #keep access_info_exchange_consumers,  
            num_info_exchange_extension,  #keep access_info_exchange_extension,
            num_info_exchange_farmers, #keep access_info_exchange_farmers
            num_info_exchange_ngo , #keep access_info_exchange_ngo
            num_info_exchange_researchers,  #keep access_info_exchange_researchers
            num_info_exchange_traders, #keep access_info_exchange_traders
            num_info_exchange_sources, #keep access_info_exchange
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

dim(per_adoptionBinary2) #[1] 200 195 #200 farmers; 195 variables retained
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
write.csv(per_adoptionBinary1,"per_data_adoptionBinary1.csv",row.names=FALSE)

#per_adoptionBinary2
columns_continuous <- intersect(per_summary_numerical$column_name_new, colnames(per_adoptionBinary2))
# Center (mean = 0) and scale (sd = 0.5) the continuous variables:
per_adoptionBinary2[,columns_continuous] = 0.5*scale(per_adoptionBinary2[,columns_continuous])
dim(per_adoptionBinary2) #[1] 200 200 #200 farmers; 200 variables retained
write.csv(per_adoptionBinary2,"per_data_adoptionBinary2.csv",row.names=FALSE)


############################################################# 
##NO TOCAR----
adoption_binary <- per_adoptionBinary1$dfs_adoption_binary

factors_binary <- per_adoptionBinary1[ , !(names(per_adoptionBinary1) %in% c("dfs_adoption_binary"))] # Remove target from feature matrix
factors_binary <- as.matrix(do.call(cbind, factors_binary))
factors_binary <- as.data.frame(factors_binary)
  dplyr::select(-dfs_total_area)%>%
  dplyr::select(-all_of(unique(highCor1$data1)))
  dplyr::select(-"dfs_agroforestry_adoption",
                -"dfs_homegarden_adoption"  ,       
                -"dfs_intercropping_adoption")

# Create a data frame with target and predictors
data <- data.frame(target = adoption_binary, factors_binary)

write.csv(data,"data.csv",row.names=FALSE)
