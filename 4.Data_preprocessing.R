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
  filter(is.na(remove))
factors_list$category_1[grepl("^Farm management characteristics", factors_list$category_1)] <- "Farm management characteristics"
factors_list$category_1[grepl("^Financial capital", factors_list$category_1)] <- "Financial capital"
factors_list$category_1[grepl("^P&I context", factors_list$category_1)] <- "P&I context"
factors_list$category_1[grepl("^P&I context", factors_list$category_1)] <- paste0(
  factors_list$category_1[grepl("^P&I context", factors_list$category_1)],"_",  factors_list$category_2[grepl("^P&I context", factors_list$category_1)])
factors_list$category_1[grepl("^P&I context_knowledge", factors_list$category_1)] <- "P&I context_knowledge"

per_data_clean<- read.csv("per_data_clean.csv",sep=",")
per_summary_categorical<-read.csv("per_summary_categorical.csv",sep=",") #598
per_summary_numerical<-read.csv("per_summary_numerical.csv",sep=",") #75 factors

factors_category<-per_summary_numerical%>%dplyr::select(column_name_new, category_1,category_2)%>%
  rbind(per_summary_categorical%>%dplyr::select(column_name_new,category_1,category_2))%>%
  distinct()

sort(unique(factors_category$category_1))

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

dim(per_data_analysis) #[1] 200 289 #200 farmers; 289 variables evaluated

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


dim(per_data_Binary) #[1] 200 297 #200 farmers; 297 variables evaluated

#############################################################    
#############################################################
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

dim(per_data_Binary) #[1] 200 297 #200 farmers; 297 variables retained

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
nzv <- caret::nearZeroVar(per_data_scaled, saveMetrics= TRUE) 
nzv # the variables with nzv== TRUE should be remove

nzv_list <- nearZeroVar(per_data_scaled)

nzv_factors<- per_data_scaled[, nzv_list]
print(nzv_factors)
view(dfSummary(nzv_factors))

## Remove nzv variables from data
per_data_Filterednzv<- per_data_scaled[, -nzv_list]

dim(per_data_Filterednzv) #[1] 200 221 #200 farmers; 221 variables retained

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

dim(per_data_Filterednzv) #[1] 200 221 #200 farmers; 222 variables retained


#write.csv(per_data_Filterednzv,"per_data_Filterednzv.csv",row.names=FALSE)


####
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
############# Sparse weighted k-means for mixed data -----
#############################################################
#https://cran.r-project.org/web/packages/vimpclust/vignettes/sparsewkm.html
#https://cran.r-project.org/web/packages/vimpclust/vimpclust.pdf
#Sparse and group-sparse clustering for mixed data An illustration of the vimpclust package
install.packages("vimpclust")
library(vimpclust)
head(HDdata)
str(HDdata)
names(per_data_sewkm)
per_data_sewkm<- per_data_Filterednzv%>%
  select(-kobo_farmer_id)%>%
  select(crop)
rownames(per_data_sewkm) <- per_data_Filterednzv$kobo_farmer_id

str(per_data_sewkm)
# === Training the sparsewkm function ===
res <- sparsewkm(X = per_data_sewkm[, !(names(per_data_sewkm) == "dfs_adoption_binary")], centers = 3)
res$W[,1:5]
plot(res, what="weights.features")

avg_weights <- rowMeans(res$W)
top_features <- sort(avg_weights, decreasing = TRUE)[1:30]
library(ggplot2)

ggplot(data.frame(
  Feature = names(top_features),
  Weight = top_features
), aes(x = reorder(Feature, Weight), y = Weight)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 20 Features by Average SparseWKM Weight",
       x = "Feature", y = "Average Weight") +
  theme_minimal()

plot(res, what="weights.levels", Which=c(1:30))
plot(res, what="weights.levels", Which=c(31:40))

#Additional plots
#The number of selected features and the number of selected levels for a given value of 
#the regularization parameter lambda provide valuable information. These two criteria may 
#be illustrated using options what=sel.features and what=sel.levels in the plot function.

plot(res, what="sel.features")
plot(res, what="sel.levels")
#The two curves are very similar and show how the number of selected features 
#relevant for the clustering rapidly decreases with lambda.
#Clustering may also be assessed using criteria based on the evolution of the explained variance.
#The latter is computed as the ratio between the between-class variance and the global variance
#in the data, and represents the ratio of information explained by the clustering. 
#The explained variance may be computed on all features used for clustering, without taking the weights into account,
#or in a weighted fashion, by taking the computed weights into account and thus suppressing all features discarded 
#by the algorithm. In the latter case and for large lambda, the explained weighted variance results in being computed 
#using one feature only, maxhr.

plot(res, what="expl.var")
plot(res, what="w.expl.var")

#These two criteria, combined with the number of selected features, may be used to select the 
#appropriate regularization parameter lambda. A good choice for lambda should preserve a high percentage
#of variance explained by the clustering, while discarding a large number of features. 
#This amounts to a trade-off between the quality of the model and its parcimony.


# === Comparing the clustering with the “ground truth” ===

library(mclust)
sapply(c(1,7,20), function(x) {adjustedRandIndex(res$cluster[,x],per_data_sewkm$dfs_adoption_binary)})
#Output: 0.2585535 0.2513924 0.1329980
#Lambda 1 (lowest penalty): ARI = 0.2585535
#Lambda 7 (moderate penalty): ARI = 0.2513924
#Lambda 12 (high penalty): ARI = 0.1329980
#This demonstrates a decline in Adjusted Rand Index (ARI) as lambda increases, i.e., as regularization increases and fewer features are kept.

table(per_data_sewkm$dfs_adoption_binary, res$cluster[,1])
#   1  2  3
#0 70 54 20
#1  2 3  51

#Correct assignments (assuming cluster 3 = adopters): 70 + 51 = 121
#Total = 200
#Accuracy ≈ 121 / 200 = 60.5%

table(per_data_sewkm$dfs_adoption_binary, res$cluster[,7])
#    1  2  3
#0  20 55 69
#1  50  5  1
table(per_data_sewkm$dfs_adoption_binary, res$cluster[,20])
#   1  2  3
#0 69 49 29
#1  5 10 41

# === Get the list of factors included in each lamba ===
#Cluster compostion
info_clust(res, 1, X = per_data_sewkm)
length(which(res$W[,1] > 0)) #218 factors
length(which(res$W[,2] > 0)) #165 factors
length(which(res$W[,3] > 0)) #120 factors
length(which(res$W[,5] > 0)) #67 factors
length(which(res$W[,6] > 0)) #52 factors
which(res$W[,6] > 0)
length(which(res$W[,10] > 0)) #16 factors
length(which(res$W[,12] > 0)) #4 factors
which(res$W[,12] > 0)

# Assuming 'res' is your sparsewkm result object

# Extract the weight matrix
W <- res$W

# Initialize an empty list to store selected features per lambda
selected_features_list <- lapply(1:ncol(W), function(i) {
  selected <- rownames(W)[W[, i] > 0]
  data.frame(
    lambda = paste0("lambda_", i),
    feature = selected,
    stringsAsFactors = FALSE
  )
})

# Combine into a single data frame
selected_features_df <- do.call(rbind, selected_features_list)

# View or save the data
head(selected_features_df)
# write.csv(selected_features_df, "selected_features_by_lambda.csv", row.names = FALSE)


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
############# CluMix -----
#############################################################  
#https://github.com/cran/CluMix
library(cluster)  # for daisy()
load("CluMix/mixdata.rda")
str(mixdata)
#CHECK tengo que transformar los ordinal variables a Ord.factor mirar function dist.subjects

per_data_CluMix<- per_data_Filterednzv%>%
  select(-kobo_farmer_id)

generate_daisy_type <- function(df) {
  # Initialize type lists
  type_list <- list(asymm = c(), ordered = c(), numeric = c())
  
  for (colname in names(df)) {
    col <- df[[colname]]
    
    if (is.factor(col)) {
      n_levels <- nlevels(col)
      if (n_levels == 2) {
        type_list$asymm <- c(type_list$asymm, colname)
      } else if (n_levels > 2) {
        type_list$ordered <- c(type_list$ordered, colname)
      }
    } else if (is.numeric(col)) {
      type_list$numeric <- c(type_list$numeric, colname)
    }
  }
  
  # Remove any empty types
  type_list <- type_list[lengths(type_list) > 0]
  
  return(type_list)
}
rownames(per_data_CluMix) <- per_data_Filterednzv$kobo_farmer_id

type_spec <- generate_daisy_type(per_data_CluMix)
type_spec

sort(unique(per_data_CluMix$male_land_tenure_hold_proportion))

# === Distance matrix ===
#https://www.rdocumentation.org/packages/cluster/versions/2.1.8/topics/daisy
# For clustering farmes using mixed type data variables
###--- Gower dissimilarity matrix
per_gower_dist <- daisy(per_data_CluMix, metric = "gower", type = type_spec)
colnames(per_data_CluMix)[190]
per_gower_dist
#TO CHECK hay un problema con male_land_tenure_hold_proportion solo tiene valores de 0 y 100


source("CluMix/similarity.subjects.r")

per_gower_dist2 <- dist.subjects(per_data_CluMix)
per_gower_dist2

per_hclust<- dendro.subjects(per_data_CluMix)
plot(per_hclust)

# === Cluster farmers ===
#https://r-charts.com/part-whole/hclust/
#Group farmers by similarity in the structure of their responses.
###--- Hierarchical clustering ---
per_hclust <- hclust(per_gower_dist, method = "ward.D2")
per_hclust
plot(per_hclust) # Dendrogram
rect.hclust(per_hclust, k = 2) # Dendrogram with 4 clusters

# Cut the dendrogram into k clusters (e.g., 3 clusters)
k <- 3
per_clusters <- cutree(per_hclust, k = k)
table(per_clusters)

per_data<-per_data_CluMix
per_data$cluster <- per_clusters[rownames(per_data)]


table(per_data$cluster, per_data$dfs_adoption_binary)


###--- PAM clustering (if you want to pre-specify k) ---
pam_result

# === Cluster factors ===
library(extracat)
clumix_files <- list.files("CluMix", pattern = "\\.R$", full.names = TRUE)
sapply(clumix_files, source)
source("CluMix/distcor.r")
source("CluMix/distmap.r")

###--- CluMix-ama ---
#The CluMix-ama (association measures approach) method consists in combination of different similarity measures. A novel 
#strategy based on category reordering is suggested for measuring the association between a 
#multi-categorical and any other type of variable. 

per_CluMixama <- dist.variables(per_data_CluMix, method="association")
distmap(per_data_CluMix, what="subjects")

heatmap(as.matrix(per_CluMixama))

###--- CluMix-dcor ---
#The CluMix-dcor (distance correlation) approach is based on a novel similarity
#measure, which is derived using the concept of generalized distance correlations [9].
per_CluMixdcor <- dist.variables(per_data_CluMix, method="distcor")
distmap(per_data_CluMix, what="variables", method="distcor")
distmap(per_data_CluMix, what="subjects", method="distcor")

heatmap(as.matrix(per_CluMixdcor))







#############################################################    
############# MFA/HCPC -----
#############################################################
library(FactoMineR)
library(factoextra)

#Goal: Reduce high-dimensional grouped factors into a smaller number of 
#interpretable axes (principal components), while considering the 
#structure of groups (e.g., capitals, context, behavior).

# === Data format for analysis ===
per_factors_list <- as.data.frame(colnames(per_data_Filterednzv))%>%
  rename("column_name_new"= "colnames(per_data_Filterednzv)")%>%
  left_join(factors_category)%>%
  select(-category_2)

per_factors_list$category_1[grepl("^crop_type", per_factors_list$column_name_new)] <- "Farm management characteristics"
per_factors_list$category_1[grepl("^district.dist", per_factors_list$column_name_new)] <- "P&I context_general"
per_factors_list$category_1[grepl("^marital_status.", per_factors_list$column_name_new)] <- "Human capital"
per_factors_list$category_1[grepl("^read_write.", per_factors_list$column_name_new)] <- "Human capital"
per_factors_list$category_1[grepl("^year_assessment.", per_factors_list$column_name_new)] <- "Biophysical context"
per_factors_list<-per_factors_list%>%filter(category_1!="xxx")%>%filter(category_1!="outcome")
head(per_factors_list)
length(unique(per_factors_list$column_name_new)) #222 factors
sort(unique(per_factors_list$category_1)) #222 factors

# I converted categorical ordinal variables with more than 2 factor levels to numeric
#since MFA transform each level to a binary column
per_data_mfa<-per_data_Filterednzv%>%
  dplyr::select(-dfs_adoption_binary)%>%
  mutate(across(where(~ is.factor(.) && nlevels(.) > 2),~ as.numeric(.)))%>%
  mutate(across(
    where(~ is.factor(.) && all(levels(.) %in% c("0", "1"))),
    ~ factor(ifelse(. == "1", "yes", "no"), levels = c("no", "yes"))
  ))
str(per_data_mfa)

sort(unique(per_data_mfa$main_crops_annual ))

str(per_data_mfa)
rownames(per_data_mfa) <- per_data_mfa$kobo_farmer_id

# Step 2: Create cleaned group definitions (handling mixed types)
group_def <- per_factors_list %>%
  filter(!is.na(category_1),
         column_name_new %in% names(per_data_mfa)) %>%
  rowwise() %>%
  mutate(var_class = class(per_data_mfa[[column_name_new]])[1]) %>%
  ungroup() %>%
  mutate(group_type = case_when(
    var_class == "factor" ~ "n",
    var_class %in% c("numeric", "integer") ~ "s",
    TRUE ~ "other"
  )) %>%
  mutate(category_1_type = paste0(category_1, " (", group_type, ")")) %>%
  group_by(category_1_type, group_type) %>%
  summarise(vars = list(column_name_new), .groups = "drop") %>%
  mutate(group_size = lengths(vars))

group_def

# Extract only grouped variables
mfa_data <- per_data_mfa[, unlist(group_def$vars)]

# Prepare MFA inputs
group_sizes <- group_def$group_size
group_types <- group_def$group_type
group_names <- group_def$category_1_type
group_names
group_types

# === MFA - Multiple Factor Analysis  ===
#https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/116-mfa-multiple-factor-analysis-in-r-essentials/#google_vignette

set.seed(123)
res_mfa <- MFA(mfa_data,
               group = group_sizes,
               type = group_types,
               name.group = group_names,
               #num.group.sup = group_supplementary_indices,
               ncp = 5,
               graph = FALSE)
res_mfa

#see how much variation is explain
res_mfa$eig
fviz_screeplot(res_mfa, addlabels = TRUE, ncp = 5)

mfa_vars <- res_mfa$call$X
mfa_vars
cat_vars_mfa <- names(mfa_vars)[sapply(mfa_vars, is.factor)]
cat_vars_mfa

plot(res_mfa,choix="ind",partial="all")

var <- get_mfa_var(res_mfa)
var$coord

important_vars_dim1 <- which(abs(var$coord[,1]) > 0.5)
important_vars_dim1
important_vars_dim2 <- which(abs(var$coord[,2]) > 0.5)
important_vars_dim2
important_vars_dim3 <- which(abs(var$coord[,3]) > 0.5)
important_vars_dim3

head(res_mfa$ind$coord)  # Coordinates of individuals (farmers)
res_mfa$quali.var$coord       # for qualitative (categorical) variables
res_mfa$quanti.var$coord      # for quantitative (numeric) variables
res_mfa$quali.var$contrib     # contribution to axes
res_mfa$quanti.var$contrib
fviz_contrib(res_mfa, "group", axes = 1)
fviz_contrib(res_mfa, "group", axes = 2)
fviz_contrib(res_mfa, "group", axes = 3)
fviz_contrib(res_mfa, "group", axes = 4)

fviz_contrib(res_mfa, "quanti.var", axes = 1)
fviz_contrib(res_mfa, "quanti.var", axes = 2)
fviz_contrib(res_mfa, "quanti.var", axes = 3)
fviz_contrib(res_mfa, "quanti.var", axes = 4)

fviz_contrib(res_mfa, "quali.var", axes = 1)
fviz_contrib(res_mfa, "quali.var", axes = 2)
fviz_contrib(res_mfa, "quali.var", axes = 3)
fviz_contrib(res_mfa, "quali.var", axes = 4)

# === HCPC – Hierarchical Clustering on Principal Components  ===
#https://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials/
#Goal: Identify groups (clusters) of farmers based on their positions
#in the reduced MFA space (i.e., their profiles).
res.hcpc <- HCPC(res_mfa, graph = FALSE, nb.clust = 3 )
res.hcpc$desc.var

#number of optimal clusters estimated
res.hcpc$call$t$nb.clust

fviz_dend(res.hcpc, 
          cex = 0.7,                     # Label size
          palette = "jco",               # Color palette see ?ggpubr::ggpar
          rect = TRUE, 
          rect_fill = TRUE, # Add rectangle around groups
          rect_border = "jco",           # Rectangle color
          labels_track_height = 0.8      # Augment the room for labels
)

fviz_cluster(res.hcpc,
             repel = TRUE,            # Avoid label overlapping
             show.clust.cent = TRUE, # Show cluster centers
             palette = "jco",         # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map")

plot(res.hcpc, choice = "3D.map")
head(res.hcpc$data.clust, 10)

c1_continues_var<- as.data.frame(res.hcpc$desc.var$quanti[1])
c1_continues_var
c1_categorical_var<- as.data.frame(res.hcpc$desc.var$quali[1])
c1_categorical_var
c2_continues_var<- as.data.frame(res.hcpc$desc.var$quanti[2])
c2_categorical_var<- as.data.frame(res.hcpc$desc.var$quali[2])
c2_categorical_var
c3_continues_var<- as.data.frame(res.hcpc$desc.var$quanti[3])
c3_categorical_var<- as.data.frame(res.hcpc$desc.var$quali[3])
c3_categorical_var
table(mfa_data$gender, res.hcpc$data.clust$clust) |> chisq.test()
summary(mfa_data$gender)


res.hcpc$desc.axes$quanti
res.hcpc$desc.ind$para

# Extract farmer IDs per cluster
head(res.hcpc$data.clust)


# Add IDs if not already included
res.hcpc$data.clust$kobo_farmer_id <- rownames(res.hcpc$data.clust)
res.hcpc


# Extract cluster info and farmer IDs
cluster_assignments <- res.hcpc$data.clust %>%
  dplyr::select(clust) %>%
  dplyr::mutate(kobo_farmer_id = rownames(res.hcpc$data.clust))
cluster_assignments

# Join with your main dataset
per_data_with_HCPCcluster <- per_data_Filterednzv %>%
  left_join(cluster_assignments, by = "kobo_farmer_id")

adoption_by_HCPCcluster <- per_data_with_HCPCcluster %>%
  group_by(clust, dfs_adoption_binary) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(clust) %>%
  mutate(percent = round(100 * n / sum(n), 1))

adoption_by_HCPCcluster

library(ggplot2)

ggplot(adoption_by_HCPCcluster, aes(x = factor(clust), y = percent, fill = dfs_adoption_binary)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Cluster", y = "DFS Adoption (%)", fill = "Adoption",
       title = "DFS Adoption across Farmer Clusters: MFA/HCPC") +
  theme_minimal()



#############################################################    



#############################################################    
############# iClusterBayes BAYESIAN CLUSTER -----
#############################################################
# === Data format for analysis ===
dim(per_data_Filterednzv)
library(tibble)
per_data_icluster<-per_data_Filterednzv%>%
  dplyr::select(-dfs_adoption_binary)%>%
  mutate(across(where(~ is.factor(.) && nlevels(.) > 2),~ as.numeric(.)))%>%
  {rownames(.) <- NULL; .} %>%
  tibble::column_to_rownames(var = "kobo_farmer_id")


per_data_icluster_continuous<-as.matrix(
  per_data_icluster %>%
  select(where(is.numeric)))
str(per_data_icluster_continuous)
all(rownames(per_data_icluster_count)==rownames(per_data_icluster_continuous))
#[1] TRUE

per_data_icluster_binary<-as.matrix(
  per_data_icluster %>%
  select(where(is.factor))%>%
    mutate(across(everything(), ~ as.integer(as.character(.)))))
str(per_data_icluster_binary)
all(rownames(per_data_icluster_binary)==rownames(per_data_icluster_binary))
#[1] TRUE

ncol(per_data_icluster_continuous) + 
  ncol(per_data_icluster_binary)

# === iClusterBayes ===
library(iClusterPlus)
library(parallel)
iManual()

set.seed(123) 
date()
#[1] "Fri Mar 28 00:45:27 2025"

## Model tuning using tune.iClusterBayes
tune.clusterBayes = tune.iClusterBayes(cpus=1,dt1=per_data_icluster_continuous, dt2=per_data_icluster_binary, 
                               type=c("gaussian","binomial"),#,"gaussian","binomial"),
                            K=1:6,n.burnin=18000,
                               n.draw=12000,prior.gamma=c(0.5,0.5),#0.5),
                            sdev=0.05,thin=3)

save.image(file="tune.clusterBayes.RData") 
load("tune.clusterBayes.RData")

## Model selection
tune.clusterBayes

#BIC or deviance ratio may be used as a criterion to select an optimal value for k, the 
#parameter that deﬁnes the number of clusters.
allBIC = NULL
devratio = NULL
nK = length(tune.clusterBayes$fit)

for(i in 1:nK){
   allBIC = c(allBIC,tune.clusterBayes$fit[[i]]$BIC)
   devratio = c(devratio,tune.clusterBayes$fit[[i]]$dev.ratio) 
   }

#From the BIC and deviance ratio plots, we see that k=3 is an optimal solution. However, if 
#the data are noisy, the deviance ratio may keep increasing and the BIC may keep decreasing 
#when k increases.
par(mar=c(4.0,4.0,0.5,0.5),mfrow=c(1,2))
plot(1:nK, allBIC,type="b",xlab="k",ylab="BIC")#,pch=c(1,1,19,1,1,1))
plot(1:nK,devratio,type="b",xlab="k",ylab="Deviance ratio")#,pch=c(1,1,19,1,1,1))

#Choose the K where: 
#- BIC is minimized
#- Deviance ratio begins to level off (the "elbow")
#k=3
best.cluster = tune.clusterBayes$fit[[3]]$clusters
best.cluster
table(best.cluster)

best.fit<-tune.clusterBayes$fit[[2]]
best.fit

# 2. Extract cluster assignments
cluster_assignments <- best.fit$clusters

# 3. Combine with farmer IDs
farmer_iClusterBayes <- data.frame(
  kobo_farmer_id = rownames(per_data_icluster),  # matches your dataset
  iClusterBayes = cluster_assignments
)

# 4. Split into a list of farmers per cluster
farmers_by_cluster <- split(farmer_clusters$FarmerID, farmer_clusters$Cluster)


per_data_with_iClusterBayes <- per_data_Filterednzv %>%
  left_join(farmer_iClusterBayes, by = "kobo_farmer_id")

adoption_by_iClusterBayes <- per_data_with_iClusterBayes %>%
  group_by(iClusterBayes, dfs_adoption_binary) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(iClusterBayes) %>%
  mutate(percent = round(100 * n / sum(n), 1))

adoption_by_iClusterBayes


ggplot(adoption_by_iClusterBayes, aes(x = factor(iClusterBayes), y = percent, fill = dfs_adoption_binary)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Cluster", y = "DFS Adoption (%)", fill = "Adoption",
       title = "DFS Adoption across Farmer Clusters: iClusterBayes") +
  theme_minimal()

## Feature Importance from Posterior Probabilities

threshold <- 0.9 #0 (not important at all) → 1 (very important)
beta_pp <- best.fit$beta.pp

# Continuous variables
top_continuous <- which(beta_pp[[1]] > threshold)
top_continuous
top_continuous_features <- colnames(per_data_icluster_continuous)[top_continuous]
top_continuous_features

# Binary variables
top_binary <- which(beta_pp[[2]] > threshold)
top_binary_features <- colnames(per_data_icluster_binary)[top_binary]
top_binary_features

# Top N continuous variables
top_n <- 60
top_cont_df <- data.frame(
  Feature = colnames(per_data_icluster_continuous),
  PosteriorProb = beta_pp[[1]]
) |> 
  dplyr::arrange(desc(PosteriorProb)) |> 
  head(top_n)

# Top N binary variables
top_bin_df <- data.frame(
  Feature = colnames(per_data_icluster_binary),
  PosteriorProb = beta_pp[[2]]
) |> 
  dplyr::arrange(desc(PosteriorProb)) |> 
  head(top_n)

top_cont_df
top_bin_df






#############################################################    
############# MOFA2 -----
#############################################################




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
