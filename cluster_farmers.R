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






