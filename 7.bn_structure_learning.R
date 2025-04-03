library(readxl)
library(dplyr)

#############################################################    
########## UPLOAD DATA #####-----
#############################################################
per_factors_adoptionBinary1<- read.csv("per_factors_adoptionBinary1.csv",sep=",")
length(unique(per_factors_adoptionBinary1$x))#39 factors
per_factors_adoptionBinary1

factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")

per_data0<- read.csv("per_data0.csv",sep=",") #all correlated factors
per_data1<- read.csv("per_data1.csv",sep=",") #data1
per_data2<- read.csv("per_data2.csv",sep=",") #data2

#############################################################    
########## SELECTED FACTORS #####-----
#############################################################
per_variables_list<-c(unique(per_factors_adoptionBinary1$x))
per_variables_list

per_analysis_adoptionBinary1<- per_adoptionBinary1%>%
  dplyr::select(all_of(per_variables_list),"dfs_adoption_binary")
names(per_analysis_adoptionBinary1) #39 selected factors

#############################################################    
########## DATA TYPE CONVERSION #####-----
#############################################################
###### --- CATEGORICAL AND BINARY VARIABLES -----
#### Convert categorical and binary to factor
str(per_analysis_adoptionBinary1)
per_analysis_adoptionBinary1<-per_analysis_adoptionBinary1%>%
  mutate(across(where(is.integer), as.factor))
str(per_analysis_adoptionBinary1)


#############################################################    
########## STRUCTURE LEARNING #####-----
#############################################################
# Algorithms to try
algorithms <- list(
  list(name = "pc.stable", args = list(test = "mi-cg", alpha = 0.5)),
  list(name = "pc.stable", args = list(test = "mi-cg", alpha = 0.05)),
  list(name = "pc.stable", args = list(test = "mi-cg", alpha = 0.01)),
  list(name = "gs", args = list(test = "mi-cg", alpha = 0.5)),
  list(name = "gs", args = list(test = "mi-cg", alpha = 0.05)),
  list(name = "gs", args = list(test = "mi-cg", alpha = 0.01)),
  list(name = "hc", args = list(score = "bic")),
  list(name = "tabu", args = list(score = "bic"))
)

# Learn structures and store arcs
structures <- list()
for (i in seq_along(algorithms)) {
  algo <- algorithms[[i]]
  cat("Running:", algo$name, "\n")
  net <- do.call(algo$name, c(list(x = data_bn), algo$args))
  structures[[i]] <- arcs(net)
}




#############################################################    
########## DATA DISCRETIZATION #####-----
#############################################################
library(cluster)

###### --- Discretize using K-means ----
# Constantinou et al 2023 -> Open problems in causal structure learning: A case study of COVID-19 in the UK
discretize_kmeans <- function(data, cluster_range = 2:5) {
  
  # Step 1: Identify continuous columns
  continuous_vars <- names(data)[sapply(data, is.numeric)]
  
  # Step 2: Create a vector to store optimal k values
  optimal_k <- c()
  
  set.seed(123) # For reproducibility
  
  for (col in continuous_vars) {
    cat("\nProcessing:", col, "\n")
    
    # Skip if fewer than 2 unique values (since k-means needs at least 2 clusters)
    unique_count <- length(unique(data[[col]]))
    
    if (unique_count < 2) {
      cat(col, "skipped (too few unique values)\n")
      next
    }
    
    # Adjust cluster range to avoid exceeding unique values
    k_range <- cluster_range[cluster_range <= unique_count]
    
    if (length(k_range) < 2) {
      cat(col, "skipped (not enough unique values for clustering)\n")
      next
    }
    
    # Step 3: Compute silhouette scores for each k
    silhouette_scores <- numeric(max(k_range))
    
    for (k in k_range) {
      km <- kmeans(data[[col]], centers = k)
      ss <- silhouette(km$cluster, dist(data[[col]]))
      silhouette_scores[k] <- mean(ss[, 3])
    }
    
    # Step 4: Select k with highest silhouette score
    best_k <- which.max(silhouette_scores[k_range])
    
    if (length(best_k) == 0 || is.na(best_k)) {
      cat(col, "skipped (no valid silhouette score)\n")
      next
    }
    
    best_k <- k_range[best_k] # Map back to the original k_range value
    optimal_k[col] <- best_k
    
    cat(col, "Optimal k (Silhouette Method):", best_k, "\n")
    
    # Step 5: Apply K-means using the best k
    clusters <- kmeans(data[[col]], centers = best_k)$cluster
    
    # Step 6: Create new factor column with discretized values
    data[[paste0(col, "_discretized")]] <- factor(clusters)
  }
  
  # Step 7: Return the modified dataset and optimal k values
  list(data = data, optimal_k = optimal_k)
}
per_adoptionBinary1_discretize_kmeans <- discretize_kmeans(per_analysis_adoptionBinary1)
per_adoptionBinary1_discretize_kmeans$optimal_k
per_adoptionBinary1_kmeans <- per_adoptionBinary1_discretize_kmeans$data
str(per_adoptionBinary1_kmeans)
per_adoptionBinary1_kmeans <- per_adoptionBinary1_kmeans%>%
  select(-where(is.numeric))
str(per_adoptionBinary1_kmeans)


### Loading and exploring the data -----
install.packages()
library(Rgraphviz)
library(bnlearn)

set.seed(123)  # For reproducibility

hc<- hc(per_adoptionBinary1_kmeans)
hc
bootstr = boot.strength(per_adoptionBinary1_kmeans, R = 100, algorithm = "tabu")
bootstr
plot(bootstr)
bootstr[with(bootstr, strength >= 0.85 & direction >= 0.5), ]
avgnet = averaged.network(bootstr, threshold = 0.5)
avgnet

?strength.plot


g <- strength.plot(avgnet, bootstr, threshold = 0.55, shape = "rectangle", render = FALSE)

# Now adjust the label size using Rgraphviz functions:
nodeRenderInfo(g) <- list(fontsize = 50)  # Adjust label size

# Render the graph with updated label size
renderGraph(g)


# Define the dataset and target variable.
target_binary <- "dfs_adoption_binary"
factors <- setdiff(names(per_analysis_adoptionBinary1), target_binary)  # All features except target
n_factors <- length(factors)

# Limit the max number of factors removed at once to avoid combinatorial explosion
max_remove <- 5  # Change this as needed

# Generate all possible subsets by removing up to `max_remove` factors
library(combinat)
subsets_list <- list()

# Start with all features
subsets_list[["All_39"]] <- factors

# Generate subsets with removals
for (remove_n in 1:max_remove) {
  combinations <- combn(factors, remove_n, simplify = FALSE)
  for (combo in combinations) {
    subset_name <- paste("Remove", remove_n, paste(combo, collapse = "_"), sep = "_")
    subsets_list[[subset_name]] <- setdiff(factors, combo)
  }
}

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
algorithms <- c("pc.stable", "gs", "iamb", "fast.iamb", "inter.iamb", "iamb.fdr", "hc")
n_rep <- 2   # number of CV repetitions
k <- 4       # 10-fold CV

# Initialize storage for results
selected_features <- list()

# ----- REPEATED 10-FOLD CROSS-VALIDATION -----
for (subset_name in names(subsets_list)) {
  cat("Evaluating subset:", subset_name, "\n")
  subset_factors <- subsets_list[[subset_name]]
  
  for (alg in algorithms) {
    for (alpha in alphas) {
      for (test in tests) {
        key <- paste(subset_name, alg, "alpha", alpha, "test", test, sep = "_")
        selected_features[[key]] <- list()
      }
    }
  }
  
  rep_index <- 1  # Track repetitions across folds
  for (r in 1:n_rep) {
    folds <- createFolds(per_data_analysis.binary[[target_var]], k = k, returnTrain = TRUE)
    
    for (fold_index in 1:k) {
      train_idx <- folds[[fold_index]]
      train_fold <- per_data_analysis.binary[train_idx, c(subset_factors, target_var)]
      
      for (alg in algorithms) {
        for (alpha in alphas) {
          for (test in tests) {
            key <- paste(subset_name, alg, "alpha", alpha, "test", test, sep = "_")
            
            # Learn the Bayesian network
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
            } else if (alg == "hc") {
              bn_model <- hc(train_fold)
            }
            
            # Extract selected features
            feats <- get_selected_features(bn_model, target = target_var)
            selected_features[[key]][[rep_index]] <- feats
          }
        }
      }
      rep_index <- rep_index + 1
    }
  }
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
      Subset = subset_name,
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


# ----- STABILITY ASSESSMENT -----
jaccard <- function(set1, set2) {
  length(intersect(set1, set2)) / length(union(set1, set2))
}

stability_scores <- list()
for (key in names(selected_features)) {
  feats_list <- selected_features[[key]]
  sims <- c()
  num_folds <- length(feats_list)
  
  for (i in 1:(num_folds - 1)) {
    for (j in (i + 1):num_folds) {
      sims <- c(sims, jaccard(feats_list[[i]], feats_list[[j]]))
    }
  }
  stability_scores[[key]] <- mean(sims)
}

cat("\nStability Scores:\n")
for (key in names(stability_scores)) {
  cat(key, ":", stability_scores[[key]], "\n")
}

# ----- IDENTIFY STABLE FEATURES -----
stable_features <- list()
threshold <- 0.5
for (key in names(selected_features)) {
  feats_list <- selected_features[[key]]
  n_folds <- length(feats_list)
  
  feature_freq <- table(unlist(feats_list))
  min_occurrences <- ceiling(threshold * n_folds)
  stable_features[[key]] <- names(feature_freq)[feature_freq >= min_occurrences]
}

cat("\nStable Features:\n")
for (key in names(stable_features)) {
  cat(key, ":", paste(stable_features[[key]], collapse = ", "), "\n")
}

# Convert stable features to a data frame
stable_df <- do.call(rbind, lapply(names(stable_features), function(key) {
  data.frame(Combination = key,
             Selected_Factors = paste(stable_features[[key]], collapse = ", "),
             stringsAsFactors = FALSE)
}))

print(stable_df)
################


library(bnlearn)

F2SL_c_G2 <- function(Data, alpha = 0.05, maxK = 3) {
  p <- ncol(Data)
  ns <- apply(Data, 2, max)
  
  skeleton <- matrix(0, p, p)  # Empty adjacency matrix
  
  for (i in 1:p) {
    PC <- list()
    
    for (j in 1:p) {
      if (i != j) {
        test <- ci.test(Data[, i], Data[, j], data = Data)
        
        if (test$p.value <= alpha) {
          skeleton[i, j] <- 1
        }
      }
    }
  }
  
  # Ensure symmetry
  skeleton <- (skeleton + t(skeleton)) / 2
  skeleton[skeleton != 1] <- 0
  
  return(skeleton)
}
