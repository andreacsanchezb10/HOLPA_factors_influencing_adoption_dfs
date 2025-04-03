# set up
rm(list = ls())
time <- proc.time()
repetitions <- 500
nCores <- 3

# replicateMode = 1: Replicate the whole experiment of 500 iterations
#                    for each one of the three datasets that are
#                    refered in the article and produce the tables and
#                    figures of the paper.
# replicateMode = 2: [DEFAULT] Replicate the tables and figures by
#                    loading our results RData files (for saving time
#                    by not performing the foul analysis from scratch)
replicateMode <- 1

if (replicateMode == 1) {
  
  # cleaning memory
  rm(list = setdiff(ls(), c("time", "repetitions", "nCores", "replicateMode")))
  
  library("parallel")
  library("survival")
  library("Hmisc")
  library("glmnet") #for the lasso function
  library("hash")
  library("ROCR")
  library("TunePareto")
  library("foreach")
  library("MASS")
  library("VGAM")
  library("MXM")

  date()
  rm(list = setdiff(ls(), c("time", "repetitions", "nCores", "replicateMode")))
  
  # load the main pipeline of the experiments (EXP function)
  source("Experiment.R")

  # Initialize the cluster
  
  cl <- makeCluster(nCores)
  
  a <- function() {
    library("MXM")
  }
  tmp <- clusterCall(cl, a)
  
  varlist <- unique(c(ls(), ls(envir = .GlobalEnv), ls(envir = parent.env(environment()))))
  clusterExport(cl, varlist = varlist, envir = environment())
  clusterSetRNGStream(cl)
  wd <- getwd()
  clusterExport(cl, varlist = "wd", envir = environment())
  
  do.wrap <- function(iter, dataset_name, bc_dataset, bc_target) {
    if(iter <= 100) {
      set.seed(iter+10)
    }
    if(iter > 100 & iter <= 200) {
      set.seed((iter-100)*10+2)
    }
    if(iter > 200 & iter <= 300) {
      set.seed((iter-200)*8+136)
    }
    if(iter > 300 & iter <= 400) {
      set.seed(2*(iter-300)+38)
    }
    if(iter > 400 & iter <= 500) {
      set.seed(3*(iter-400)+77)
    }
    
    require("glmnet")
    require("hash")
    require("MASS")
    require("VGAM")
    require("stats")
    require("ROCR")
    require("Hmisc")
    require("TunePareto")
    require("methods")
    require("utils")
    require("MXM")
    
    res <- EXP(dataset_name, bc_dataset, bc_target, test = "testIndLogistic", task = "C")
    print(res)
    r <- vector("list", 1)
    
    r[[1]]$SES_queues <- length(res$SES_res@queues)
    r[[1]]$SES_vars <- length(unlist(res$SES_res@queues))
    r[[1]]$vs <- unique(unlist(res$SES_res@queues))
    #vl <- res$lasso_vars
    #r[[1]]$jaccard <- sum(!is.na(match(vl, vs)))/length(unique(c(vl, vs)))
    r[[1]]$SES_performance <- res$SES_performance
    r
  }

  data<- read.csv("data.csv",sep=",")
  bc_dataset <- data[, -which(names(data) == "target")] # Remove target from feature matrix
  bc_target <- as.factor(data$target)
  
  print(bc_target)
  
  test <- "testIndLogistic"
  
  set.seed(123456789)
  
  results_breast_full <- clusterApply(cl, 1:repetitions, 
                                      do.wrap, dataset_name = "dataset_name", 
                                      bc_dataset = bc_dataset, bc_target = bc_target)
  
  # save the current dataset results
  save(results_breast_full, file = "breast.rda")
  
  date()
  
  stopCluster(cl)
}


# Breast Cancer (Classification) Results

if (replicateMode == 1) {
  load("C:/Users/andreasanchez/OneDrive - CGIAR/3_chapter_PhD/HOLPA_factors_influencing_adoption_dfs/breast.rda")
} else {
  load("C:/Users/andreasanchez/OneDrive - CGIAR/3_chapter_PhD/HOLPA_factors_influencing_adoption_dfs/v80i07-replication/original_results/breast.rda")
}

sesPerf <- vector("numeric", repetitions)
sesNVar <- vector("numeric", repetitions)
sesNSig <- vector("numeric", repetitions)

stdPerf <- vector("numeric", repetitions)
meanPerf <- vector("numeric", repetitions)

results <- results_breast_full
head(results)

for (i in 1:length(results)) {
  sesPerf[i] <- results[[i]][[1]]$SES_performance[1] # take the first performance or take the mean
  sesNVar[i] <- results[[i]][[1]]$SES_queues
  sesNSig[i] <- length(results[[i]][[1]]$SES_performance)
  #jaccard[i] <- results[[i]][[1]]$jaccard
  
  stdPerf[i] <- sd(results[[i]][[1]]$SES_performance)
  meanPerf[i] <- mean(results[[i]][[1]]$SES_performance)
}

coefvar <- stdPerf/abs(meanPerf)

jss_b <- NULL
jss_b$sesPerf <- sesPerf
jss_b$sesNVar <- sesNVar
jss_b$sesNSig <- sesNSig
jss_b$meanPerf <- meanPerf
jss_b$stdPerf <- stdPerf
jss_b$coefvar <- coefvar

rm(results_breast_full)

#############################
#                           #
#  JSS paper TABLE 3        #
#                           #
#############################

# Frequency of signature multiplicity. Each cell reports how many
# times j equivalent signatures are retrieved for its corresponding
# data set (row), where j is the number reported on top of the cell"s
# column. The notation 10+ indicates 10 or more signatures.

table3 = matrix(0, 1, 20)
rownames(table3) <- c("Adoption")
colnames(table3) <- c(1:19,"+20")
for (i in 1:19) {
  table3[1, i] = sum(jss_b$sesNSig == i)
}
table3[1, 20] = sum(jss_b$sesNSig >= 20)

print("")
print("Table 3")
print(table3)
print("")
unique(sapply(results, function(x) x[[1]]$SES_queues))

# Extract $vs where $SES_queues = 14
vs_for_queues <- lapply(results, function(x) {
  if (length(x) > 0 && "SES_queues" %in% names(x[[1]])) {
    if (x[[1]]$SES_queues == 8) {
      return(x[[1]]$vs)
    }
  }
  return(NULL)
})

# Remove NULL values (if any)
vs_for_queues <- Filter(Negate(is.null), vs_for_queues)

# Show the result
print(vs_for_queues)
str(results)
###########################
#                         #
#  JSS paper TABLE 4      #
#                         #
###########################

# Quantiles of the coefficient of variation (CV) of the SES performances. Results are reported separately for each data set (rows).

q1 = quantile(jss_b$coefvar, probs = c(0.025, 0.975), na.rm = TRUE)
m1 = mean(jss_b$coefvar, na.rm=T)

table4 = matrix(0, 1, 3)
table4[1, ] = c(q1[1], m1, q1[2])*100
rownames(table4) <- c("Adoption")
colnames(table4) <- c("2.5%", "Median", "97.5%")

print("")
print("Table 4")
print(table4)
###########
# Extract selected variables
all_vars <- sort(unique(unlist(lapply(results, function(x) x[[1]]$vs))))
all_vars

# Create a binary matrix for presence/absence of variables
binary_matrix <- t(sapply(results, function(x) as.integer(all_vars %in% x[[1]]$vs)))


# Step 3: Compute selection frequency for each variable
selection_frequency <- colMeans(binary_matrix)

# Step 4: Create a data frame of variable stability
stability_df <- data.frame(
  Variable = all_vars,
  Selection_Frequency = selection_frequency
)

# Step 5: Rank by stability (higher frequency = more stable)
stability_df <- stability_df[order(-stability_df$Selection_Frequency), ]

# Step 6 (Optional): Identify consensus set (e.g., > 25% threshold)
consensus_set <- stability_df[stability_df$Selection_Frequency >= 0.25, ]

# Show results
print(stability_df)
print("Consensus Set (Selected in > 50% of repetitions):")
print(consensus_set)

z<- data[, consensus_set$Variable]
names(z)
