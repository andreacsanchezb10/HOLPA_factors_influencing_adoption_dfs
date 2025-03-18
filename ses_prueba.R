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

source("experiments.R")
if (replicateMode == 1) {
  
  # cleaning memory
  rm(list = setdiff(ls(), c("time", "repetitions", "nCores", "replicateMode")))
  
  # default repository
  defaultRepos <- "http://cran.us.r-project.org"
  
  # set library folder
  #.libPaths(new = c("myLibs", .libPaths())) 
  
  library("parallel")
  #if (!require("survival", lib.loc = "myLibs")) {
    # install.packages("survival", repos = defaultRepos, lib = "myLibs")
    library("survival")
  #}
  i#f (!require("Hmisc", lib.loc = "myLibs")) {
    #install.packages("Hmisc", repos = defaultRepos, lib = "myLibs")
    library("Hmisc")
  #}
  #if (!require("glmnet", lib.loc = "myLibs")) {
    #install.packages("glmnet", repos = defaultRepos, lib = "myLibs")
    library("glmnet") #for the lasso function
  #}
  #if (!require("hash", lib.loc = "myLibs")) {
    #install.packages("hash", repos = defaultRepos, lib = "myLibs")
    library("hash")
  #}
  #if (!require("ROCR", lib.loc = "myLibs")) {
    #install.packages("ROCR", repos = defaultRepos, lib = "myLibs")
    library("ROCR")
  #}
  # if (!require("TunePareto", lib.loc = "myLibs")) {
    #install.packages("TunePareto", repos = defaultRepos, lib = "myLibs")
    library("TunePareto")
  # }
  # if (!require("foreach", lib.loc = "myLibs")) {
    #install.packages("foreach", repos = defaultRepos, lib = "myLibs")
    library("foreach")
  #}
  # if (!require("MASS", lib.loc = "myLibs")) {
    #install.packages("MASS", repos = defaultRepos, lib = "myLibs")
    library("MASS")
  #}
  # if (!require("VGAM", lib.loc = "myLibs")) {
    #install.packages("VGAM", repos = defaultRepos, lib = "myLibs")
    library("VGAM")
  #}
  #if (!require("MXM", lib.loc = "myLibs")) {
    #install.packages("MXM", repos = defaultRepos, lib = "myLibs")
    library("MXM")
  #}
  
  # load the main pipeline of the experiments (EXP function)
  source("complementary/experiments.R")
  # load the lasso model and metric Complementary functions
  #source("complementary/lasso_functions.R")
  
  

  #################################################
  #                                               #
  #  Breast Cancer dataset (classification task)  #
  #                                               #
  #################################################
  
  date()
  rm(list = setdiff(ls(), c("time", "repetitions", "nCores", "replicateMode")))
  
  # load the main pipeline of the experiments (EXP function)
  source("experiments.R")
  # load the lasso model and metric Complementary functions
  #source("complementary/lasso_functions.R")
  
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
    
   # .libPaths(new = c("myLibs", .libPaths())) 
    
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
    
    dataset_name <- "dataset_name"
    
    res <- EXP(dataset_name, bc_dataset, bc_target, test = "testIndLogistic", task = "C")
    
    r <- vector("list", 1)
    
    r[[1]]$SES_queues <- length(res$SES_res@queues)
    r[[1]]$SES_vars <- length(unlist(res$SES_res@queues))
    vs <- unique(unlist(res$SES_res@queues))
    #vl <- res$lasso_vars
    r[[1]]$jaccard <- sum(!is.na(match(vl, vs)))/length(unique(c(vl, vs)))
    # r[[1]]$Lasso_vars <- length(vl)
    r[[1]]$SES_performance <- res$SES_performance
    #r[[1]]$lasso_performance <- res$lasso_performance
    r
  }
  
  # breast cancer dataset (classification task)
  
  load("datasets/breastCancer_complete_data.RData")
  
  test <- "testIndLogistic"
  
  set.seed(123456789)
  
  results_breast_full <- clusterApply(cl, 1:repetitions, 
                                      do.wrap, dataset_name = dataset_name, 
                                      bc_dataset = bc_dataset, bc_target = bc_target)
  
  # save the current dataset results
  save(results_breast_full, file = "results/breast.rda")
  
  date()
  
  stopCluster(cl)
}


#######################################
#                                     #
#  LOAD AND MERGE EXPERIMENT RESULTS  #
#                                     #
#######################################

if (replicateMode == 1) {
  load("C:/Users/andreasanchez/OneDrive - CGIAR/3_chapter_PhD/HOLPA_factors_influencing_adoption_dfs/v80i07-replication/results/aquatictox.rda")
} else {
  load("C:/Users/andreasanchez/OneDrive - CGIAR/3_chapter_PhD/HOLPA_factors_influencing_adoption_dfs/v80i07-replication/original_results/aquatictox.rda")
}

# Aquatictox Regression Results

sesPerf <- vector("numeric", repetitions)
#lassoPerf <- vector("numeric", repetitions)
sesNVar <- vector("numeric", repetitions)
#lassoNVar <- vector("numeric", repetitions)
sesNSig <- vector("numeric", repetitions)
jaccard <- vector("numeric", repetitions)
stdPerf <- vector("numeric", repetitions)
meanPerf <- vector("numeric", repetitions)

results <- results_aquatictox_full

for (i in 1:length(results)) {
  sesPerf[i] <- results[[i]][[1]]$SES_performance[1] # take the first performance or take the mean
  sesNVar[i] <- results[[i]][[1]]$SES_queues
  sesNSig[i] <- length(results[[i]][[1]]$SES_performance)
  jaccard[i] <- results[[i]][[1]]$jaccard
  
  # lassoPerf[i] <- results[[i]][[1]]$lasso_performance
  #lassoNVar[i] <- results[[i]][[1]]$Lasso_vars
  
  stdPerf[i] <- sd(results[[i]][[1]]$SES_performance)
  meanPerf[i] <- mean(results[[i]][[1]]$SES_performance)
}

coefvar <- stdPerf/abs(meanPerf)

jss_a <- NULL
jss_a$sesPerf <- sesPerf
#jss_a$lassoPerf <- lassoPerf
jss_a$sesNVar <- sesNVar
#jss_a$lassoNVar <- lassoNVar
jss_a$sesNSig <- sesNSig
jss_a$jaccard <- jaccard
jss_a$meanPerf <- meanPerf
jss_a$stdPerf <- stdPerf
jss_a$coefvar <- coefvar

rm(results_aquatictox_full)

# Breast Cancer (Classification) Results

if (replicateMode == 1) {
  load("C:/Users/andreasanchez/OneDrive - CGIAR/3_chapter_PhD/HOLPA_factors_influencing_adoption_dfs/v80i07-replication/results/breast.rda")
} else {
  load("C:/Users/andreasanchez/OneDrive - CGIAR/3_chapter_PhD/HOLPA_factors_influencing_adoption_dfs/v80i07-replication/original_results/breast.rda")
}

sesPerf <- vector("numeric", repetitions)
#lassoPerf <- vector("numeric", repetitions)
sesNVar <- vector("numeric", repetitions)
#lassoNVar <- vector("numeric", repetitions)
sesNSig <- vector("numeric", repetitions)
jaccard <- vector("numeric", repetitions)

stdPerf <- vector("numeric", repetitions)
meanPerf <- vector("numeric", repetitions)

results <- results_breast_full

for (i in 1:length(results)) {
  sesPerf[i] <- results[[i]][[1]]$SES_performance[1] # take the first performance or take the mean
  sesNVar[i] <- results[[i]][[1]]$SES_queues
  sesNSig[i] <- length(results[[i]][[1]]$SES_performance)
  jaccard[i] <- results[[i]][[1]]$jaccard
  
  #lassoPerf[i] <- results[[i]][[1]]$lasso_performance
  #lassoNVar[i] <- results[[i]][[1]]$Lasso_vars
  
  stdPerf[i] <- sd(results[[i]][[1]]$SES_performance)
  meanPerf[i] <- mean(results[[i]][[1]]$SES_performance)
}

coefvar <- stdPerf/abs(meanPerf)

jss_b <- NULL
jss_b$sesPerf <- sesPerf
#jss_b$lassoPerf <- lassoPerf
jss_b$sesNVar <- sesNVar
#jss_b$lassoNVar <- lassoNVar
jss_b$sesNSig <- sesNSig
jss_b$jaccard <- jaccard
jss_b$meanPerf <- meanPerf
jss_b$stdPerf <- stdPerf
jss_b$coefvar <- coefvar

rm(results_breast_full)

# Vijver dataset (survival) results

if (replicateMode == 1) {
  load("C:/Users/andreasanchez/OneDrive - CGIAR/3_chapter_PhD/HOLPA_factors_influencing_adoption_dfs/v80i07-replication/results/vijver.rda")
} else {
  load("C:/Users/andreasanchez/OneDrive - CGIAR/3_chapter_PhD/HOLPA_factors_influencing_adoption_dfs/v80i07-replication/original_results/vijver.rda")
}

sesPerf <- vector("numeric", repetitions)
#lassoPerf <- vector("numeric", repetitions)
sesNVar <- vector("numeric", repetitions)
#lassoNVar <- vector("numeric", repetitions)
sesNSig <- vector("numeric", repetitions)
jaccard <- vector("numeric", repetitions)

stdPerf <- vector("numeric", repetitions)
meanPerf <- vector("numeric", repetitions)

results <- results_vijver_full

for (i in 1:length(results)) {
  sesPerf[i] <- results[[i]][[1]]$SES_performance[1] # take the first performance or take the mean
  sesNVar[i] <- results[[i]][[1]]$SES_queues
  sesNSig[i] <- length(results[[i]][[1]]$SES_performance)
  jaccard[i] <- results[[i]][[1]]$jaccard
  
  #lassoPerf[i] <- results[[i]][[1]]$lasso_performance
  #lassoNVar[i] <- results[[i]][[1]]$Lasso_vars
  
  stdPerf[i] <- sd(results[[i]][[1]]$SES_performance)
  meanPerf[i] <- mean(results[[i]][[1]]$SES_performance)
}

coefvar <- stdPerf/abs(meanPerf)

jss_c <- NULL
jss_c$sesPerf <- sesPerf
#jss_c$lassoPerf <- lassoPerf
jss_c$sesNVar <- sesNVar
#jss_c$lassoNVar <- lassoNVar
jss_c$sesNSig <- sesNSig
jss_c$jaccard <- jaccard
jss_c$meanPerf <- meanPerf
jss_c$stdPerf <- stdPerf
jss_c$coefvar <- coefvar

rm(results_vijver_full)

# Tables and figures reported in the JSS article

# writing file
sink("codeOut.txt")

#############################
#                           #
#  JSS paper TABLE 3        #
#                           #
#############################

# Frequency of signature multiplicity. Each cell reports how many
# times j equivalent signatures are retrieved for its corresponding
# data set (row), where j is the number reported on top of the cell"s
# column. The notation 10+ indicates 10 or more signatures.

table3 = matrix(0, 3, 10)
rownames(table3) <- c("Breast Cancer", "AquaticTox", "Vijver-2002")
colnames(table3) <- c(1:9, "10+")
for (i in 1:9) {
  #table3[1, i] = sum(jss_b$sesNSig == i)
  #table3[2, i] = sum(jss_a$sesNSig == i)
  table3[3, i] = sum(jss_c$sesNSig == i)
}
table3[1, 10] = sum(jss_b$sesNSig >= 10)
table3[2, 10] = sum(jss_a$sesNSig >= 10)
table3[3, 10] = sum(jss_c$sesNSig >= 10)

print("")
print("Table 3")
print(table3)
print("")

###########################
#                         #
#  JSS paper TABLE 4      #
#                         #
###########################

# Quantiles of the coefficient of variation (CV) of the SES performances. Results are reported separately for each data set (rows).

q1 = quantile(jss_b$coefvar, probs = c(0.025, 0.975), na.rm = TRUE)
m1 = mean(jss_b$coefvar, na.rm=T)
q2 = quantile(jss_a$coefvar, probs = c(0.025, 0.975), na.rm = TRUE)
m2 = mean(jss_a$coefvar, na.rm=T)
q3 = quantile(jss_c$coefvar, probs = c(0.025, 0.975), na.rm = TRUE)
m3 = mean(jss_c$coefvar, na.rm=T)

table4 = matrix(0, 3, 3)
table4[1, ] = c(q1[1], m1, q1[2])*100
table4[2, ] = c(q2[1], m2, q2[2])*100
table4[3, ] = c(q3[1], m3, q3[2])*100
rownames(table4) <- c("Breast Cancer", "AquaticTox", "Vijver-2002")
colnames(table4) <- c("2.5%", "Median", "97.5%")

print("")
print("Table 4")
print(table4)

###########################
#                         #
#  JSS paper TABLE 5      #
#                         #
###########################

# Quantiles of the difference in performance between SES and LASSO. Positive values indicate SES outperforming LASSO

q1 = quantile(jss_b$sesPerf - jss_b$lassoPerf, probs = c(0.025, 0.975), na.rm = TRUE)
m1 = mean(jss_b$sesPerf - jss_b$lassoPerf)
q2 = quantile((jss_a$sesPerf - -(jss_a$lassoPerf)), probs = c(0.025, 0.975), na.rm = TRUE)
m2 = mean((jss_a$sesPerf - -(jss_a$lassoPerf)))
q3 = quantile(jss_c$sesPerf - jss_c$lassoPerf, probs = c(0.025, 0.975), na.rm = TRUE)
m3 = mean(jss_c$sesPerf - jss_c$lassoPerf)

table5 = matrix(0, 3, 3)
table5[1, ] = c(q1[1], m1, q1[2])
table5[2, ] = c(q2[1], m2, q2[2])
table5[3, ] = c(q3[1], m3, q3[2])
rownames(table5) <- c("Breast Cancer", "AquaticTox", "Vijver-2002")
colnames(table5) <- c("2.5%", "Mean", "97.5%")

print("")
print("Table 5")
print(table5)


###########################
#                         #
#  JSS paper TABLE 6      #
#                         #
###########################

# Distribution of the number of variables selected by SES and LASSO. For each method and data set both the average number and the standard deviation (St.D.) of selected variables is reported

table6 = matrix(0, 3, 4)
table6[1, ] = c(mean(jss_b$sesNVar), mean(jss_b$lassoNVar), sd(jss_b$sesNVar), sd(jss_b$lassoNVar))
table6[2, ] = c(mean(jss_a$sesNVar), mean(jss_a$lassoNVar), sd(jss_a$sesNVar), sd(jss_a$lassoNVar))
table6[3, ] = c(mean(jss_c$sesNVar), mean(jss_c$lassoNVar), sd(jss_c$sesNVar), sd(jss_c$lassoNVar))
rownames(table6) <- c("Breast Cancer", "AquaticTox", "Vijver-2002")
colnames(table6) <- c("average SES", "average LASSO", "StD.SES", "StD.LASSO")

print("")
print("Table 6")
print(table6)

############################################################
#                                                          #
#  FIGURE 1. Coefficient of Variation for SES performances #
#                                                          #
############################################################

# Boxplot of the SES performances" coefficient of variation across the 500 iterations for each dataset.

coefvars <- c(jss_a$coefvar, jss_b$coefvar, jss_c$coefvar)
indicator <-  c(rep("AquaticTox", length(jss_a$coefvar)), 
                rep("BreastCancer", length(jss_b$coefvar)), 
                rep("Vijver-2002", length(jss_c$coefvar)))
pdf(file = file.path("figures", "fig1.pdf"), width = 6, height = 5)
boxplot(coefvars ~ indicator, main = "Coefficient of Variation for SES performances", ylim = c(0, 0.3))
grid()
dev.off()

###############################################################
#                                                             #
#  FIGURE 2. Difference in performances between SES and LASSO #
#                                                             #
###############################################################

# Boxplot of the difference among SES and LASSO performances across the 500 iterations for each dataset.

# we eliminate outliers above or below 1
perfs_a <- jss_a$sesPerf - (-1 * jss_a$lassoPerf)
perfs_a <- perfs_a[abs(perfs_a) <= 1]
perfs_b <- jss_b$sesPerf - jss_b$lassoPerf
perfs_b <- perfs_b[abs(perfs_b) <= 1]
perfs_c <- jss_c$sesPerf - jss_c$lassoPerf
perfs_c <- perfs_c[abs(perfs_c) <= 1]
perfs <- c(perfs_a, perfs_b, perfs_c)
indicator <-  c(rep("AquaticTox", length(perfs_a)), 
                rep("BreastCancer", length(perfs_b)), 
                rep("Vijver-2002", length(perfs_c)))
pdf(file = file.path("figures", "fig2.pdf"), width = 6, height = 5)
boxplot(perfs ~ indicator, main = "Difference in performances between SES and Lasso", ylim = c(-0.5, 0.3))
grid()
dev.off()

#################################################################################################################

# MXM package
.libPaths(new = c("myLibs", .libPaths())) 
if (!require("MXM", lib.loc = "myLibs")) {
  install.packages("MXM", lib = "myLibs")
  library("MXM", lib.loc = "myLibs")
}

# code for  section 4.2
print("")
print("")
print("Code for section 4.2")
print("")

set.seed(12345678)
library("hash")
dataset <- matrix(runif(1000 * 300, 1, 100), nrow = 1000, ncol = 300)
target <- 3 * dataset[, 10] + 2 * dataset[, 200] + 3 * dataset[, 20] + runif(1000, 0, 10)
dataset[, 15] <- dataset[, 10]
dataset[, 250] <- dataset[, 200]
dataset[, 230] <- dataset[, 200]
system.time(sesObject <- SES(target, dataset, max_k = 5, 
                             threshold = 0.2, test = "testIndFisher", hash = TRUE, 
                             hashObject = NULL))
summary(sesObject)

hashObj <- sesObject@hashObject
sesObject2 <- SES(target, dataset, max_k = 2, threshold = 0.01, 
                  test = "testIndFisher", hash = TRUE, hashObject = hashObj)
summary(sesObject2)

# Code for section 4.3
print("")
print("")
print("Code for section 4.3")
print("")

cv.ses.object <- cv.ses(target, dataset, kfolds = 10, task = "R")

print(cv.ses.object$best_performance)
print(cv.ses.object$best_configuration)

#################################################################################################################

# End of code.R
sink()
