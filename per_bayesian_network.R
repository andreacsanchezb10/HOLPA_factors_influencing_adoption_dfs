library(dplyr)
library(readxl)

factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")


per_data_clean<- read.csv("per_data_clean.csv",sep=",")
per_summary_categorical<-read.csv("per_summary_categorical.csv",sep=",")
per_summary_numerical<-read.csv("per_summary_numerical.csv",sep=",")




#########################################################################################################################################################################################
############# DATA TYPE CONVERSION
# data standardization I need to implement this first-----
#########################################################################################################################################################################################
#### Convert categorical nominal to factor
columns_categorical_nominal <- intersect(per_summary_categorical$column_name_new2[per_summary_categorical$categorical_type %in%c("nominal")], colnames(per_data_clean))
print(columns_categorical_nominal)  # Check if it holds expected values

per_data_clean<- per_data_clean%>%
  mutate(across(all_of(columns_categorical_nominal), as.factor))

#### Convert categorical ordinal and binary variables to numeric
columns_categorical_ordinal <- intersect(per_summary_categorical$column_name_new2[per_summary_categorical$categorical_type %in%c("ordinal",NA)], colnames(per_data_clean))
print(columns_categorical_ordinal)  # Check if it holds expected values

per_data_clean<- per_data_clean%>%
  mutate(across(all_of(columns_categorical_ordinal), as.numeric))

#### Convert continuous variables to numeric
columns_continuous <- intersect(per_summary_numerical$column_name_new, colnames(per_data_clean))
print(columns_continuous)  # Check if it holds expected values

per_data_clean<- per_data_clean%>%
  mutate(across(all_of(columns_continuous), as.numeric))

##########################################################################
############# SELECT VARIABLE FOR ANALYSIS -----
##########################################################################

per_variables_list<-c(unique(per_summary_categorical$column_name_new2),unique(per_summary_numerical$column_name_new))
per_variables_list

per_data_analysis<- per_data_clean%>%
  select(all_of(per_variables_list))


## CHECK missing data
na_columns <- colnames(per_data_analysis)[colSums(is.na(per_data_analysis)) > 0]
print(na_columns)

## CHECK FOR NOW JUST REMOVE THE COLUMNS WITH NA

per_data_analysis<- per_data_analysis%>%
  select(!all_of(na_columns))


## CHECK remove the outcomes different than dfs_adoption_binary,
excluded_outcomes<- c ( "dfs_agroforestry_area" ,"dfs_agroforestry_area" ,
                        "dfs_cover_crops_area", "dfs_homegarden_area" ,   
                        "dfs_total_area",                                 
                        "dfs_crop_rotation_area" ,
 "dfs_intercropping_area"    ,                      "dfs_fallow_area",                                
 "dfs_strip_vegetation_area",                       "dfs_hedgerows_area",
"dfs_agroforestry_adoption"  ,                    
 "dfs_cover_crops_adoption" ,                       "dfs_crop_rotation_adoption" ,                    
"dfs_fallow_adoption"   ,                          "dfs_hedgerows_adoption" ,                        
 "dfs_homegarden_adoption",                         "dfs_intercropping_adoption",                     
 "dfs_strip_vegetation_adoption",
"education_level","ethnicity","marital_status",
"rainfall_timing_change_perception.nochange",     
 "rainfall_timing_change_perception.notsure" ,     
 "rainfall_timing_change_perception.startearlier",
 "rainfall_timing_change_perception.startlater" ,  
 "rainfall_timing_change_perception.stopearlier" , 
 "rainfall_timing_change_perception.stoplater" ,   
 "rainfall_timing_change_perception.unpredictable",
"temperature_change_perception")

per_data_analysis<- per_data_analysis%>%
  select(!all_of(excluded_outcomes))

summary(per_data_analysis)
names(per_data_analysis)

str(per_data_analysis)

apply(per_data_analysis, 2, var)  # Compute variance of each variable

#https://github.com/drkowal/BayesSubsets?utm_source=chatgpt.com



### 
par(mfrow = c(1, 1))



##### ONLY CONTINUOUS VARIABLES ALLOWED

#### Step 1. Structure learning
# structure learning algorithms: https://search.r-project.org/CRAN/refmans/bnlearn/html/structure.learning.html

### conditional independence test: https://search.r-project.org/CRAN/refmans/bnlearn/html/conditional.independence.tests.html
## mutual information: an information-theoretic distance measure. Again it is proportional to the log-likelihood ratio (they differ by a 
#2n2n factor). The asymptotic χ2χ 2test (mi-g), the Monte Carlo permutation test (mc-mi-g) and the sequential 
#Monte Carlo permutation test (smc-mi-g) are implemented.


tiers = list("INT", names(isachs)[1:11])

#### Constraint-Based Learning Algorithms

### PC (pc.stable), a modern implementation of the first practical constraint-based structure learning algorithm.
pc.mig<- pc.stable(per_data_analysis, alpha = 0.01, test="mi-g")
pc.mig
plot(pc.mig, main = "pc.mi-g",highlight= "dfs_adoption_binary")

pc.mcmig<- pc.stable(per_data_analysis, alpha = 0.01, test="mc-mi-g")
pc.mcmig
plot(pc.mcmig, main = "pc.mcmig",highlight= "dfs_adoption_binary")

all.equal(pc.mig, pc.mcmig)

### Grow-Shrink (gs): based on the Grow-Shrink Markov Blanket, the first (and simplest) Markov blanket
#detection algorithm used in a structure learning algorithm.
gs.mig<- gs(per_data_analysis, alpha = 0.01, test="mi-g")
gs.mig
plot(gs.mig, main = "gs.mi-g",highlight= "dfs_adoption_binary")
bnlearn::score(gs.mig,data=per_data_analysis,type= "bic-g")


gs.mcmig<- gs(per_data_analysis, alpha = 0.01, test="mc-mi-g")
gs.mcmig
plot(gs.mcmig, main = "gs.mcmig",highlight= "dfs_adoption_binary")

all.equal(gs.mig, gs.mcmig)
all.equal(pc.mig, gs.mig)
all.equal(pc.mcmig, gs.mcmig)

### GIncremental Association (iamb): based on the Markov blanket detection algorithm of the same name,
#which is based on a two-phase selection scheme (a forward selection followed by an attempt to remove false positives)..
iamb.mig<- iamb(per_data_analysis, alpha = 0.01, test="mi-g")
iamb.mig
plot(iamb.mig, main = "iamb.mi-g",highlight= "dfs_adoption_binary")

iamb.mcmig<- iamb(per_data_analysis, alpha = 0.01, test="mc-mi-g")
iamb.mcmig
plot(iamb.mcmig, main = "iamb.mcmig",highlight= "dfs_adoption_binary")

all.equal(iamb.mig, iamb.mcmig)


#### Score-based Learning Algorithms
# network scores: https://search.r-project.org/CRAN/refmans/bnlearn/html/network.scores.html

#the corresponding Bayesian Information Criterion (BIC) score (bic-g). default
hc.bicg<- hc(per_data_analysis, alpha = 0.01, score = "bic-g")
hc.bicg
plot(hc.bicg, main = "hc.bicg",highlight= "dfs_adoption_binary")

#the corresponding Akaike Information Criterion (AIC) score (aic-g).
hc.aic<- hc(per_data_analysis, alpha = 0.01, score = "aic-g")
hc.aic
plot(hc.aic, main = "hc.aic",highlight= "dfs_adoption_binary")

# the multivariate Gaussian log-likelihood (loglik-g) score.
hc.loglikg<- hc(per_data_analysis, alpha = 0.01, score = "loglik-g")
hc.loglikg
plot(hc.loglikg, main = "hc.loglikg",highlight= "dfs_adoption_binary")
bnlearn::score(hc.loglikg,data=per_data_analysis,type= "loglik-g")


# Compare scores
#BIC
bnlearn::score(gs.mig,data=per_data_analysis,type= "bic-g")
bnlearn::score(pc.mcmig,data=per_data_analysis,type= "bic-g")

bnlearn::score(hc.bicg,data=per_data_analysis,type= "bic-g")
bnlearn::score(hc.aic,data=per_data_analysis,type= "bic-g")
bnlearn::score(hc.loglikg,data=per_data_analysis,type= "bic-g")


net = network(per_data_analysis) 
net
prior = jointprior(net, N = 5)


x<-bn.boot(per_data_analysis, algorithm = "hc", 
           R = 10, statistic = narcs)

summary(unlist(x))
quantile(unlist(x), c(0.05, 0.95))

boot_res <- boot.strength(per_data_analysis, R = 10, algorithm = "hc")
boot_res
avg_bn <- averaged.network(boot_res, threshold = 0.5)
graphviz.plot(avg_bn)

x<-bn.boot(per_data_analysis, algorithm = "hc", 
        R = 10, statistic = narcs)

summary(unlist(x))



#Structure learning algorithms

## Max-Min Hill-Climbing (mmhc):
per.mmhc<- mmhc(per_data_analysis)
per.mmhc

plot(per.mmhc, main = "Max-Min Hill-Climbing (mmhc) per.mmhc")



## Restricted Maximization (rsmax2):
per.rsmax2<- rsmax2(per_data_analysis,maximize = "hc",restrict = "si.hiton.pc")
per.rsmax2

plot(per.rsmax2, main = "Restricted Maximization (rsmax2) per.rsmax2")

## Hybrid HPC (h2pc):
per.h2pc<- h2pc(per_data_analysis)
per.h2pc

plot(per.h2pc, main = "Hybrid HPC (h2pc) per.h2pc")



# Grow-Shrink algorithm
per.gs <- gs(per_data_analysis)
per.gs

## the Markov blanket of Adoption binary

