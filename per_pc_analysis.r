library(pcalg)
library(reshape2)
library(tidygraph)
library(ggraph)
library(ggplot2)
factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")


#########################################################################################################################################################################################
############# DATA TYPE CONVERSION -----
#########################################################################################################################################################################################
#### Convert categorical and binary variables to numeric to perform pc algorithm

columns_factor <- intersect(factors_list$column_name_new[factors_list$metric_type %in%c("categorical","binary")], colnames(per_data_clean))
print(columns_factor)  # Check if it holds expected values

per_data_analysis<- per_data_clean%>%
  mutate(across(all_of(columns_factor), as.numeric))


##########################################################################
############# SELECT ONLY NUMERICAL VARIABLES -----
##########################################################################

per_variables <- intersect(factors_list$column_name_new, colnames(per_data_analysis))
print(per_variables)  

per_data_analysis<- per_data_analysis%>%
  select(all_of(per_variables))%>%
  select(where(is.numeric))
  select(gender,age, n_people,year_birth,n_visits_extensionist,dfs_total_area)
  str(per_data_analysis)  
  
head(per_data_analysis)
str(per_data_analysis)  # Shows the structure of your dataset
summary(per_data_analysis)  # Provides a summary of each variable
head(per_data_analysis)  # Displays the first few rows
  
zero_sd_vars <- names(which(apply(per_data_analysis, 2, sd) == 0))
zero_sd_vars

per_data_analysis<- per_data_analysis%>%
  select(!all_of(zero_sd_vars))
names(per_data_analysis)

#######################################################
############# BACKGROUND KNOWLEDGE ############################
#######################################################----
variables<-unique(factors_list$column_name_new)

bk <- matrix(TRUE, length(variables), length(variables), dimnames = list(variables, variables))
bk

##### RULE: Prohibition of paths ----
bk["age", setdiff(variables, "year_birth")] <- FALSE  # Prohibition of paths leading to “year_birth”, unless "age"->"year_birth
bk["year_birth", setdiff(variables, "age")] <- FALSE  # Prohibition of paths leading to “age”, unless "year_birth"->"age
bk[setdiff(variables, c("age","year_birth")),"years_in_community"] <- FALSE  # Prohibition of paths leading to "years_in_community" unless "age"->"years_in_community" or "year_birth"->"years_in_community" 

bk["n_adults_wa", c("n_adults_wa_male", "n_adults_wa_female")] <- FALSE # Prohibition of this path n_adults_wa -> "n_adults_wa_male", "n_adults_wa_female"
bk["n_adults_old", c("n_adults_old_male", "n_adults_old_female")] <- FALSE # Prohibition of this path n_adults_old -> "n_adults_old_male", "n_adults_old_female"
print(bk)
t.bk <-t(bk)
bk.list <- melt(t.bk)
bk.list
colnames(bk.list) <- c("from", "to", "weight")  # Rename columns

bk.list <- bk.list[bk.list$weight == TRUE, ]  # Keep only edges that can be TRUE
bk.list

############
#The dataset per_data_analysis contains n = 200 observations of p = XX continuous variables 
#with a multivariate Gaussian distribution.

#######################################################
############# PC ALGORITHM ############################
#######################################################----
#All information that is needed in the conditional independence test can be passed in the argument suffStat
per.suffStat <- list(C = cor(per_data_analysis), n = nrow(per_data_analysis))
per.suffStat

per.varNames <- colnames(per_data_analysis)
per.varNames

#### STEP 1. 	Estimation of the skeleton of the DAG. 
#The skeleton of a DAG is the undirected graph that has the same edges as the DAG but no edge orientations.
#The main task of function skeleton() in ﬁnding the skeleton is to compute and test several conditional independencies. 

#If used in the PC algorithm, it estimates the order-independent “PC-stable” (method="stable")

per.skeleton <- skeleton(per.suffStat, indepTest = gaussCItest,labels = per.varNames, alpha = 0.01,method = "stable")
per.skeleton

#### STEP 2. The PC-algorithm is implemented in function pc()
#The PC algorithm is known to be order-dependent, in the sense that the computed skeleton depends on the order in which the variables are given. 
#skel.method ="stable" (default) provides an order-independent skeleton
per.pc <- pc(per.suffStat, indepTest = gaussCItest,labels = per.varNames, alpha = 0.05,skel.method ="stable")
per.pc

par(mfrow = c(1,2))
plot(per.skeleton, main = "Skeleton of DAG")
plot(per.pc, main = "PC Algorithm DAG")

#### Plot results using ggplot2


# Extract adjacency matrix
per.pc.amat <- as(per.pc, "amat")
per.pc.matrix <- as(per.pc.amat, "matrix")  # Convert to standard matrix
per.pc.matrix
per.pc.t.matrix<-t(per.pc.matrix)

# Convert adjacency matrix to data frame for visualization
per.pc.edge_list <- melt(per.pc.t.matrix)
per.pc.edge_list
colnames(per.pc.edge_list) <- c("from", "to", "weight")  # Rename columns
per.pc.edge_list
per.pc.edge_list <- per.pc.edge_list[per.pc.edge_list$weight == 1, ]  # Keep only edges that exist
per.pc.edge_list

# Create a node data frame with proper variable names
per.pc.node.names <- unique(c(per.pc.edge_list$from, per.pc.edge_list$to))
per.pc.nodes <- data.frame(name = per.pc.node.names, stringsAsFactors = FALSE)

# Convert edge list to a graph object
per.pc.graph <- tbl_graph(nodes = per.pc.nodes, edges = per.pc.edge_list, directed = TRUE)
per.pc.graph

per.pc.graph <- per.pc.graph %>% 
  activate(nodes) %>% 
  mutate(name = as.character(name))  # Ensure names remain characters
per.pc.graph

ggraph(per.pc.graph, layout = "fr") +  
  geom_edge_link(arrow = arrow(length = unit(4, "mm")), 
                 end_cap = circle(4, "mm"),  
                 edge_width = 1) +  
  geom_node_point(size = 6, color = "darkblue") +  
  geom_node_text(aes(label = name), repel = TRUE, size = 5) +  
  theme_void() +  
  ggtitle("PC Algorithm")


#### Add background knowledge ###
per.pc.edge_list

per.pc.edge_list.bk <- per.pc.edge_list %>%
  semi_join(bk.list, by = c("from", "to"))


# Create a node data frame with proper variable names
per.pc.node.names.bk <- unique(c(per.pc.edge_list.bk$from, per.pc.edge_list.bk$to))
per.pc.nodes.bk <- data.frame(name = per.pc.node.names.bk, stringsAsFactors = FALSE)

# Convert edge list to a graph object
per.pc.graph.bk <- tbl_graph(nodes = per.pc.nodes.bk, edges = per.pc.edge_list.bk, directed = TRUE)
per.pc.graph.bk

per.pc.graph.bk <- per.pc.graph.bk %>% 
  activate(nodes) %>% 
  mutate(name = as.character(name))  # Ensure names remain characters
per.pc.graph.bk

ggraph(per.pc.graph.bk, layout = "fr") +  
  geom_edge_link(arrow = arrow(length = unit(4, "mm")), 
                 end_cap = circle(4, "mm"),  
                 edge_width = 1) +  
  geom_node_point(size = 6, color = "darkblue") +  
  geom_node_text(aes(label = name), repel = TRUE, size = 5) +  
  theme_void() +  
  ggtitle("PC Algorithm + Background knowledge")

















#######################
#If you want to forbid only one direction, use addBgKnowledge() (asymmetric).
# Extract adjacency matrix
amat.per <- as(pc.per, "amat")
amat.per <- as(amat.per, "matrix")  # Convert to standard matrix
amat.per


amat.per.bk <- addBgKnowledge(amat.per, x = "years_in_community", y = "n_visits_consumers")

amat.per.bk<-t( amat.per.bk )
amat.per.bk

par(mfrow = c(1,2))
plot(as(t(amat.per), "graphNEL")) #estimated CPDAG
plot(as(t( amat.per.bk ), "graphNEL")); box(col="gray")


# Convert adjacency matrix to data frame for visualization
edge_list.bk <- melt(amat.per.bk)
edge_list.bk
edge_list.bk <- edge_list.bk[edge_list.bk$value == 1, ]  # Keep only edges that exist
edge_list.bk
colnames(edge_list.bk) <- c("from", "to", "weight")  # Rename columns
edge_list.bk

# Create a node data frame with proper variable names
nodes.bk <- data.frame(name = node_names, stringsAsFactors = FALSE)

# Ensure `edge_list.bk` uses factor levels matching `nodes.bk`
edge_list.bk$from <- factor(edge_list.bk$from, levels = nodes.bk$name)
edge_list.bk$to <- factor(edge_list.bk$to, levels = nodes.bk$name)



# Convert edge list to a graph object
graph.bk <- tbl_graph(nodes = nodes.bk, edges = edge_list, directed = TRUE)
graph.bk

graph.bk <- graph.bk %>% 
  activate(nodes) %>% 
  mutate(name = as.character(name))  # Ensure names remain characters
graph.bk

ggraph(graph.bk, layout = "fr") +  
  geom_edge_link(arrow = arrow(length = unit(4, "mm")), 
                 end_cap = circle(4, "mm"),  
                 edge_width = 1) +  
  geom_node_point(size = 6, color = "darkblue") +  
  geom_node_text(aes(label = name), repel = TRUE, size = 5) +  
  theme_void() +  
  ggtitle("PC Algorithm Causal Graph with Variable Names")











############## If you want to remove an edge entirely, use fixedGaps (symmetric).
fixedGaps <- matrix(0, nrow = length(varNames.per), ncol = length(varNames.per))
rownames(fixedGaps) <- colnames(fixedGaps) <- varNames.per  # Ensure names match
fixedGaps
# Set all edges pointing *to* "age" as forbidden
for (var in varNames.per) {
  if (var != "age") {
    fixedGaps[var, "age"] <- 1  # Forbid causal effects to age
    fixedGaps["age", var] <- 0  # Allow causal effects *from* age
  }
}

# Ensure symmetry
fixedGaps <- fixedGaps | t(fixedGaps)
fixedGaps
# Run PC algorithm with constraints
pc.gmG8.constrained <- pc(suffStat.per, indepTest = gaussCItest, labels = varNames.per, 
                          alpha = 0.01, skel.method = "stable", fixedGaps = fixedGaps)

# Plot the constrained CPDAG
plot(pc.gmG8.constrained, main = "PC Algorithm with No Causal Pathways to 'Gender'")



library(igraph)
## Edge list
showEdgeList(pc.per)

## Adjacency matrix
showAmat(pc.per)

## Plot using package igraph; show estimated CPDAG:
iplotPC(pc.per)

##################





##### CHECK WHY THERE ARE NO ARROWS IN THE PLOT ############
###If your PC model (CPDAG) has no arrows (only an undirected graph or even a disconnected graph),
###it is likely due to one of the following reasons:
## 1. If α (alpha) is too small, fewer edges remain in the skeleton, and the graph may be disconnected or have only undirected edges.

## 2. Too Few Samples (n Too Small)
suffStat.per$n

## 3.Variables Are Highly Correlated (C is Too Dense or Sparse)
#Many zeros or near-zero values → Too little correlation → Increase alpha.
#Very high correlations (~1.0 everywhere) → PC may struggle to orient the graph.
cor_mat <- suffStat.per$C
print(round(cor_mat, 2))

amat.per <- as(pc.per, "amat")
print(amat.per)



