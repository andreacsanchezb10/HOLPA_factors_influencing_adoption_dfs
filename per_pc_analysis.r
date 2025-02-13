head(per_data_clean)


per_variables <- intersect(factors_list$column_name_new, colnames(per_data_clean))
print(per_variables)  

per_data_analysis<- per_data_clean%>%
  select(all_of(per_variables))%>%
  select(where(is.numeric))
  select(age, n_people,year_birth,n_visits_extensionist,hired_labour_permanent_adults_female,dfs_total_area)
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



############
#The dataset per_data_analysis contains n = 200 observations of p = 3 continuous variables 
#with a multivariate Gaussian distribution.

#######################################################
############# PC ALGORITHM ############################
#######################################################----
#All information that is needed in the conditional independence test can be passed in the argument suffStat
suffStat.per <- list(C = cor(per_data_analysis), n = nrow(per_data_analysis))
suffStat.per

varNames.per <- colnames(per_data_analysis)
varNames.per

#### STEP 1. 	Estimation of the skeleton of the DAG. 
#The skeleton of a DAG is the undirected graph that has the same edges as the DAG but no edge orientations.
#The main task of function skeleton() in ﬁnding the skeleton is to compute and test several conditional independencies. 

#If used in the PC algorithm, it estimates the order-independent “PC-stable” (method="stable")

skeleton.per <- skeleton(suffStat.per, indepTest = gaussCItest,labels = varNames.per, alpha = 0.01,method = "stable")
skeleton.per

#### STEP 2. The PC-algorithm is implemented in function pc()
#The PC algorithm is known to be order-dependent, in the sense that the computed skeleton depends on the order in which the variables are given. 
#skel.method ="stable" (default) provides an order-independent skeleton
pc.per <- pc(suffStat.per, indepTest = gaussCItest,labels = varNames.per, alpha = 0.05,skel.method ="stable")
pc.per

par(mfrow = c(1,2))
plot(skeleton.per, main = "Skeleton of DAG")
plot(pc.per, main = "PC Algorithm DAG")


# Extract adjacency matrix
amat.per <- as(pc.per, "amat")
amat.per <- as(amat.per, "matrix")  # Convert to standard matrix
amat.per

library(tidygraph)
library(ggraph)
library(ggplot2)
library(reshape2)

# Convert adjacency matrix to data frame for visualization
edge_list <- melt(amat.per)
edge_list
edge_list <- edge_list[edge_list$value == 1, ]  # Keep only edges that exist
edge_list
colnames(edge_list) <- c("from", "to", "weight")  # Rename columns
edge_list

# Create a node data frame with proper variable names
nodes <- data.frame(name = node_names, stringsAsFactors = FALSE)

# Ensure `edge_list` uses factor levels matching `nodes`
edge_list$from <- factor(edge_list$from, levels = nodes$name)
edge_list$to <- factor(edge_list$to, levels = nodes$name)

# Convert factor levels to indices explicitly
edge_list$from <- as.integer(edge_list$from)
edge_list$to <- as.integer(edge_list$to)
edge_list

# Convert edge list to a graph object
graph <- tbl_graph(nodes = nodes, edges = edge_list, directed = TRUE)
graph

graph <- graph %>% 
  activate(nodes) %>% 
  mutate(name = as.character(name))  # Ensure names remain characters
graph

ggraph(graph, layout = "fr") +  
  geom_edge_link(arrow = arrow(length = unit(4, "mm")), 
                 end_cap = circle(4, "mm"),  
                 edge_width = 1) +  
  geom_node_point(size = 6, color = "darkblue") +  
  geom_node_text(aes(label = name), repel = TRUE, size = 5) +  
  theme_void() +  
  ggtitle("PC Algorithm Causal Graph with Variable Names")


#### Add previous knowledge ###
#If you want to forbid only one direction, use addBgKnowledge() (asymmetric).

amat.per
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



