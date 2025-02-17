library(pcalg)
library(reshape2)
library(tidygraph)
library(ggraph)
library(ggplot2)
library(dagitty)
library(ggdag)


factors_list<-read_excel("factors_list.xlsx",sheet = "factors_list")


#########################################################################################################################################################################################
############# DATA TYPE CONVERSION
# data standardization I need to implement this first-----
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

##### RULE: Prohibition of paths
bk["age", setdiff(variables, "year_birth")] <- FALSE  # Prohibition of paths leading to “year_birth”, unless "age"->"year_birth
bk["year_birth", setdiff(variables, "age")] <- FALSE  # Prohibition of paths leading to “age”, unless "year_birth"->"age
bk[setdiff(variables, c("age","year_birth")),"years_in_community"] <- FALSE  # Prohibition of paths leading to "years_in_community" unless "age"->"years_in_community" or "year_birth"->"years_in_community" 

#bk["n_adults_wa", c("n_adults_wa_male", "n_adults_wa_female")] <- FALSE # Prohibition of this path n_adults_wa -> "n_adults_wa_male", "n_adults_wa_female"
#bk["n_adults_old", c("n_adults_old_male", "n_adults_old_female")] <- FALSE # Prohibition of this path n_adults_old -> "n_adults_old_male", "n_adults_old_female"
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
#### STEP 1. 	Estimation of the skeleton of the DAG. ----

#All information that is needed in the conditional independence test can be passed in the argument suffStat
per.suffStat <- list(C = cor(per_data_analysis), n = nrow(per_data_analysis))
per.suffStat

per.varNames <- colnames(per_data_analysis)
per.varNames

#The skeleton of a DAG is the undirected graph that has the same edges as the DAG but no edge orientations.
#The main task of function skeleton() in ﬁnding the skeleton is to compute and test several conditional independencies. 

#If used in the PC algorithm, it estimates the order-independent “PC-stable” (method="stable")

per.skeleton <- skeleton(per.suffStat, indepTest = gaussCItest,labels = per.varNames, alpha = 0.01,method = "stable")
per.skeleton

#### STEP 2. The PC-algorithm is implemented in function pc() ----
#The PC algorithm is known to be order-dependent, in the sense that the computed skeleton depends on the order in which the variables are given. 
#skel.method ="stable" (default) provides an order-independent skeleton
per.pc <- pc(per.suffStat, indepTest = gaussCItest,labels = per.varNames, alpha = 0.01,skel.method ="stable")
per.pc

par(mfrow = c(1,2))

#### STEP 3. Plot skeleton and PC results ----
## Using plot()
plot(per.skeleton, main = "Skeleton of DAG")
plot(per.pc, main = "PC Algorithm DAG")

## Using ggplot2()

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

## Using daggity
per.varNames # node  names

per.pc.matrix # adjacency matrix

per.pc.dagitty <- pcalg2dagitty(per.pc.matrix,per.varNames,type="dag")
per.pc.dagitty

per.pc.dagitty_edges <- per.pc.dagitty[!sapply(per.pc.dagitty, is.null)]
per.pc.dagitty_edges

per.pc.dag <- dagitty(paste("dag {", paste(per.pc.dagitty_edges, collapse="; "), "}"))
print(per.pc.dag)

plot(ggdag(per.pc.dag))

ggdag_status(per.pc.dag) + theme_dag()

#### STEP 4. Add background knowledge ----
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



#######################################################
############# FCI ALGORITHM ############################----
#######################################################
#### STEP 1. Estimation of the skeleton of the DAG. ----
#same as in PC algorithm

#### STEP 2.The FCI-algorithm is implemented in function fci() ----
## gaussCItest() function: using zStat() to test for (conditional) independence between gaussian random variables. Test Conditional Independence of Gaussians via Fisher's Z
#

per.fci <- fci(per.suffStat, indepTest=gaussCItest, labels =per.varNames ,alpha = 0.01) # TOO SLOW
per.rfci <- rfci(per.suffStat, indepTest=gaussCItest, labels =per.varNames ,alpha = 0.01) #  FAST

#### STEP 3. Plot FCI results ----
## Using plot()
plot(per.fci, main = "FCI Algorithm PAG")

plot(per.rfci, main = "RFCI Algorithm PAG")

## Using ggplot2()
# Extract adjacency matrix
per.rfci.amat <- as(per.rfci, "amat")
per.rfci.matrix <- as(per.rfci.amat, "matrix")  # Convert to standard matrix
per.rfci.matrix
per.rfci.t.matrix<-t(per.rfci.matrix)

# Convert adjacency matrix to data frame for visualization
per.rfci.edge_list <- melt(per.rfci.t.matrix)
per.rfci.edge_list
colnames(per.rfci.edge_list) <- c("from", "to", "weight")  # Rename columns
per.rfci.edge_list
per.rfci.edge_list <- per.rfci.edge_list[per.rfci.edge_list$weight == 1, ]  # Keep only edges that exist
per.rfci.edge_list

# Create a node data frame with proper variable names
per.rfci.node.names <- unique(c(per.rfci.edge_list$from, per.rfci.edge_list$to))
per.rfci.nodes <- data.frame(name = per.rfci.node.names, stringsAsFactors = FALSE)

# Convert edge list to a graph object
per.rfci.graph <- tbl_graph(nodes = per.rfci.nodes, edges = per.rfci.edge_list, directed = TRUE)
per.rfci.graph

per.rfci.graph <- per.rfci.graph %>% 
  activate(nodes) %>% 
  mutate(name = as.character(name))  # Ensure names remain characters
per.rfci.graph

ggraph(per.rfci.graph, layout = "fr") +  
  geom_edge_link(arrow = arrow(length = unit(4, "mm")), 
                 end_cap = circle(4, "mm"),  
                 edge_width = 1) +  
  geom_node_point(size = 6, color = "darkblue") +  
  geom_node_text(aes(label = name), repel = TRUE, size = 5) +  
  theme_void() +  
  ggtitle("RFCI Algorithm")

## Using daggity
# Extract adjacency matrix
per.varNames # node  names

per.rfci.matrix # adjacency matrix

per.rfci.dagitty <- pcalg2dagitty(per.rfci.matrix,per.varNames,type="pag")
per.rfci.dagitty

per.rfci.dagitty_edges <- per.rfci.dagitty[!sapply(per.rfci.dagitty, is.null)]
per.rfci.dagitty_edges

per.rfci.dag <- dagitty(paste("dag {", paste(per.pc.dagitty_edges, collapse="; "), "}"))
print(per.rfci.dag)

plot(ggdag(per.rfci.dag))

ggdag_status(per.rfci.dag) + theme_dag()



