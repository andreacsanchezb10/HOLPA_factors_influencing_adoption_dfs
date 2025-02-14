library("pcalg")
library("Rgraphviz")



##########################################
############# PC ALGORITHM #########----
###########################################
#The dataset gmG8 contains n = 5000 observations of p = 8 continuous variables 
#with a multivariate Gaussian distribution.
data("gmG", package = "pcalg") ## loads data sets gmG and gmG8

#All information that is needed in the conditional independence test can be passed in the argument suffStat
suffStat <- list(C = cor(gmG8$x), n = nrow(gmG8$x))
suffStat

varNames <- gmG8$g@nodes
varNames

#### STEP 1. 	Estimation of the skeleton of the DAG. 
#The skeleton of a DAG is the undirected graph that has the same edges as the DAG but no edge orientations.
#The main task of function skeleton() in ﬁnding the skeleton is to compute and test several conditional independencies. 

#If used in the PC algorithm, it estimates the order-independent “PC-stable” (method="stable")

skel.gmG8 <- skeleton(suffStat, indepTest = gaussCItest,labels = varNames, alpha = 0.01,method = "stable")


#### STEP 2. The PC-algorithm is implemented in function pc()
#The PC algorithm is known to be order-dependent, in the sense that the computed skeleton depends on the order in which the variables are given. 
#skel.method ="stable" (default) provides an order-independent skeleton
pc.gmG8 <- pc(suffStat, indepTest = gaussCItest,labels = varNames, alpha = 0.01,skel.method ="stable")

par(mfrow = c(1,3))
plot(gmG8$g, main = "") #True causal DAG
plot(skel.gmG8, main = "") #estimated skeleton
plot(pc.gmG8, main = "") #estimated CPDAG

### plot with ggplot2
# Extract adjacency matrix
amat <- as(pc.gmG8, "amat")
amat <- as(amat, "matrix")  # Convert to standard matrix
amat

library(tidygraph)
library(ggraph)
library(ggplot2)
library(reshape2)

# Convert adjacency matrix to data frame for visualization
edge_list <- melt(amat)
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

##################################################################################################
############# PCI ALGORITHM WITH BACKGROUND INFORMATION ADDED #########----
#################################################################################################
#Assuming that we know the there is no edge from B and E, we can supply this on prior knowledge.
data("gmG", package = "pcalg") ## loads data sets gmG and gmG8

#All information that is needed in the conditional independence test can be passed in the argument suffStat
suffStat <- list(C = cor(gmG8$x), n = nrow(gmG8$x))
suffStat

varNames <- gmG8$g@nodes
varNames

#### STEP 1. 	Estimation of the skeleton of the DAG. 
#The skeleton of a DAG is the undirected graph that has the same edges as the DAG but no edge orientations.
#The main task of function skeleton() in ﬁnding the skeleton is to compute and test several conditional independencies. 

#If used in the PC algorithm, it estimates the order-independent “PC-stable” (method="stable")

skel.gmG8 <- skeleton(suffStat, indepTest = gaussCItest,labels = varNames, alpha = 0.01,method = "stable")


#### STEP 2. The PC-algorithm is implemented in function pc()
#The PC algorithm is known to be order-dependent, in the sense that the computed skeleton depends on the order in which the variables are given. 
#skel.method ="stable" (default) provides an order-independent skeleton
pc.gmG8 <- pc(suffStat, indepTest = gaussCItest,labels = varNames, alpha = 0.01,skel.method ="stable")
par(mfrow = c(1,1))
plot(pc.gmG8, main = "original") #estimated CPDAG
# Extract adjacency matrix from PC-algorithm output
amat <- as(pc.gmG8, "amat")
amat
# Add background knowledge
amat_updated <- addBgKnowledge(gInput = amat, x = "Bar", y = "Author" )
amat_updated

amat_matrix <- as(amat_updated, "matrix")
amat_matrix

pc.gmG8.graph <- as(amat_matrix, "graphNEL")
pc.gmG8.graph

par(mfrow = c(1,2))
plot(pc.gmG8, main = "original") #estimated CPDAG

plot(pc.gmG8.graph)

pc.new <- pc(suffStat, indepTest = gaussCItest,labels = varNames, alpha = 0.01,skel.method ="stable",
             edgedE=pc.gmG8.graph)








# Convert adjacency matrix to graphNEL properly
pc.graphNEL <- as(graphAM(adjMat = amat_updated, edgemode = "directed"), "graphNEL")

# Plot the updated graph
plot(pc.graphNEL)


pc.gmG8.graph <- as(amat_updated, "graphNEL")

# Convert back to a graph object
pc.gmG8.graph <- as(amat_updated, "graphNEL")

# Plot the updated graph
library(Rgraphviz)
plot(pc.gmG8.graph)


par(mfrow = c(1,1))

plot(pc.gmG8, main = "") #estimated CPDAG
as(pc.gmG8,"matrix")

as(pc.gmG8,"amat")
t(as(pc.gmG8,"amat"))

pc.gmG8 <- addBgKnowledge(pc.gmG8, x = "Author", y = "Bar")
pc.amat2
pc.amat2<-t(pc.amat2)

plot(pc.amat2,"graphNEL")

xpc.gmG8 <- pc(suffStat, indepTest = gaussCItest,labels = varNames, alpha = 0.01,skel.method ="stable",
              fixedGaps = pc.amat2)



par(mfrow = c(1,1))
plot(gmG8$g, main = "") #True causal DAG
plot(skel.gmG8, main = "") #estimated skeleton
plot(pc.gmG8, main = "") #estimated CPDAG


pag2cpdag<-function(fciAlgo){
  res<-as(fciAlgo,"amat")#a amat type pag
  res[res==3]<-1
  res[res==2]<-0
  return(res)
}
pag2cpdag(pc.gmG8)
res1<-pdag2allDags(pag2cpdag(pc.gmG8))
res1
plotAllDags(res1)

##################################################################################################
############# FCI (FAST CONDITIONAL INTERFERANCE) ALGORITHM #########----
#################################################################################################
#In the ﬁrst and second step, an initial skeleton and unshielded colliders are found as in the PC-algorithm.
#In the third step, a set called “Possible-D-SEP” is computed.
#Since the FCI and RFCI algorithms are build upon the PC algorithm, theyare also order-dependent in their skeleton estimation. It is more involved, 
#however, to resolve their order-dependence issues in the skeleton, see Colombo and Maathuis [2014]. However, the default function estimates an initial order-
#independent skeleton in these algorithms (for additional details on how to make the ﬁnal skeleton of FCI fully order-independent, see Colombo and Maathuis [2014]).
data("gmL")
suffStat <- list(C = cor(gmL$x), n = nrow(gmL$x)) 

varNames <- gmL$g@nodes
varNames

# LABEL 1 WAS STABLISH AS LATENT VARIABLE
fci.gmL <- fci(suffStat, indepTest=gaussCItest, alpha = 0.9999, labels = c("2","3","4","5"))
par(mfrow = c(1,2))

plot(gmL$g, main = "") #True causal DAG 
plot(fci.gmL) #estimated PAG on the observed nodes with the labels 2, 3, 4 and 5


## CODE FOR GRAPHS
#https://pmc.ncbi.nlm.nih.gov/articles/PMC8120292/




#####################################################################################
######### USING A DIFFERENT PACKAGE
install.packages("dagitty")
library(reticulate)
library(tidyverse)
library(ggpubr)
library(dagitty)
library(broom)

# installation
# py_install("gcastle==1.0.3", pip = T)
# py_install("torchvision"), apparently the algorithm requires Torch

gc <- import("castle")
algo <- import("castle.algorithms")


set.seed(1)
n <- 10000
a <- rnorm(n) 
b <- rnorm(n) 
c <- 0.3*a + 0.2*b + 0.01*rnorm(n)
d <- 0.8*c + 0.01*rnorm(n)
# e <- -0.4*a + -0.4*d + 0.01*rnorm(n) # we will add a collider later

df <- data.frame(a,b,c,d)

df1 <- as.matrix(df)

df |>
  ggplot(aes(x=c,y=d)) +
  geom_point() +
  theme_minimal()

dag <- dagitty('dag {
bb="0,0,1,1"
A [pos="0.236,0.380"]
B [pos="0.238,0.561"]
C [pos="0.413,0.463"]
D [pos="0.600,0.460"]
A -> C
B -> C
C -> D
}'
)

plot(dag)


