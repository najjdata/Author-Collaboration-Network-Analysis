

# clear everything out of memory
rm(list=ls())  
getwd()
dir_path <-"C:/Users/elnaj/Desktop/MSBA Classes/classes-FAll 2020/IDS 564 media netwrk analysis/final project"
setwd(dir_path)
myData <- read.table('ca-GrQc.txt.gz')
str(myData)
library(igraph)
library(dplyr)

#create graph 

el <- myData
g_GRCN=graph.data.frame(el, directed = FALSE, vertices= NULL)

################################
#initial analysis
################################

# Edges result :28980
ecount(g_GRCN)
## Vertices result:5242
vcount(g_GRCN)


#Is it a simple graph? No!
## Check whether Self_loops exist, as do multiple edges
is.simple(g_GRCN)


# check cpmonents 

is_connected(g_GRCN) #False not connected
is_connected(g_GRCN, mode = "weak")
is_connected(g_GRCN, mode = "strong")
count_components(g_GRCN) #355 


# so it's undirected--
transitivity(g_GRCN)  #.6298
# Avg. path length and diameter
average.path.length(g_GRCN, directed=FALSE)  #6.0485

diameter(g_GRCN, directed = FALSE)  #17
# Summarize the graph structure
summary(g_GRCN)  #5242 x 28980 (14484 simplified)

table(sapply(maximal.cliques(g_GRCN), length))
# 1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   18   20   21   22   24   26 
# 1 1606 1606  594  182   52   28   13    9    4    1    4    2    1    1    1    2    1    1    1    1    1 
# 32   34   35   43   44 
# 1    1    1    2    1 


#################################################################
r
g_GRCN<-simplify(g_GRCN,remove.multiple = TRUE, edge.attr.comb="sum")
summary(g_GRCN)
is_simple(simplify(g_GRCN, remove.loops=FALSE))
num_weight<-E(g_GRCN)$weight 
length(num_weight)    
E(g_GRCN)$weight <-num_weight  # like this??
is.simple(g_GRCN)

####################################
#Degree //
####################################

plot(density(degree(g_GRCN)), main=" Degree Density Distribution")
plot(density(betweenness(g_GRCN)))

summary(degree.distribution(g_GRCN))

# normlized degree distribution 
t = table(degree(g_GRCN))
plot(t / sum(t), xlim=c(1,100), ylim=c(0, 0.28), xlab="Degree", ylab="Frequency", main= ("Normalized degree distribution"));


# compute the CCDF
deg = degree(g_GRCN)
P = NULL;
for (i in 1:max(deg)) {
  P[i] = length(deg[deg >= i]) / length(deg);
} 
# ploss ccdf of degree distrbution
plot(1:max(deg), P, log="xy", type = "l", xlab="Degree", ylab="CCDF", main= "complementary cumulative distribution function (CCDF)")
# degree frequency
hist(deg,  xlab="Degree", main= ("Degree frequency"))
library("ggplot2")
G.degrees <- degree(g_GRCN)

# Let's count the frequencies of each degree
G.degree.histogram <- as.data.frame(table(G.degrees))

# Need to convert the first column to numbers, otherwise
# the log-log thing will not work (that's fair...)
G.degree.histogram[,1] <- as.numeric(G.degree.histogram[,1])

# Now, plot it!
ggplot(G.degree.histogram, aes(x = G.degrees, y = Freq)) +
  geom_point() +
  scale_x_continuous("Degree\n(nodes with this amount of connections)",
                     breaks = c(1, 3, 10, 30, 100, 300),
                     trans = "log10") +
  scale_y_continuous("Frequency\n(how many of them)",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000),
                     trans = "log10") +
  ggtitle("Degree Distribution (log-log)") +
  theme_bw()


mean(G.degrees)

################################
# Betweeness / /
#############################
g_betw <- betweenness(g_GRCN)

round(g_betw, 2)
summary (round(g_betw, 2))


#You may plot the Lorenz curve, with a straight line representing the equidistribution case, as follows:
lorenz = function(d) {
  d = sort(d, dec=TRUE)
  n = length(d);
  s = sum(d);
  p = seq(0, 1, 0.01);
  c = NULL;
  items = NULL;
  i = 1;
  for (x in p) {
    if (x == 0)  {
      items[i] = 0;
      c[i] = 0;
    } else { 
      items[i] = floor(n * x);
      c[i] = sum(d[1:items[i]]) / s;
    }  
    i = i + 1;
  }
  names(c) = p
  list = list(p = p, c = c, items = items); 
} 
  r = lorenz(degree(g_GRCN))
plot(r$p, r$c, type="l", lwd=3, xlab="percentage of top units", ylab="percentage of character", xlim=c(0,1), ylim=c(0,1));
curve(x+0, lty = 1, xlim=c(0,1), ylim=c(0,1), add=T);
#######################
#transitivity

####################


transitivity(g_GRCN)
# gives the clustering coefficient of the whole network

transitivity(g_GRCN,type="local")
# gives the clustering coefficient of each node

graph.edge.betweenness<-edge.betweenness(g_GRCN,e=E(g_GRCN))
graph.edge.betweenness
# Closeness refers to how connected a node is to its neighbors

graph.closeness<-closeness(g_GRCN,vids=V(g_GRCN))
graph.closeness


###############################################################################################################
# so it's undirected--
transitivity(g_GRCN)  #.6298
# Avg. path length and diameter
average.path.length(g_GRCN, directed=FALSE)  #6.0485

diameter(g_GRCN, directed = FALSE)  #17
# Summarize the graph structure
summary(g_GRCN)  #5242 x 28980 (14484 simplified)

table(sapply(maximal.cliques(g_GRCN), length))
# 1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   18   20   21   22   24   26 
# 1 1606 1394  594  182   52   28   13    9    4    1    4    2    1    1    1    2    1    1    1    1    1 
# 32   34   35   43   44 
# 1    1    1    2    1 

######## for some of these larger ones, let's look deeper and try to understand what's going on in those

largest_cliques(g_GRCN)   # these are the 42 vertices from the largest clique--plot these!!

# [1] 21012 22691 773   14807 3372  21847 2741  24955 6610  25758 11241 570   6179  45    21281 23293 15003 20635
# [19] 19423 18894 4164  7956  12365 17655 25346 1653  9785  21508 14540 12781 2212  19961 2952  6830  8879  11472
# [37] 12496 12851 15659 17692 20108 20562 22887 4513





############## triangles #########################
transitivity(g_GRCN, type="global") #ratio of triangles (direction disregarded) to connected triples.
#.62984
mean(transitivity(g_GRCN, type="local")) # what does this do-check
# triad types in this network #



# mean degree

mean(degree(g_GRCN))  #5.56

# degrees- what is most prevalent in the network?
max(degree.distribution(g_GRCN))  #.22835

dd <- degree.distribution(g_GRCN)
#filter the ones with zero
str(dd)


keep <- which(dd>0)
n<- dd[keep]

plot(n, xlim= c(1,50), col="blue", 
     xlab= "Number of connections", main="Frequency of degrees in network- removing the zeroes", 
     type="p")

barplot(n, xlim= c(1,50), col="blue",
        xlab= "Number of connections", main="Frequency of degrees in network- removing zeroes")


constraints_SAP <- round(constraint(g_GRCN, nodes=V(g_GRCN)), digits=4)
# Degree centrality
degree_sap <- degree(g_GRCN)
# Node betweenness
betweens_SAP <- round(betweenness(g_GRCN, v=V(g_GRCN), directed = FALSE, nobigint =TRUE, normalized = FALSE))
# Edge betwenness
edgebetweens_SAP<-edge.betweenness(g_GRCN, e=E(g_GRCN), directed = FALSE)
# Local clustering coefficients
clustering_SAP <- transitivity(g_GRCN, type="local", vids=V(g_GRCN)) 

meas <- cbind(degree_sap, edgebetweens_SAP, constraints_SAP, betweens_SAP)
round(cor(meas), 4)
# degree_sap edgebetweens_SAP constraints_SAP betweens_SAP
# degree_sap           1.0000          -0.0052         -0.6336       0.5170
# edgebetweens_SAP    -0.0052           1.0000          0.0041      -0.0021
# constraints_SAP     -0.6336           0.0041          1.0000      -0.4336
# betweens_SAP         0.5170          -0.0021         -0.4336       1.0000


###########################   clusters    ##############################################

is.connected(g_GRCN, mode = "strong")

clust1s <- clusters(g_GRCN, mode = "strong")
table(clust1s$csize)  
# 1    2    3    4    5    6    7    8    9   10   12   14 4158 
# 1  177   98   30   17   12    8    6    2    1    1    1    1 
table(clust1s$no)  #355 total clusters
table(clust1s$membership)

# different cluster sizes
#get the ones with 2,3, 9 and plot

cs <- which(clust1s$csize == 2)
cs2 <- el[cs, ]
cs <- which(clust1s$csize == 3)
cs3 <- el[cs, ]
cs <- which(clust1s$csize == 4 | clust1s$csize == 5 | clust1s$csize == 6)
cs4 <- el[cs, ]

cs2 <- rbind(cs2, cs3, cs4)

g_GRCN2=graph.data.frame(cs2, directed = FALSE, vertices= NULL)

#g_GRCN2<-simplify(g_GRCN2, edge.attr.comb="sum")  do we need to simplify?

l=layout_with_kk(g_GRCN2)
plot(g_GRCN2, main = "strong with clusters =2, 3, 4", layout=l)


# # Closeness Centrality
get.adjacency(g_GRCN, sparse=FALSE)
 close_SAP <- closeness(g_GRCN)
 mean(close_SAP)  # not too good

# can we do k means clustering w this??
g <- g_GRCN2
M<-as_adjacency_matrix(g, sparse=FALSE)

results <- mst.knn(M)

#igraph::V(results$network)$label.cex <- seq(0.6,0.6,length.out=2)

plot(results$network, vertex.size=8, 
     vertex.color=igraph::clusters(results$network)$membership, 
     layout=igraph::layout.fruchterman.reingold(results$network, niter=10000),
     main=paste("MST-kNN \n Clustering solution \n Number of clusters=",results$cnumber,sep="" ))



num_weight<-E(g_GRCN_simpl)$weight 
E(g_GRCN_simpl)$weight <- num_weight
E(g_GRCN_simpl)$width <- (E(g_GRCN_simpl)$weight)
plot(g_GRCN_simpl, vertex.label = NA, layout=layout_with_mds(g_GRCN_simpl), mark.groups = cw,
     main="Original Network-Simplified")


summary(num_weight)

cw <- walktrap.community(g_GRCN)
modularity(cw)
#c.w1 <- membership(cw)
V(g_GRCN)$size <- 3*(graph.strength(g_GRCN))^(1/3)  #optimal for viewing
V(g_GRCN)$color <- "lightblue"
#edge weight-prop
plot(g_GRCN, edge.arrow.size=0.2, vertex.label = NA, mark.groups = cw, 
     main="Original Network- Fruchterman Reingold")


# doing walktrap is better if it's with smaller groups- think how we could subset?



#consider a subset on cliques instead of clusters? maybe examine cliques of size x
v<- cliques(g_GRCN)
v #they are part of  the largest clique

g_GRCN

# a decent plot of the overall network - simplified
V(g_GRCN_simpl)$size <- 3*(graph.strength(g_GRCN_simpl))^(1/3)  #optimal for viewing
V(g_GRCN_simpl)$color <- "lightblue"
#l <- layout_with_kk(g_GRCN)
#edge weight-prop
#edge weight-prop
num_weight<-E(g_GRCN_simpl)$weight 
E(g_GRCN_simpl)$weight <- num_weight
E(g_GRCN_simpl)$width <- (E(g_GRCN_simpl)$weight)
plot(g_GRCN_simpl, vertex.label = NA, layout=layout_with_mds(g_GRCN_simpl), mark.groups = cw,
     main="Original Network-Simplified")



summary(num_weight)

# doing walktrap is better if it's with smaller groups



#consider a subset on cliques instead of clusters?
v<- count_max_cliques(g_GRCN_simpl)
v  #they are part of  the largest clique


g_GRCN

# just looking at the largest clique
s1<-induced_subgraph(g_GRCN_simpl, v= largest.cliques(g_GRCN_simpl)[[1]])
s1
l=layout_with_gem(s1)

V(s1)$size <- 3*(graph.strength(s1))^(1/3)  #optimal for viewing
V(s1)$color <- "lightblue"
#l <- layout_with_kk(g_GRCN)
#edge weight-prop
#edge weight-prop
num_weight<-E(s1)$weight 
E(s1)$weight <- num_weight
E(s1)$width <- (E(s1)$weight)
plot(s1, vertex.label = NA, layout=l,
     main="Largest Clique In Network-GEM layout")

#all the weights are the same...


# subgraph with only the largest clique
edge_density(g_GRCN_simpl)
mean(degree(s1))

g <- g_GRCN_simpl   #this looks bad consider only subgroups for this viz
M<-as_adjacency_matrix(g, sparse=FALSE)

results <- mst.knn(M)

#igraph::V(results$network)$label.cex <- seq(0.6,0.6,length.out=2)

plot(results$network, vertex.size=.8, 
     vertex.color=igraph::clusters(results$network)$membership, 
     layout=igraph::layout.fruchterman.reingold(results$network, niter=10000),
     main=paste("MST-kNN \n Clustering solution \n Number of clusters=",results$cnumber,sep="" ))


############ see if you can get the top (20-30? any number and make it into graph object)
num_weight<-E(g_GRCN_simpl)$weight
h<-hub.score(g_GRCN_simpl, scale=FALSE, weights = num_weight)$vector 
head(sort(h, decreasing=TRUE))
hd<-head(sort(h,decreasing=TRUE), n = 30)  #here are the nodes


#hub and authorities are same since the graph is undirected





