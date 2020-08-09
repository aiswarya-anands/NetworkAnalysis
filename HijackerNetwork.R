# Required Libraries
library(igraph)
library(readxl)
library(RColorBrewer)

# Load adjacency file - all
Hijackers <- read_excel("Hijackers.xls")
View(Hijackers)

# Load adjacency file - main
Main <- read_excel("Hijackers_OnPlane.xls")
View(Main)

# Load attributes file
Attributes <- read_excel("Attributes.xls")
View(Attributes)

# Create data
Hij <- as.matrix(Hijackers)
rownames(Hij) <- colnames(Hij)

Main.Hij <- as.matrix(Main)
rownames(Main.Hij) <- colnames(Main.Hij)

# 1
# Network from Adjacency Matrix

# 19-only network
Main.network <- graph_from_adjacency_matrix(Main.Hij, mode="undirected", weighted = NULL) %>%
  # Set attributes to nodes
  set_vertex_attr("Ties", value = Attributes$`Ties`) %>%
  set_vertex_attr("Meeting", value = Attributes$`Las Vegas Meeting`) 

colrs <- c("#D95F02","#7570B3","#E7298A","#66A61E","#666666" )
V(Main.network)$color <- colrs[V(Main.network)$Ties]
plot(Main.network, vertex.size=5, vertex.label=NA)

# All network
Hij.network <- graph_from_adjacency_matrix(Hij, mode="undirected", weighted = NULL) %>%
  # Set attributes to nodes
  set_vertex_attr("Network.Strength", value = Attributes$`Network Strength`)
  
plot(Hij.network, edge.arrow.size=.3, vertex.color="gold", vertex.size=5, 
     vertex.frame.color="gray", vertex.label.color="black", vertex.label.cex=0.6, 
     vertex.label.dist=2, edge.curved=0.2)

new.colrs <- c("#D95F02","#666666")
V(Hij.network)$color <- new.colrs[V(Hij.network)$Network.Strength]
plot(Hij.network, vertex.size=5, vertex.label=NA)

# Average Path Length
path <- average.path.length(Hij.network)
path

# 2
# Centrality measures

# Degree Centrality
deg.cen <- degree(Hij.network, mode="all")
plot(Hij.network, edge.arrow.size=.3, vertex.color="#666666", vertex.label=NA,
     vertex.frame.color="gray", edge.curved=0.2, vertex.size=deg.cen)

deg.dist <- degree_distribution(Hij.network, mode="all", cumulative=T)
hist(deg.cen, breaks=1:vcount(Hij.network)-1, main="Histogram of node degree", xlab="Degree")


plot(x=0:max(deg.cen), y=1-deg.dist, pch=19, cex=1.2, col="orange",
      xlab="Degree", ylab="Cumulative Frequency")

write.csv(deg.cen, file = "degree.csv")

# Closeness Centrality
clo.cen <- closeness(Hij.network)
plot(Hij.network, edge.arrow.size=.3, vertex.color="#666666", vertex.label=NA,
     vertex.frame.color="gray", edge.curved=0.2, vertex.size=500*clo.cen)
clo.cen
write.csv(clo.cen, file = "closeness.csv")

# Betweenness Centrality
bet.cen <- betweenness(Hij.network)
bet.cen
plot(Hij.network, edge.arrow.size=.3, vertex.color="#666666", vertex.label=NA,
     vertex.frame.color="gray", vertex.size=0.02*bet.cen)
write.csv(bet.cen, file = "betweenness.csv")

#Eigenvector Centrality
eig.cen <- eigen_centrality(Hij.network)
eig.cen$vector
plot(Hij.network, edge.arrow.size=.3, vertex.color="#666666", vertex.label=NA,
     vertex.frame.color="gray", vertex.size=10*eig.cen$vector)
write.csv(eig.cen, file = "eigen.csv")

# 3
# Empirical Network
l.fr <- layout_with_fr(Hij.network)
plot(Hij.network, vertex.label= NA, vertex.size = 0.5, layout=l.fr) 

# Graph Centrality
eg.deg.cen <- centr_degree(Hij.network)$centralization
eg.clo.cen <- centr_clo(Hij.network)$centralization
eg.bet.cen <- centr_betw(Hij.network)$centralization
eg.eig.cen <- centr_eigen(Hij.network)$centralization

cat("Empirical Network : Degree Centrality : ", eg.deg.cen)
cat("Empirical Network : Closeness Centrality : ", eg.clo.cen)
cat("Empirical Network : Between Centrality : ", eg.bet.cen)
cat("Empirical Network : Eigenvector Centrality : ", eg.eig.cen)

cat("Empirical Network : Mean of Degree Centrality : ", mean(deg.cen))
cat("Empirical Network : Mean of Closeness Centrality : ", mean(clo.cen))
cat("Empirical Network : Mean of Between Centrality : ", mean(bet.cen))
cat("Empirical Network : Mean of Eigenvector Centrality : ", mean(eig.cen$vector))

cat("Average Path Length",average.path.length(Hij.network))

# Barabasi-Albert Model

bar.graph <- sample_pa(61, power = 1.2, m = NULL, out.dist = NULL, out.seq = NULL,
                   out.pref = FALSE, zero.appeal = 1, directed = FALSE,
                   algorithm ="psumtree", start.graph = NULL)
plot(bar.graph, vertex.label= NA, vertex.size = 0.5, xlab = "Scale-free network model")

cat("BA Network : Degree Centrality : ", centr_degree(bar.graph)$centralization)
cat("BA Network : Closeness Centrality : ", centr_clo(bar.graph)$centralization)
cat("BA Network : Between Centrality : ", centr_betw(bar.graph)$centralization)
cat("BA Network  Networkector Centrality : ", centr_eigen(bar.graph)$centralization)

cat("BA Network : Mean of Degree Centrality : ", mean(degree(bar.graph)))
cat("BA Network : Mean of Closeness Centrality : ", mean(closeness(bar.graph)))
cat("BA Network : Mean of Between Centrality : ", mean(betweenness(bar.graph)))
cat("BA Network : Mean of Eigenvector Centrality : ", mean(eigen_centrality(bar.graph)$vector))

cat("Average Path Length",average.path.length(bar.graph))

# Small World Model
sw.graph <- watts.strogatz.game(1, 61, 1, 0.1, loops = FALSE, multiple = FALSE)
plot(sw.graph, vertex.label= NA,vertex.size = 0.5, xlab = "Small world model")

l <- layout_in_circle(Hij.network)
plot(Hij.network, layout=l, vertex.size = 0.5, vertex.label=NA)

cat("SW Network : Degree Centrality : ", centr_degree(sw.graph)$centralization)
cat("SW Network : Closeness Centrality : ", centr_clo(sw.graph)$centralization)
cat("SW Network : Between Centrality : ", centr_betw(sw.graph)$centralization)
cat("SW Network : Eigenvector Centrality : ", centr_eigen(sw.graph)$centralization)

cat("SW Network : Mean of Degree Centrality : ", mean(degree(sw.graph)))
cat("SW Network : Mean of Closeness Centrality : ", mean(closeness(sw.graph)))
cat("SW Network : Mean of Between Centrality : ", mean(betweenness(sw.graph)))
cat("SW Network : Mean of Eigenvector Centrality : ", mean(eigen_centrality(sw.graph)$vector))

cat("Average Path Length",average.path.length(sw.graph))

# Random Network
# No of nodes = 61
# No of edges = 131
n <- 61
m <- gsize(Hij.network)
ran.graph <- erdos.renyi.game(n, m, type="gnm", directed = FALSE)
plot(ran.graph, vertex.label= NA, vertex.size = 0.5, xlab = "Random Network: G(N,L) model")

# Simulation
# Generate 100 random graphs of the same size
x <- 1
g.deg.cen <- c()
g.clo.cen <- c()
g.bet.cen <- c()
g.eig.cen <- c()
avg.deg.cen <- c()
avg.clo.cen <- c()
avg.bet.cen <- c()
avg.eig.cen <- c()

repeat {
  ran.graph <- sample_gnm(n, m, directed = FALSE)

  deg.cen <- c(g.deg.cen, centr_degree(ran.graph)$centralization)
  clo.cen <- c(g.clo.cen, centr_clo(ran.graph)$centralization)
  bet.cen <- c(g.bet.cen, centr_betw(ran.graph)$centralization)
  eig.cen <- c(g.eig.cen, centr_eigen(ran.graph)$centralization)
  
  avg.deg.cen <- c(avg.deg.cen, mean(degree(ran.graph)))
  avg.clo.cen <- c(avg.clo.cen, mean(closeness(ran.graph)))
  avg.bet.cen <- c(avg.bet.cen, mean(betweenness(ran.graph)))
  avg.eig.cen <- c(avg.eig.cen, mean(eigen_centrality(ran.graph)$vector))
                   
  if (x == 100){
    break
  }
  x = x+1
}

rg.deg.cen <- mean(deg.cen)
rg.clo.cen <- mean(clo.cen)
rg.bet.cen <- mean(bet.cen)
rg.eig.cen <- mean(eig.cen)

cat("Random Network : Degree Centrality : ", rg.deg.cen)
cat("Random Network : Closeness Centrality : ", rg.clo.cen)
cat("Random Network : Between Centrality : ", rg.bet.cen)
cat("Random Network : Eigenvector Centrality : ", rg.eig.cen)


cat("SW Network : Mean of Degree Centrality : ", mean(avg.deg.cen))
cat("SW Network : Mean of Closeness Centrality : ", mean(avg.clo.cen))
cat("SW Network : Mean of Between Centrality : ", mean(avg.bet.cen))
cat("SW Network : Mean of Eigenvector Centrality : ", mean(avg.eig.cen))

cat("Average Path Length",average.path.length(ran.graph))

