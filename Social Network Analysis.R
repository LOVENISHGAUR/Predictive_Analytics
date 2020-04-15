install.packages("igraph")
install.packages("tidyr", dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("splitstackshape", dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("networkD3", dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages("webshot", dependencies=TRUE, repos='http://cran.rstudio.com/')

library(stringr)
library(igraph)
library(dplyr)
library(splitstackshape)
library(tidyr)
library(networkD3)

getwd()
setwd("/Users/lovenishgaur/Documents/Predictive_Analytics_IIM_Raipur/Campus_session/")


#1. Working Example for learning
#================================
drug.df <- read.csv("Drug.csv")
str(drug.df)

# convert edges to edge list matrix
edges <- as.matrix(drug.df[, c(1,2)])
g <- graph.edgelist(edges,directed=FALSE)
g
eigen_centrality(g)$vector
options(scipen=999)
eigen_centr


# plot graph
# nodes' size is proportional to their eigenvector centrality
plot(g)
plot(g, vertex.label = NA, vertex.size = eigen_centrality(g)$vector*20)
plot(g, vertex.label = drug.df$Entity, vertex.size = eigen_centrality(g)$vector*20)
plot(g, vertex.label = round(eigen_centrality(g)$vector,2), vertex.size = eigen_centrality(g)$vector*20)
plot(g, vertex.label.dist=0.5, vertex.size = eigen_centrality(g)$vector*20)

plot(g, layout = layout_in_circle, vertex.size = 1, vertex.label.dist = 0.5)
plot(g, layout = layout_in_circle, vertex.size = eigen_centrality(g)$vector*20, vertex.label.dist = 0.5, alpha=0.5)

plot(g, layout = layout_on_grid, vertex.size = 1, vertex.label.dist = 0.5)
plot(g, layout = layout_on_grid, vertex.size = eigen_centrality(g)$vector*20, vertex.label.dist = 0.5)

simpleNetwork(as.data.frame(edges))

#Centrality Metrics
degree(g)
betweenness(g)
closeness(g)
eigen_centrality(g)


# get 10 Kings Heath's 1-level ego network
# for a 2-level ego network set argument order = 2 in make_ego_graph().
Kings.ego <- make_ego_graph(g, order = 1, nodes = "10 Kings Heath")
plot(Kings.ego[[1]], vertex.size = 1, vertex.label.dist = 0.5)

#Degree of distribution
degree.distribution(g) # normalized
edge_density(g)



#2. Amazon Product Sales Network
#================================

data = read.csv("amazon0302.csv")
str(data)

edges <- as.matrix(data[,1:2])
g <- graph.edgelist(edges+1,directed=TRUE)
g
eigen_centrality(g)
# plot graph
# nodes' size is proportional to their eigenvector centrality
plot(g, vertex.label = NA, vertex.size = 1)


#3. How ISIS uses Twitter
#================================

tweets = read.csv("ISIS_tweets.csv")
str(tweets)
print(paste("Read ", length(tweets$tweets), " tweets.", sep=""));
head(tweets)

################## Structuring the data ##################
# Get @-messages, senders, receivers
at.sender <- as.character(tweets$username)
mentions <- tolower(str_extract_all(tweets$tweets, "@\\w+"))
mentions.df<- data.frame(at.sender, mentions)
str(mentions.df)

#String split (deleting the @ and un-nesting)/(this is a deep layer command)
finalMentions = mentions.df %>% mutate(mentions = strsplit(as.character(mentions), ",")) %>% unnest(mentions)
str(finalMentions)

#gsub perform replacement of the first and all matches respectively.
finalMentions$mentions = gsub("character\\(0\\)","",finalMentions$mentions)
finalMentions = finalMentions[which(finalMentions$mentions!=""),]
finalMentions

#removing double quotes
finalMentions$mentions<-gsub("\"","",finalMentions$mentions)
# removing @
finalMentions$mentions<-gsub("@","",finalMentions$mentions)
# removing c(
finalMentions$mentions<-gsub("c\\(","",finalMentions$mentions)
# removing )
finalMentions$mentions<-gsub("\\)","",finalMentions$mentions)
# removing space
finalMentions$mentions<-gsub(" ","",finalMentions$mentions)

print(paste( " @-messages from ", length(unique(finalMentions$at.sender)), " senders and ", length(unique(finalMentions$mentions)), " receivers.", sep=""));

#Transform data frame into a graph
ats.graph <- graph.data.frame(finalMentions, directed=T)
plot(ats.graph)
plot(ats.graph, vertex.label = NA, vertex.size = eigen_centrality(ats.graph)$vector*20)

ats.graph <- graph.data.frame(finalMentions, directed=T)
plot(ats.graph)

edges <- as.matrix(finalMentions[, c(1,2)])
g <- graph.edgelist(edges,directed=TRUE)
plot(g, vertex.label = NA, vertex.size = 1)
plot(g, vertex.label = NA, vertex.size = eigen_centrality(g)$vector*20)
plot(g, vertex.label = eigen_centrality(g)$vector, vertex.size = eigen_centrality(g)$vector*20)
plot(ats.graph, vertex.label.dist=0.5, vertex.size = eigen_centrality(g)$vector*20)

simpleNetwork(as.data.frame(edges))

#4. Calculating Network Metrics
c = as.data.frame(eigen_centrality(ats.graph)$vector)
options(scipen=999)
max(eigen_centrality(ats.graph)$vector)
write.csv(as.data.frame(c), file="eigen_centrality.csv")

d = as.data.frame(degree(ats.graph))
degree(ats.graph, v=V(ats.graph), mode="out", loops=TRUE, normalized = FALSE)
degree_out = as.data.frame(degree(ats.graph, v=V(ats.graph), mode="out", loops=TRUE, normalized = FALSE))
degree_in = as.data.frame(degree(ats.graph, v=V(ats.graph), mode="in", loops=TRUE, normalized = FALSE))
max(degree(ats.graph))
write.csv(degree_in, file="indegree.csv")
write.csv(degree_out, file="outdegree.csv")

betweenness(ats.graph)
betweenness = as.data.frame(betweenness(ats.graph))
write.csv(betweenness, file="betweenness.csv")

closeness(ats.graph)
write.csv(closeness(ats.graph), file="closeness.csv")

eigen_centrality(ats.graph)

mobiayubi.ego <- make_ego_graph(ats.graph, order = 1, nodes = "mobi_ayubi")
plot(mobiayubi.ego[[1]], vertex.size = 1, vertex.label.dist = 0.5)

maghrebiqm.ego <- make_ego_graph(ats.graph, order = 1, nodes = "maghrebiqm")
plot(maghrebiqm.ego[[1]], vertex.size = 1, vertex.label.dist = 0.5)

pachaconsumer.ego <- make_ego_graph(ats.graph, order = 1, nodes = "pachaconsumer")
plot(pachaconsumer.ego[[1]], vertex.size = 1, vertex.label.dist = 0.5)

IshfaqAhmad.ego <- make_ego_graph(ats.graph, order = 1, nodes = "_IshfaqAhmad")
plot(IshfaqAhmad.ego[[1]], vertex.size = eigen_centrality(g)$vector*20, vertex.label.dist = 0.5)

fidaee.ego <- make_ego_graph(ats.graph, order = 1, nodes = "Fidaee_Fulaani")
plot(fidaee.ego[[1]], vertex.size = eigen_centrality(g)$vector*40, vertex.label.dist = 0.5)

warrnews.ego <- make_ego_graph(ats.graph, order = 1, nodes = "warrnews")
plot(warrnews.ego[[1]], vertex.size = eigen_centrality(ats.graph)$vector*20, vertex.label.dist = 0.5)

Uncle_SamCoco.ego <- make_ego_graph(ats.graph, order = 1, nodes = "Uncle_SamCoco")
plot(Uncle_SamCoco.ego[[1]], vertex.size = eigen_centrality(ats.gmake_tree(40, children = 3, mode = "undirected")raph)$vector*20, vertex.label.dist = 0.5)

tr = make_tree(finalMentions, mode = "out")
plot(tr, vertex.size=10, vertex.label=NA) 

edge_density(ats.graph)

#Hubs and Authorities
#=======================
hs <- hub_score(ats.graph, weights=NA)$vector

as <- authority_score(ats.graph, weights=NA)$vector


par(mfrow=c(1,2))

plot(ats.graph, vertex.size=hs*50, vertex.label=NA)

plot(ats.graph, vertex.size=as*100, vertex.label=NA)


#3d Interactive Visualization of Graph
#====================================

library(networkD3)
library(webshot)
ats.graph = simpleNetwork(finalMentions)
saveNetwork(ats.graph,file = 'ISIS_Network.html',selfcontained = T)
str(finalMentions)
finalMentions$mentions = as.factor(finalMentions$mentions)
finalMentions = as.data.frame(finalMentions)
simpleNetwork(finalMentions)

forceNetwork(Links=finalMentions, Nodes=finalMentions, NodeID = "mentions", Group="at.sender")
forceNetwork(Links=finalMentions, Nodes=finalMentions, NodeID = "at.sender", Group="at.sender")
forceNetworkOutput(ats.graph)
