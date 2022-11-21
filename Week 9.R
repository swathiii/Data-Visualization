library(WDI)
new_wdi_cache <- WDIcache()

install.packages("igraph")
install.packages("ggnetwork")
library(igraph)
library(tidyverse)

airports <- read_csv('airports.dat', col_types = 'icccccdddccccc')
routes <- read_csv('routes.dat') #Rguesses the data types if it's not specified 

View(airports)
View(routes)
View(nodes)
View(links)
View(education)

nodes <- airports %>% select(IATACode, Name, City, Country, longitude, latitude) %>%
  filter(IATACode != '\\N') %>% #filter out the airports without any routes
  filter(Country == 'United Kingdom')

links <- routes %>% select(Source, Destination, Airline, Plane) %>%
  filter(Source %in% nodes$IATACode) %>%
  filter(Destination %in% nodes$IATACode)

net <- graph_from_data_frame( d=links, 
                              vertices = nodes,
                              directed = T)

#visualizing the network : ggnetwork (geom_edges function)
library(ggnetwork)

ggplot(net, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(colour='blue') + 
  geom_nodes(size=2) +
  geom_text(aes(x=x, y=y, label = City)) +
  theme_blank() +
  labs(title = 'Airport routes')

education <- WDI(indicator = c("SE.PRM.ENRR","SE.SEC.ENRR",
                               "SE.TER.ENRR","SE.SEC.PROG.ZS","SE.PRM.CMPT.ZS"),
                 start = 2014, 
                 end = 2014, 
                 extra = TRUE,
                 cache = new_wdi_cache)

education <- education[education$region!= 'Aggregates',]
education <- na.omit(education)

education.features <- education[,7:11]

#creating adjacency matrix
education.features_scaled <- scale(education.features)
education.distance.matrix <- as.matrix(dist(education.features_scaled))
#setting the threshold
education.adjacency_matrix <- education.distance.matrix < 1.0

g1 <- graph_from_adjacency_matrix(education.adjacency_matrix, mode = 'undirected')
#visualizing it 
ggplot(g1, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(colour = 'plum') +
  geom_nodes(size = 4, colour = 'grey') +
  theme_blank() +
  labs( caption = "WDI dataset")

#adding information and attributes to the graph
g1 <- set_vertex_attr(g1, name = "Region", value=as.character(education$region))
ggplot(g1, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(colour = 'grey') +
  geom_nodes(size = 4, aes(colour = Region)) +
  theme_blank() +
  labs(caption = 'WDI School enrollment and progression datasets')

#EXERCISE : use different threshold values
education.adjacency_matrix <- education.distance.matrix < 6.0

g1 <- set_vertex_attr(g1, name = "Region", value=as.character(education$region))
ggplot(g1, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(colour = 'grey') +
  geom_nodes(size = 4, aes(colour = Region)) +
  theme_blank() +
  labs(caption = 'WDI School enrollment and progression datasets')

#EXTRA: using a different distance measure : canberra, manhattan and minkowski
education.distance.matrix <- as.matrix(dist(education.features_scaled, 'minkowski'))

education.adjacency_matrix <- education.distance.matrix < 1.0

g1 <- set_vertex_attr(g1, name = "Region", value=as.character(education$region))
ggplot(g1, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(colour = 'grey') +
  geom_nodes(size = 4, aes(colour = Region)) +
  theme_blank() +
  labs(caption = 'WDI School enrollment and progression datasets')

#------------------------------GRAPH LAYOUT------------------------------------------------ 
#ggnetwork has a set of layout functions that can be used to change the
#layout of the graph

new.g1 <- ggnetwork(g1, layout = igraph::layout.random(g1))
ggplot(new.g1, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(colour = 'grey') +
  geom_nodes(size = 4, aes(colour = Region)) +
  theme_blank() +           #used to remove typical plot elements that are not shown in the network 
  labs(caption = 'WDI School enrollment and progression datasets')

#using a specific layout : Fruchterman-Reingold : a frequently used force-directed layout
new.g1 <- ggnetwork(g1, layout=igraph::layout.fruchterman.reingold(g1))
ggplot(new.g1, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(colour='grey') +
  geom_nodes(size = 4, aes(colour = Region)) +
  theme_blank() +       #used to remove typical plot elements that are not shown in the network 
  labs(caption='WDI School enrollment and progression datasets')

#using Kamada-Kawai force-directed algorithm 
new.g1 <- ggnetwork(g1, layout=igraph::layout.kamada.kawai(g1))
ggplot(new.g1, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(colour='grey') +
  geom_nodes(size = 4, aes(colour = Region)) +
  theme_blank() +  #used to remove typical plot elements that are not shown in the network 
  labs(caption = 'WDI School enrollment and progression datasets')

#EXERCISE : Try a different layout from the link : Davidson-Harel layout
new.g1 <- ggnetwork(g1, layout=igraph::layout.davidson.harel(g1))
ggplot(new.g1, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(colour='grey') +
  geom_nodes(size = 5, aes(colour=Region)) +
  theme_blank() +
  labs(caption = 'WDI School enrollment and progression datasets')

#EXTRA : Use income instead of region and observe the difference 
g1 <- set_vertex_attr(g1, name = "income", value=as.character(education$income))

new.g1 <- ggnetwork(g1, layout=igraph::layout.kamada.kawai(g1))
ggplot(new.g1, aes(x=x, y=y, xend=xend, yend=yend)) +
  geom_edges(colour='grey') +
  geom_nodes(size = 4, aes(colour = income)) +
  theme_blank() +  #used to remove typical plot elements that are not shown in the network 
  labs(caption = 'WDI School enrollment and progression datasets')



#-------------------------DENDOGRAMS---------------------------------------------------------
