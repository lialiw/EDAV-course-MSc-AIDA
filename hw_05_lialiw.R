# Sources:
# https://stackoverflow.com/questions/20209303/how-to-get-vertex-ids-back-from-graph
# https://www.rdocumentation.org/packages/tidygraph/versions/1.2.0/topics/morphers
# https://rdrr.io/cran/tidygraph/man/group_graph.html
# https://rdrr.io/github/thomasp85/tidygraph/man/morphers.html


library(tidyverse)
library(readr)
library(tidygraph)
library(ggraph)
library(stringr)
library(igraph)

gr_municipal <- read_csv("hw_05_greece.csv")
head(gr_municipal)
#str(gr_municipal)
#view(gr_municipal)

graph_routes <- as_tbl_graph(gr_municipal)
graph_routes
class(graph_routes)

# 1. Create an undirected network where the nodes are the prefectures and the edges connect 
# adjacent prefectures with the road distance of their capitals as the weight.

#ggraph(graph_routes) + geom_edge_link() + geom_node_point() + theme_graph()

ggraph(graph_routes, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = distance), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  labs(edge_width = "Distances") +
  theme_graph() 


# 2. Provide a graph of the network you created (experiment with the various layouts of 
# ggraph for the most aesthetically pleasing result for you). In any case, the names of 
# the nodes should at least be displayed. You can use the distance between two nodes to 
# color or define the thickness of the edges that connect them.

  graph_routes %>%
    ggraph(layout = "kk") +
    geom_node_text(aes(label = name, color = name), size = 3, repel = TRUE) +
    #geom_edge_diagonal() +
    geom_edge_link(aes(width = distance),color = "grey", alpha = 0.7) +
    scale_edge_width(range = c(0.2, 1))
  
  
# 3. Find the shortest path between "Evros" and "Messinia" and provide the corresponding 
# graphs when:

cities <- graph_routes %>% activate(nodes) %>% pull(name)
cities

from <- which(cities == "Έβρος")
to <-  which(cities == "Μεσσηνία")

# a) The cost is the road distance between the capitals of the prefectures.

shortest <- graph_routes %>% morph(to_shortest_path, from, to, weights = distance) %>%
            mutate(selected_node = TRUE)  %>%
            activate(edges) %>% 
            mutate(selected_edge = TRUE)  %>%
            unmorph() %>%
  
            activate(nodes) %>%
            mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
            activate(edges) %>%
            mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
            arrange(selected_edge)

shortest

shortest %>%
  ggraph(layout = "kk") +
  geom_edge_diagonal(aes(alpha = selected_edge), color = "gray") +
  geom_node_text(aes(label = name, color =name, alpha = selected_node ), size = 3)

shortest %>%
  activate(edges) %>%
  filter(selected_edge == 2) %>%
  as_tibble() %>%
  summarise( total_stops = n() - 1, total_distance = sum(distance) )


# b) The cost is the number of prefectures one crosses.

shortest <- graph_routes %>% morph(to_shortest_path, from, to) %>%
  mutate(selected_node = TRUE)  %>%
  activate(edges) %>% 
  mutate(selected_edge = TRUE)  %>%
  unmorph() %>%
  
  activate(nodes) %>%
  mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
  activate(edges) %>%
  mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
  arrange(selected_edge)

shortest

shortest %>%
  ggraph(layout = "kk") +
  geom_edge_diagonal(aes(alpha = selected_edge), color = "gray") +
  geom_node_text(aes(label = name, color =name, alpha = selected_node ), size = 3)

shortest %>%
  activate(edges) %>%
  filter(selected_edge == 2) %>%
  as_tibble() %>%
  summarise( total_stops = n() - 1, total_distance = sum(distance) )


shortest <- graph_routes %>% morph(to_shortest_path, from, to) %>%
  mutate(selected_node = TRUE)  %>%
  activate(edges) %>% 
  mutate(selected_edge = TRUE)  %>%
  unmorph() 

# 4. Calculate the minimum spanning tree of the network.

minimumSpanningTree <- graph_routes %>% morph(to_minimum_spanning_tree, weights= distance) %>%
  mutate(selected_node = TRUE)  %>%
  activate(edges) %>% 
  mutate(selected_edge = TRUE)  %>%
  unmorph() %>%
  
  activate(nodes) %>%
  mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
  activate(edges) %>%
  mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
  arrange(selected_edge)

minimumSpanningTree

minimumSpanningTree %>%
  ggraph(layout = "kk") +
  geom_edge_diagonal(aes(alpha = selected_edge), color = "gray") +
  geom_node_text(aes(label = name, color =name, alpha = selected_node ), size = 3)


# 5. Use some morph function of your choice.

comp <- morph(graph_routes, to_components, type="strong") %>%
  mutate(selected_node = TRUE)  %>%
  activate(edges) %>% 
  mutate(selected_edge = TRUE)%>%
  unmorph() %>%
  
  activate(nodes) %>%
  mutate(selected_node = ifelse(is.na(selected_node), 1, 2)) %>%
  activate(edges) %>%
  mutate(selected_edge = ifelse(is.na(selected_edge), 1, 2)) %>%
  arrange(selected_edge)

comp%>%
  ggraph(layout = "kk") +
  geom_edge_diagonal(aes(alpha = selected_edge), color = "gray") +
  geom_node_text(aes(label = name, color =name, alpha = selected_node ), size = 3)


