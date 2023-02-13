library(igraph)
load("data/g.Rda") # load a sample graph

E(g)$weight # this graph has edge weights--most of them are 0 because the graph is pretty sparse (not all edges exist because not all individuals interacted).
strength(g) # gives us good values--these values make sense. This takes into account the edge weights.
degree(g) # degree doesn't seem to work for a weighted graph. It tells me that the degree of all individuals is 50 (there are 51 total individuals in the network) because the edges do exist, they just have a weight of 0.

# I want to be able to calculate degree(g) and have it ignore edges with weight 0, since weight 0 means that the individuals did not interact.

# A different problem: plotting doesn't work well either.
plot(g, vertex.size = 15, edge.width = 20*E(g)$weight, vertex.label = NA, vertex.color = "lightblue", main = "co-flight") # we get the error "non-positive edge weight found, ignoring all weights during graph layout".

# To make it plot correctly, we have to change all 0 edge weights to NA, like this:
g_for_plotting <- g
E(g_for_plotting)$weight <- na_if(E(g_for_plotting)$weight, 0)

# Now it will plot correctly:
plot(g_for_plotting, vertex.size = 15, edge.width = 20*E(g_for_plotting)$weight, vertex.label = NA, vertex.color = "lightblue", main = "co-flight") # this is how it's supposed to look!

# But changing the edge weights to NA makes it impossible to calculate strength:
strength(g_for_plotting) # returns NA's for everything
strength(g) # returns the proper values

degree(g_for_plotting) # and meanwhile, setting the edge weights to NA did NOT fix the original problem with degree: it still calculates the degrees as 50 for everyone, even though we can see clearly from the graph that they should not be.


