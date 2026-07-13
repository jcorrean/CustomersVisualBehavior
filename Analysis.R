
library(igraph) 
library(dplyr) 
all_edges <- bind_rows( lapply(NetworkList, function(x) x$edges))
agg_edges <- all_edges %>% count(from,to,name="weight") 

library(pheatmap)

# matriz de transiciones
mat <- xtabs(
 weight ~ from + to,
 data = agg_edges
)

pheatmap(
 mat,
 clustering_distance_rows = "euclidean",
 clustering_distance_cols = "euclidean",
 clustering_method = "ward.D2",
 color = colorRampPalette(
  c("white","orange","red")
 )(100)
)