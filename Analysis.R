load("~/Documents/GitHub/CustomersVisualBehavior/PreProcessedData.RData")


all_aois <- bind_rows(
 lapply(
  NetworkList,
  function(x){
   
   data.frame(
    AOI = names(x$node_freq),
    Visits = as.numeric(x$node_freq)
   )
   
  }
 )
)

freq_tab <- all_aois %>%
 group_by(AOI) %>%
 summarise(
  Visits = sum(Visits)
 ) %>%
 arrange(
  desc(Visits)
 )


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
  c("white","darkgreen")
 )(100)
)
