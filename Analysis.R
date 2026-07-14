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

png("F2.png", width = 5000, height = 3500,units = "px", res = 600)
pheatmap(
 mat,
 clustering_distance_rows = "euclidean",
 clustering_distance_cols = "euclidean",
 clustering_method = "ward.D2",
 color = colorRampPalette(
  c("white","darkgreen")
 )(100)
)
dev.off()

all_aois <- bind_rows(
  lapply(NetworkList,
         function(x){
           data.frame(
             AOI = names(x$node_freq),
             Visits = as.numeric(x$node_freq)
           )
         })
)

freq_tab <- all_aois %>%
  group_by(AOI) %>%
  summarise(
    Visits = sum(Visits)
  ) %>%
  arrange(desc(Visits))

knitr::kable(
  freq_tab,
  caption="AOI visitation frequencies."
)

library(ggplot2)
ggplot(
  freq_tab,
  aes(
    reorder(AOI,Visits),
    Visits
  )
)+
  geom_col(fill="darkgreen")+
  coord_flip()

all_edges <- bind_rows(
  lapply(
    NetworkList,
    function(x) x$edges
  )
)

agg_edges <- all_edges %>%
  count(from,to,name="weight")

mat <- xtabs(
  weight ~ from + to,
  data = agg_edges
)
