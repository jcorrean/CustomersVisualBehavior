library(readr)
Secuencia <- read_delim("Secuencia.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

library(dplyr)
library(igraph)

build_aoi_network <- function(df){
 
 # ordenar por secuencia temporal
 df <- df[order(df$Secuencia), ]
 
 # extraer AOIs
 areas <- df$Area
 
 # eliminar permanencias consecutivas
 areas <- areas[
  c(TRUE, diff(areas) != 0)
 ]
 
 # construir aristas
 edges <- data.frame(
  from = head(areas, -1),
  to   = tail(areas, -1)
 )
 
 g <- graph_from_data_frame(
  edges,
  directed = TRUE
 )
 
 return(
  list(
   graph = g,
   edges = edges
  )
 )
}


build_aoi_network



NetworkList <- list()

subjects <- unique(Secuencia$Sujeto)

for(s in subjects){
 
 data_s <- dplyr::filter(
  Secuencia,
  Sujeto == s
 )
 
 ensayos <- unique(data_s$Ensayo)
 
 for(e in ensayos){
  
  data_se <- dplyr::filter(
   data_s,
   Ensayo == e
  )
  
  net <- build_aoi_network(data_se)
  
  if(!is.null(net)){
   
   nombre <- paste0(
    "S",
    s,
    "_E",
    e
   )
   
   NetworkList[[nombre]] <- net
  }
 }
}


s <- 101

data_s <- dplyr::filter(
 Secuencia,
 Sujeto == s
)

unique(data_s$Ensayo)

 data_se <- dplyr::filter(
  data_s,
  Ensayo == 1
 )

net <- build_aoi_network(data_se)

net


plot(net$graph)

data_se <- dplyr::filter(
 Secuencia,
 Sujeto == 101,
 Ensayo == 1
)

str(data_se)


head(data_se$Area)
class(data_se$Area)


