library(readr)
Secuencia <- read_delim("Secuencia.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

library(dplyr)
library(igraph)

build_aoi_network <- function(df){
 
 # Orden temporal
 df <- df[order(df$Secuencia), ]
 
 # Secuencia original de AOIs
 areas <- df$Area
 
 # Eliminar permanencias consecutivas
 areas <- areas[
  c(TRUE, diff(areas) != 0)
 ]
 
 # Necesitamos al menos dos AOIs
 if(length(areas) < 2){
  return(NULL)
 }
 
 # Crear aristas
 edges <- data.frame(
  from = head(areas, -1),
  to   = tail(areas, -1)
 )
 
 # Frecuencia de visitas por AOI
 node_freq <- table(df$Area)
 
 # Grafo dirigido
 g <- graph_from_data_frame(
  edges,
  directed = TRUE
 )
 
 # Atributo del nodo
 V(g)$freq <- as.numeric(
  node_freq[
   match(
    V(g)$name,
    names(node_freq)
   )
  ]
 )
 
 return(
  list(
   graph = g,
   edges = edges,
   node_freq = node_freq
  )
 )
}






















datos <- subset(
 Secuencia,
 Sujeto==104 &
  Ensayo==1
)

s104 <- datos[order(datos$Secuencia), ]



areas <- s104$Area

# Colapsar repeticiones consecutivas
areas <- areas[
 c(TRUE, diff(areas) != 0)
]

edges <- data.frame(
 from = head(areas, -1),
 to   = tail(areas, -1)
)

library(igraph)

g <- graph_from_data_frame(
 edges,
 directed = TRUE
)
plot(g)
