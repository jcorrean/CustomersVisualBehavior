library(readr)
library(dplyr)
library(igraph)

# =====================================================
# CARGAR DATOS
# =====================================================

Secuencia <- read_delim(
  "Secuencia.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE
)

# =====================================================
# FUNCION PARA RECONSTRUIR RED AOI->AOI
# =====================================================

build_aoi_network <- function(df){
  
  # Orden temporal
  df <- df[order(df$Secuencia), ]
  
  # AOIs visitados
  areas <- df$Area
  
  # Si solo hay una observación no se puede construir red
  if(length(areas) < 2){
    
    return(NULL)
    
  }
  
  # Transiciones consecutivas
  edges <- data.frame(
    from = head(areas,-1),
    to   = tail(areas,-1)
  )
  
  # Frecuencia de transición
  edges_weighted <- edges %>%
    count(from,to,name="weight")
  
  # Grafo dirigido
  g <- graph_from_data_frame(
    edges_weighted,
    directed = TRUE
  )
  
  # Peso de aristas
  E(g)$weight <- edges_weighted$weight
  
  # Frecuencia de visitas por AOI
  node_freq <- table(areas)
  
  V(g)$visits <- as.numeric(
    node_freq[
      match(
        V(g)$name,
        names(node_freq)
      )
    ]
  )
  
  # Devolver todo
  return(
    list(
      graph = g,
      edges = edges,
      edges_weighted = edges_weighted,
      node_freq = node_freq
    )
  )
}

# =====================================================
# CONSTRUIR TODAS LAS REDES
# =====================================================

NetworkList <- list()

subjects <- unique(Secuencia$Sujeto)

for(s in subjects){
  
  cat("Procesando sujeto:",s,"\n")
  
  data_s <- filter(
    Secuencia,
    Sujeto == s
  )
  
  ensayos <- unique(data_s$Ensayo)
  
  for(e in ensayos){
    
    data_se <- filter(
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

# =====================================================
# EJEMPLO
# =====================================================

net104 <- NetworkList[["S104_E1"]]

plot(
  net104$graph,
  vertex.size = V(net104$graph)$visits * 5,
  edge.width = E(net104$graph)$weight,
  vertex.label.cex = 0.8,
  edge.arrow.size = .4,
  layout = layout_with_fr(net104$graph)
)

length(NetworkList)

Secuencia |>
  dplyr::distinct(Sujeto, Ensayo) |>
  nrow()


Problemas <- data.frame()

subjects <- unique(Secuencia$Sujeto)

for(s in subjects){
  
  data_s <- filter(
    Secuencia,
    Sujeto == s
  )
  
  ensayos <- unique(data_s$Ensayo)
  
  for(e in ensayos){
    
    data_se <- filter(
      data_s,
      Ensayo == e
    )
    
    if(nrow(data_se) < 2){
      
      Problemas <- rbind(
        Problemas,
        data.frame(
          Sujeto = s,
          Ensayo = e,
          N = nrow(data_se)
        )
      )
      
    }
    
  }
  
}

Problemas
table(Problemas$Sujeto)

table(
  sapply(
    NetworkList,
    function(x) gorder(x$graph)
  )
)



summary(
  sapply(
    NetworkList,
    function(x)
      gorder(x$graph)
  )
)


summary(
  sapply(
    NetworkList,
    function(x)
      gsize(x$graph)
  )
)
# ¿Qué AOIs aparecen más frecuentemente?


AOI_frequency <- table(
  unlist(
    lapply(
      NetworkList,
      function(x) V(x$graph)$name
    )
  )
)

sort(
  AOI_frequency,
  decreasing=TRUE
)
