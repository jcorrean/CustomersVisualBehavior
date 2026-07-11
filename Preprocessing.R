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
  
  # =====================================
  # ORDEN TEMPORAL
  # =====================================
  
  df <- df[order(df$Secuencia), ]
  
  areas <- df$Area
  
  if(length(areas) < 2){
    return(NULL)
  }
  
  # =====================================
  # VARIABLES SECUENCIALES
  # =====================================
  
  FirstAOI <- as.character(areas[1])
  
  FirstTransition <- paste0(
    areas[1],
    "_",
    areas[2]
  )
  
  First3AOIs <- paste(
    head(areas,3),
    collapse = "_"
  )
  
  # =====================================
  # RED
  # =====================================
  
  edges <- data.frame(
    from = head(areas,-1),
    to   = tail(areas,-1)
  )
  
  edges_weighted <- edges %>%
    count(from,to,name="weight")
  
  g <- graph_from_data_frame(
    edges_weighted,
    directed = TRUE
  )
  
  E(g)$weight <- edges_weighted$weight
  
  node_freq <- table(areas)
  
  V(g)$visits <- as.numeric(
    node_freq[
      match(
        V(g)$name,
        names(node_freq)
      )
    ]
  )
  
  # =====================================
  # RESULTADO
  # =====================================
  
  return(
    list(
      graph = g,
      edges = edges,
      edges_weighted = edges_weighted,
      node_freq = node_freq,
      
      FirstAOI = FirstAOI,
      FirstTransition = FirstTransition,
      First3AOIs = First3AOIs
    )
  )
}

# =====================================================
# CONSTRUIR TODAS LAS REDES
# =====================================================

NetworkList <- list()

TrajectoryFeatures <- data.frame()

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
      
      # ===================================
      # TABLA DE CARACTERISTICAS
      # ===================================
      
      TrajectoryFeatures <- rbind(
        TrajectoryFeatures,
        data.frame(
          Sujeto = s,
          Ensayo = e,
          
          FirstAOI = net$FirstAOI,
          FirstTransition = net$FirstTransition,
          First3AOIs = net$First3AOIs,
          
          Nodes = gorder(net$graph),
          Edges = gsize(net$graph),
          
          stringsAsFactors = FALSE
        )
      )
      
    }
  }
}
dim(TrajectoryFeatures)
length(unique(TrajectoryFeatures$Sujeto))
table(TrajectoryFeatures$Sujeto)

head(TrajectoryFeatures)
dim(TrajectoryFeatures)
length(unique(TrajectoryFeatures$Sujeto))

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

# ¿Qué transiciones aparecen más frecuentemente?

all_edges <- do.call(
  rbind,
  lapply(
    NetworkList,
    function(x)
      x$edges
  )
)

table(
  all_edges$from,
  all_edges$to
)


all_edges |>
  count(from,to) |>
  arrange(desc(n))


all_edges <- do.call(
  rbind,
  lapply(
    NetworkList,
    function(x)
      x$edges
  )
)

TransitionMatrix <- all_edges |>
  count(from,to)
