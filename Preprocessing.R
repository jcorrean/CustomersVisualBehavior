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
    head(areas, 3),
    collapse = "_"
  )
  
  # =====================================
  # RED
  # =====================================
  
  edges <- data.frame(
    from = head(areas, -1),
    to   = tail(areas, -1)
  )
  
  edges_weighted <- edges %>%
    count(from, to, name = "weight")
  
  g <- graph_from_data_frame(
    edges_weighted,
    directed = TRUE
  )
  
  E(g)$weight <- edges_weighted$weight
  
  # =====================================
  # ATRIBUTOS DE NODO
  # =====================================
  
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
  # ENTROPIA DE TRANSICION
  # =====================================
  
  probs <- edges_weighted$weight /
    sum(edges_weighted$weight)
  
  Entropy <- -sum(
    probs * log2(probs)
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
      First3AOIs = First3AOIs,
      
      Entropy = Entropy
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
          
          Choice = unique(data_se$ChosenCondition),
          
          FirstAOI = net$FirstAOI,
          FirstTransition = net$FirstTransition,
          First3AOIs = net$First3AOIs,
          
          Nodes = gorder(net$graph),
          Edges = gsize(net$graph),
          
          Entropy = net$Entropy,
          
          stringsAsFactors = FALSE
        )
      )
      
    }
  }
}

summary(TrajectoryFeatures$Entropy)

aggregate(
  Entropy ~ Choice,
  data = TrajectoryFeatures,
  mean
)

boxplot(
  Entropy ~ Choice,
  data = TrajectoryFeatures
)

anova(
  lm(
    Entropy ~ Choice,
    data = TrajectoryFeatures
  )
)

summary(
  aov(
    Entropy ~ Choice,
    data = TrajectoryFeatures
  )
)


aggregate(
  Nodes ~ Choice,
  data = TrajectoryFeatures,
  mean
)

aggregate(
  Edges ~ Choice,
  data = TrajectoryFeatures,
  mean
)

summary(
  aov(
    Nodes ~ Choice,
    data = TrajectoryFeatures
  )
)

summary(
  aov(
    Edges ~ Choice,
    data = TrajectoryFeatures
  )
)

quantile(
  TrajectoryFeatures$Nodes,
  probs=c(.33,.66)
)

table(
  SearchType,
  Choice
)

tabla3 <- table(
  TrajectoryFeatures$First3AOIs,
  TrajectoryFeatures$Choice
)

chisq.test(tabla3)


library(nnet)

m1 <- multinom(
  Choice ~ Entropy,
  data = TrajectoryFeatures
)

m2 <- multinom(
  Choice ~ FirstAOI + Entropy,
  data = TrajectoryFeatures
)

m3 <- multinom(
  Choice ~ FirstTransition + Entropy,
  data = TrajectoryFeatures
)

m4 <- multinom(
  Choice ~ Nodes + Edges + Entropy,
  data = TrajectoryFeatures
)
