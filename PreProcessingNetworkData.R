setwd("/home/juan/Documents/PaperConAndreinaYJulian/nuevo")
library(readr)
Secuencia <- read_delim("Secuencia.csv", ";",
                        escape_double = FALSE, trim_ws = TRUE)

Secuencia$Area <- factor(Secuencia$Area)
levels(Secuencia$Area) <- c("Superior image", "Central image", "Inferior image", "Superior star rating", "Central star rating", "Inferior star rating", "Superior Dist from center", "Central Dist from center", "Inferior Dist from center", "Superior review score", "Center review score", "Inferior review score", "Superior review score", "Center review score", "Inferior review score", "Superior Number of comments", "Center Number of comments", "Inferior Number of comments", "Superior location score", "Center location score", "Inferior location score", "Superior price", "Center price", "Inferior price", "Superior block", "Center block", "Inferior block")

library(dplyr)
library(igraph)

GraphMeasures <- NULL
for (i in c(101,102,103,104,105,106,107,108,109,110,111,112,114,115,116,117,118,119,120,122,123,125,126,127,129,130,131,132,133,134,135,997,999,137,138,139,141,142,143,145)){
  
  s101 <- filter(Secuencia, Sujeto == i)
  
  unique(s101$Ensayo)
  # Para el sujeto 101 en la posicion ==1, deben sacarse los datos 
  # de los siguientes ensayos: 5, 6, 7, 8 y 19
  # Luego habria que repetir lo mismo para posicion == 2
  # s101P2 <- filter(s101, Posicion == 2)
  # unique(s101P2$Ensayo)
  # Y para posicion == 3
  # s101P3 <- filter(s101, Posicion == 3)
  # unique(s101P3$Ensayo)
  
  
  # Aqui se inicia, entonces, con el primer ensayo
  # para el sujeto 101 en la posicion 1
  for (j in unique(s101$Ensayo)){
    s101P1E5 <- filter(s101, Ensayo == j)
    Grafo <- as.data.frame(matrix(t(s101P1E5$Area), ncol=2, 
                                  byrow=TRUE), stringsAsFactors=FALSE)
    Tempos <- as.data.frame(matrix(t(s101P1E5$Tempos), ncol=2, 
                                   byrow=TRUE), stringsAsFactors=FALSE)
    
    
    df.g <- graph.data.frame(Grafo, directed = TRUE, )
    node.size <- setNames(c(Tempos$V1), c(Tempos$V2))
    deg <- degree(df.g, mode = "in")
    coords <- layout_(df.g, randomly())
    plot(df.g, 
         vertex.size =deg*11,
         vertex.label.degree=2,
         vertex.label.dist=1.2,
         vertex.label.cex=1,
         layout = coords)
    
    
    I <- data.frame(In = degree(df.g, mode = 'in'))
    I$Node <- row.names(I)
    
    C <- data.frame(Closeness = closeness(df.g))
    
    B <- data.frame(Betweeness = betweenness(df.g))
    
    
    SP <- list(I, C, B)
    SP <- do.call(cbind.data.frame, SP)
    SP$Sujeto <- unique(s101P1E5$Sujeto)
    SP$Posicion <- unique(s101P1E5$Posicion)
    SP$Ensayo <- unique(s101P1E5$Ensayo)
    SP$Selection <- unique(s101P1E5$ChosenCondition)
    SP$ChosenImage <- unique(s101P1E5$NombreSeleccionado) 
    GraphMeasures <- rbind(GraphMeasures, SP)
  }
}

library(ggplot2)
library(cowplot)

GraphMeasures <- GraphMeasures[complete.cases(GraphMeasures), ]



# Estimación del indice IPCC
NuevoPivot <- NULL
for (i in c(101,102,103,104,105,106,107,108,109,110,111,112,114,115,116,117,118,119,120,122,123,125,126,127,129,130,131,132,133,134,135,997,999,137,138,139,141,142,143,145)){
  sss <- filter(GraphMeasures, Sujeto == i)
  for (i in unique(sss$Ensayo)){
    sss2 <- filter(sss, Ensayo == i)
    NuevoPivot <- rbind(NuevoPivot,sss2[1,])
    
  }
}

library(pivottabler)
qhpvt(filter(NuevoPivot, (Sujeto == 999 )), "Posicion", "Selection", "n()")
InDegree <- ggplot(data=GraphMeasures, aes(x=Posicion, y=In, fill=factor(Selection)))+ stat_summary(fun.y = mean, geom = "bar", position="dodge") + stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.3,position=position_dodge(.9)) + labs(x = "Screen position", y = "In degree") 

# Tablas de contingencia para estimar IPCC
NuevoPivot["IPCC1"] <- 0
NuevoPivot["IPCC2"] <- 0

for (i in c(101,102,103,104,105,106,107,108,109,110,111,112,114,115,116,117,118,119,120,122,123,125,126,127,129,130,131,132,133,134,135,997,999,137,138,139,141,142,143,145)){
  
  
  Fila <- aggregate(Selection~Posicion, data = filter(NuevoPivot, (Sujeto == i )),FUN = function(x){NROW(x)})
  Columna <- aggregate(Posicion~Selection, data = filter(NuevoPivot, (Sujeto == i )),FUN = function(x){NROW(x)})
  
  Columna$Posicion <- Columna$Posicion/sum(Columna$Posicion)
  Fila$Selection <- Fila$Selection/sum(Fila$Selection)
  
  NuevoPivot$IPCC1[which(NuevoPivot$Sujeto==i & NuevoPivot$Posicion==1)] <- Fila$Selection[1]
  NuevoPivot$IPCC1[which(NuevoPivot$Sujeto==i & NuevoPivot$Posicion==2)] <- Fila$Selection[2]
  NuevoPivot$IPCC1[which(NuevoPivot$Sujeto==i & NuevoPivot$Posicion==3)] <- Fila$Selection[3]
  NuevoPivot$IPCC2[which(NuevoPivot$Sujeto==i & NuevoPivot$Selection=='Comentarios')] <- Columna$Posicion[1]
  NuevoPivot$IPCC2[which(NuevoPivot$Sujeto==i & NuevoPivot$Selection=='Precio')] <- Columna$Posicion[2]
  NuevoPivot$IPCC2[which(NuevoPivot$Sujeto==i & NuevoPivot$Selection=='Real')] <- Columna$Posicion[3]
  
}

NuevoPivot["IPPC3"] <- NuevoPivot$IPCC1*NuevoPivot$IPCC2