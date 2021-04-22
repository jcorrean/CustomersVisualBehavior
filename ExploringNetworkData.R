# setwd("/home/julan/ownCloud/FederalSergipe/Projetos/Konrad/Curso/Practicas/Booking/Analisis/data/")
library(readr)
GraphMeasures2 <- read_delim("GraphMeasures2.csv", ",",
                             escape_double = FALSE, trim_ws = TRUE)

NuevoPivot2 <- NULL
for (i in c(101,102,103,104,105,106,107,108,109,110,111,112,114,115,116,117,118,119,120,122,123,125,126,127,129,130,131,132,133,134,135,997,999,137,138,139,141,142,143,145)){
  sss <- filter(GraphMeasures2, Sujeto == i)
  for (i in unique(sss$Ensayo)){
    sss2 <- filter(sss, Ensayo == i)
    NuevoPivot2 <- rbind(NuevoPivot2,sss2[1,])
    
  }
}


pave <- right_join(GraphMeasures, NuevoPivot, by = c("Sujeto", "Ensayo"))
pave2 <- pave[c(1:9,17:19)]
names(pave2) <- c("In" ,"Node" ,"Closeness" ,"Betweeness" ,"Sujeto" ,"Posicion" ,"Ensayo" ,"Selection" ,"ChosenImage" ,"IPCC1" ,"IPCC2" ,"IPCC3")

pave3 <- right_join(pave2, NuevoPivot2, by = c("Sujeto", "Ensayo"))
pave4 <- pave3[c(1:12,22:25)]

names(pave4) <- c("In" ,"Node" ,"Closeness" ,"Betweeness" ,
                  "Sujeto" ,"Posicion" ,"Ensayo" ,"Selection",
                  "ChosenImage" ,"IPCC1" ,"IPCC2" ,"IPCC3",
                  "Price","Comments","Review Score",
                  "Location score")

pave2 <- pave4
pave2 <- pave2[complete.cases(pave2), ]


pave2$Posicion <- recode(pave2$Posicion, `1` = "Top", `2` = "Center", `3` = "Bottom")
pave2$Node <- recode(pave2$Node, 
                     "Superior review score" = "Review Score (Top)",
                     "Inferior Dist from center" = "Dist to City Center (Bottom)",
                     "Superior Dist from center" = "Dist to City Center (Top)",
                     "Center review score" = "Review Score (Center)",
                     "Superior Number of comments" = "Number of comments (Top)",
                     "Superior price" = "Price (Top)",
                     "Inferior price" = "Price (Bottom)",
                     "Superior location score" = "Location score (Top)",
                     "Center price" = "Price (Center)",
                     "Center location score" = "Location Score (Center)",
                     "Inferior location score" = "Location Score (Inferior)",
                     "Inferior Number of comments" = "Number of Comments (Bottom)",
                     "Inferior review score"  = "Review Score (Bottom)",
                     "Central star rating" = "Stars Rating (Center)",
                     "Superior image" = "Image (Top)",
                     "Center Number of comments"  = "Number of comments (Center)",
                     "Inferior image" = "Image (Bottom)",
                     "Superior star rating" = "Stars Rating (Top)",
                     "Inferior star rating" = "Stars Rating (Bottom)",
                     "Central Dist from center" = "Dist to City Center (Center)",
                     "Central image"  = "Image (Center)" )

table(pave$Node)
nodes <- data.frame(table(pave2$Node))
names(nodes)[c(1,2)] <- c("Node", "Frequency")



### VISUALIZACION
library(ggplot2)
A <- ggplot(nodes, aes(x = reorder(Node, Frequency), y = Frequency)) + 
  geom_bar(stat = "identity", fill="lightgreen", colour="black") + theme_classic() + 
  theme(axis.text.x = element_text(angle=30, hjust=1, colour = "black"), 
        axis.line = element_line(colour="black"),
        axis.text.y = element_text(colour = "black")) +
  xlab("Areas of Interest in Booking Options") +
  coord_flip()

# NC <- pave %>% select(Node, Betweenness) %>% arrange(desc(Betweenness))

B <- ggplot(pave2, aes(x=reorder(Node, Closeness), y=Closeness)) +
  geom_boxplot(fill="lightgreen", colour="black") +
  coord_flip() + theme_bw() + ylab("Node Closeness Empirical Distribution") + 
  xlab("Areas of Interests") +
  theme(axis.text.x = element_text(colour="black", size=rel(0.9))) +
  theme(axis.text.y = element_text(colour="black", size=rel(0.9))) +
  facet_grid(. ~ Posicion)

library(psych)
Dcloseness <- describe(pave2$Closeness)
row.names(Dcloseness) <- "Closeness"

C <- ggplot(pave2, aes(x=reorder(Node, Betweeness), y=Betweeness)) +
  geom_boxplot(fill="lightgreen", colour="black") +
  coord_flip() + theme_bw() + ylab("Node Betweenness Empirical Distribution") + 
  xlab("Areas of Interests") +
  theme(axis.text.x = element_text(colour="black", size=rel(0.9))) +
  theme(axis.text.y = element_text(colour="black", size=rel(0.9))) +
  facet_grid(. ~ Posicion)

DBetweenness <- describe(pave2$Betweeness)
row.names(DBetweenness) <- "Betweenness"

D <- ggplot(pave2, aes(x=reorder(Node, In), y=In)) +
  geom_boxplot(fill="lightgreen", colour="black") +
  coord_flip() + theme_bw() + ylab("Node In-Degree Centrality Empirical Distribution") + 
  xlab("Areas of Interests") +
  theme(axis.text.x = element_text(colour="black", size=rel(0.9))) +
  theme(axis.text.y = element_text(colour="black", size=rel(0.9))) +
  facet_grid(. ~ Posicion)

DIndegree <- describe(pave2$In)
row.names(DIndegree) <- "In-degree Centrality"

Metrics <- rbind(DBetweenness, Dcloseness, DIndegree)

Metrics <- describe(pave2[c(1,3,4)])

library(ggpubr)
ggarrange(A, B, C, D,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

S1 <- filter(pave2, Sujeto == 101)
S2 <- filter(pave2, Sujeto == 102)
S3 <- filter(pave2, Sujeto == 103)

#library(ggridges)
#ggplot(MedidasGrafos, aes(x = inDegree, fill = ChosenCondition, colour = ChosenCondition)) +  geom_density(alpha = 0.2)
library(ggridges)
E <- ggplot(pave2, aes(x=as.factor(Sujeto), y=In)) + 
  geom_boxplot(fill = "lightgreen") + theme_dark() + coord_flip() + xlab("Participant") +
  facet_wrap(~Posicion) + ylab("In-degree") +
  theme(axis.text.x = element_text(colour="black")) +
  theme(axis.text.y = element_text(colour="black"))

f <- ggplot(pave2, aes(x=as.factor(Sujeto), y=Betweeness)) + 
  geom_boxplot(fill = "lightgreen") + theme_dark() + coord_flip() + xlab("Participant") +
  facet_wrap(~Posicion) + ylab("Betweenness") +
  theme(axis.text.x = element_text(colour="black")) +
  theme(axis.text.y = element_text(colour="black"))

G <- ggplot(pave2, aes(x=as.factor(Sujeto), y=Closeness)) + 
  geom_boxplot(fill = "lightgreen") + theme_dark() + coord_flip() + xlab("Participant") +
  facet_wrap(~Posicion) + ylab("Closeness") +
  theme(axis.text.x = element_text(colour="black")) +
  theme(axis.text.y = element_text(colour="black"))

ggarrange(E, f, G,
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)

#### Some robust models

NuevoPivot3 <- NULL
for (i in c(101,102,103,104,105,106,107,108,109,110,111,112,114,115,116,117,118,119,120,122,123,125,126,127,129,130,131,132,133,134,135,997,999,137,138,139,141,142,143,145)){
  sss <- filter(pave2, Sujeto == i)
  for (i in unique(sss$Ensayo)){
    sss2 <- filter(sss, Ensayo == i)
    NuevoPivot3 <- rbind(NuevoPivot3,sss2[1,])
    
  }
}



library(WRS2)
m1 <- t1way(pave2$In ~ pave2$Posicion)
m2 <- t1way(pave2$Closeness ~ pave2$Posicion)
m3 <- t1way(pave2$Betweeness ~ pave2$Posicion, tr=0.1)
m4 <- t1way(pave2$IPCC3 ~ pave2$Posicion)
lincon(pave2$IPCC3 ~ pave2$Posicion)
describeBy(pave2$IPCC3, group = pave2$Posicion, mat = TRUE)

m1
m2
m3
m4

Metrics <- pave2[c(1, 3, 4, 12)]

pairs.panels(Metrics, smooth = TRUE, density = TRUE, digits = 2, method = "spearman", hist.col = "lightgreen")

MetricsB <- filter(pave2, Posicion == "Bottom") 
MetricsB <- subset(MetricsB, select = c(In, Closeness, Betweeness, IPCC3))

MetricsC <- filter(pave2, Posicion == "Center") 
MetricsC <- subset(MetricsC, select = c(In, Closeness, Betweeness, IPCC3))

MetricsT <- filter(pave2, Posicion == "Top") 
MetricsT <- subset(MetricsT, select = c(In, Closeness, Betweeness, IPCC3))

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
pairs.panels(MetricsB, smooth = TRUE, density = TRUE, digits = 2, method = "spearman", hist.col = "lightgreen", main = "Bottom", stars = TRUE)
pairs.panels(MetricsC, smooth = TRUE, density = TRUE, digits = 2, method = "spearman", hist.col = "lightgreen", main = "Center", stars = TRUE)
pairs.panels(MetricsT, smooth = TRUE, density = TRUE, digits = 2, method = "spearman", hist.col = "lightgreen", main = "Top", stars = TRUE)

m5 <- t1way(pave2$IPCC3 ~ pave2$Price)
m5
plot(pave2$IPCC3 ~ pave2$Price)

m6 <- t1way(pave2$IPCC3 ~ pave2$Comments)
m6
m7 <- t1way(pave2$IPCC3 ~ pave2$`Review Score`)
m7
m8 <- t1way(pave2$IPCC3 ~ pave2$`Location score`)
m8


Effects <- data.frame(Variable = c("Price", "Comments", "Position", "Location Score", "Review Score"),
                      Effect = c(0.49, 0.46, 0.34, 0.19, 0.17), 
                      LCI = c(0.41, 0.39, 0.27, 0.13, 0.11),
                      HCI = c(0.56, 0.54, 0.39, 0.25, 0.24)) 


m9 <- lm(pave2$IPCC3 ~ pave2$Price + pave2$Comments + pave2$Posicion + pave2$`Location score` + pave2$`Review Score`)
summary(m9)

# Checking the assumptions of model 9
library(gvlma)
gvlma(m9)

# An ultimate model was proposed as a robust regression
library(robust)
m10 <- lmRob(pave2$IPCC3 ~ pave2$Price + pave2$Comments + pave2$Posicion + pave2$`Location score` + pave2$`Review Score`)
summary(m10)


ggplot(Effects, aes(x=reorder(Variable, Effect), y=Effect)) + 
  geom_bar(stat="identity", color="black", fill = "lightgreen",
           position=position_dodge()) + theme_bw() +
  geom_errorbar(aes(ymin=LCI, ymax=HCI), width=.2,
                position=position_dodge(.9)) + 
  theme(axis.text.x = element_text(colour="black", size=rel(2))) +
  theme(axis.text.y = element_text(colour="black", size=rel(2))) + 
  xlab("Area of Interest") + theme(axis.title.x=element_text(size=18)) +
  ylab("Effect Size") + 
  theme(axis.title.y=element_text(size=18))

