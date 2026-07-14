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


#########################################################
# RQ2
#########################################################

tabla_firstAOI <- table(
  TrajectoryFeatures$FirstAOI,
  TrajectoryFeatures$Choice
)

test_firstAOI <- chisq.test(
  tabla_firstAOI
)

tabla_firstTransition <- table(
  TrajectoryFeatures$FirstTransition,
  TrajectoryFeatures$Choice
)

test_firstTransition <- chisq.test(
  tabla_firstTransition
)

tabla_first3 <- table(
  TrajectoryFeatures$First3AOIs,
  TrajectoryFeatures$Choice
)

test_first3 <- chisq.test(
  tabla_first3
)

anova_nodes <- summary(
  aov(
    Nodes ~ Choice,
    data = TrajectoryFeatures
  )
)

anova_edges <- summary(
  aov(
    Edges ~ Choice,
    data = TrajectoryFeatures
  )
)

anova_entropy <- summary(
  aov(
    Entropy ~ Choice,
    data = TrajectoryFeatures
  )
)

multiverse_results <- data.frame(
  Universe = c(
    "FirstAOI",
    "FirstTransition",
    "First3AOIs",
    "Nodes",
    "Edges",
    "Entropy"
  ),
  Test = c(
    "Chi-square",
    "Chi-square",
    "Chi-square",
    "ANOVA",
    "ANOVA",
    "ANOVA"
  ),
  Pvalue = c(
    test_firstAOI$p.value,
    test_firstTransition$p.value,
    test_first3$p.value,
    anova_nodes[[1]]["Choice","Pr(>F)"],
    anova_edges[[1]]["Choice","Pr(>F)"],
    anova_entropy[[1]]["Choice","Pr(>F)"]
  )
)

knitr::kable(
  multiverse_results,
  digits = 4,
  caption = "Multiverse analysis of visual exploration."
)

library(nnet)
library(pscl)
library(knitr)
library(dplyr)

# Asegurar que es factor
TrajectoryFeatures$Choice <- as.factor(
  TrajectoryFeatures$Choice
)

#################################################
# MODELS
#################################################

m1 <- multinom(
  Choice ~ Entropy,
  data = TrajectoryFeatures,
  trace = FALSE
)

m2 <- multinom(
  Choice ~ Entropy + FirstAOI,
  data = TrajectoryFeatures,
  trace = FALSE
)

m3 <- multinom(
  Choice ~ Entropy + FirstTransition,
  data = TrajectoryFeatures,
  trace = FALSE
)

m4 <- multinom(
  Choice ~ Nodes + Edges + Entropy,
  data = TrajectoryFeatures,
  trace = FALSE
)

#################################################
# TABLE
#################################################

results_models <- data.frame(
  Model = c(
    "M1",
    "M2",
    "M3",
    "M4"
  ),
  
  Predictors = c(
    "Entropy",
    "FirstAOI + Entropy",
    "FirstTransition + Entropy",
    "Nodes + Edges + Entropy"
  ),
  
  McFadden_R2 = c(
    pR2(m1)["McFadden"],
    pR2(m2)["McFadden"],
    pR2(m3)["McFadden"],
    pR2(m4)["McFadden"]
  ),
  
  AIC = c(
    AIC(m1),
    AIC(m2),
    AIC(m3),
    AIC(m4)
  )
)

results_models <- results_models %>%
  mutate(
    McFadden_R2 = round(McFadden_R2, 4),
    AIC = round(AIC, 1)
  )

kable(
  results_models,
  caption = "Comparative performance of multinomial models predicting booking decisions from alternative representations of visual exploration."
)
