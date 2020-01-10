install.packages('igraph')

library( bipartite)
library(xlsx)
library(asnipe)
library(assortnet)
library(popgraph)
library(ggplot2)
library(ggmap)
library(rnetcarto)
library(ecodist)
library(igraphdata)
library(statnet)
library( RColorBrewer)
library(igraph)
library(tidygraph)




m  <- read.table(row.names=1, header=TRUE, fill = TRUE,  text=(
               "azarasSpinetail BayCrownedBrushFinch	BlackFlowerpiercer BlackishTapaculo	Buff-breastedMountainTanager ChiguancoTrush	CineriousConeBill	GreatThrush	Hooded-Siskin	MaskedFlowerpiercer	Red-BreastedCotinga	RufousCollaredSparrow	Scarlet-belliedmountainTanager StripeHeadedBrushFinch TuftedTitTyrant	TurquoisJay	WhiteCrestedElania	YellowBreastedBrushFinch
                azarasSpinetail 0	0	1	0	1	0	0	1	1	2	0	1	3	1	0	1	0	3
                BayCrownedBrushFinch 0 0 0	0	0	0	1	0	0	1	0	0	0	0	0	0	0	1
                BlackFlowerpiercer 1	0	0	0	1	1	1	6	0	5	0	2	7	1	1	1	2	6
                BlackishTapaculo 0	0	0	0	2	1	0	1	0	1	1	0	2	0	0	1	0	1
                BuffbreastedMountainTanager	1	0	1	2	0	1	0	7	1	7	3	2	11 0	0	2	1	7   
                ChinguancoTrush	0	0	1	1	1	0	0	2	0	2	1	1	2 0	1	0	0	3
                CineriousConeBill 0	1	1	0	0	0	0	2	0	2	0	0	1	0	1	1	1	2
                GreatThrush	1	0	6	1	7	2	2	0	1	9	0	6	15	0	2	4	2	12
                Hooded-Siskin	1	0	0	0	1	0	0	1	0	0	0	0	1	0	0	1	0	1
                MaskedFlowerpiercer	2	1	5	1	7	2	2	9	0	0	4	5	18	1	1	2	3	14
                Red-BreastedCotinga	0	0	0	1	3	1	0	0	0	4	0	0	6	0	1	0	0	5
                RufousCollaredSparrow	1	0	2	0	2	1	0	6	0	5	0	0	7	0	1	0	0	5
                Scarlet-belliedmountainTanager 3	0	7	2	11	2	1	15	1	18	6	7	0	1	2	5	3	17
                StripeHeadedBrushFinch 1	0	1	0	0	0	0	0	0	1	0	0	1	0	0	0	0	1
                TuftedTitTyrant	0	0	1	0	0	1	1	2	0	1	1	1	2	0	0	0	0	2
                TurquoisJay	1	0	1	1	2	0	1	4	1	2	0	0	5	0	0	0	1	3
                WhiteCrestedElania 0	0	2	0	1	0	1	2	0	3	0	3	3	0	0	1	0	2
                YellowBreastedBrushFinch 3	1	6	1	7	3	2	12	1	14	5	5	17	1	2	3	2	0"))
m 
ma <- as.matrix(m)
ma
g=graph_from_adjacency_matrix(ma, mode="undirected", weighted=T)
g
plot(g)
plot(g, layout=layout_in_circle(g))
plot(g, edge.width=E(g)$weight, layout=layout_in_circle(g))

getwd()

setwd("/Users/fsajedi/Desktop") 
attrib<-for.net.data  <- read.xlsx("Counts.xlsx", sheetName = "Sheet1", header = TRUE)
rownames(ma)
V(g)$count=attrib[match(rownames(ma), attrib$Name), "Count"]
V(g)$count


as.numeric(V(g)$name)

V(g)$name


V(g)$color=c("gold","blue", "pink", "red", "green", "purple", "violet", "yellow", 
             "navy", "orange","black","grey", "coral","magenta" , "slateblue", "aquamarine", "maroon", "darkred")

help("igraph")

d =weighteddegree(g, v = V(g), mode ="all",
       loops = TRUE, normalized = FALSE)
d
x = d/V(g)$count
V(g)$count
x
tkplot(g,edge.curved=TRUE, vertex.label="",edge.width=E(g)$weight,vertex.size=V(g)$count)
legend("topleft", legend=c("azarasSpinetail",
                           "BayCrownedBrushFinch",
                           "BlackFlowerpiercer",
                          "BlackishTapaculo",
                           "BuffbreastedMountainTanager",
                           "ChinguancoTrush",
                           "CineriousConeBill",
                          "GreatThrush",
                          "Hooded-Siskin",
                          "MaskedFlowerpiercer",
                           "Red-BreastedCotinga",
                           "RufousCollaredSparrow",
                           "Scarlet-belliedmountainTanager",
                           "StripeHeadedBrushFinch",
                           "TuftedTitTyrant",
                           "TurquoisJay",
                           "WhiteCrestedElania",
                           "YellowBreastedBrushFinch"), pch=21, pt.bg=c("gold","blue", "pink", "red", "green", "purple", "violet", "yellow", 
                                                                        "navy", "orange","black","grey", "coral","magenta" , "slateblue", "aquamarine", "maroon", "darkred"))
networklevel(g)
centrality_pagerank(weights = E(g), directed = FALSE, damping = 0.85,
                    personalized = NULL)
eigen_centrality(g, directed = FALSE, scale = TRUE, weights = NULL,
                 options = arpack_defaults)
