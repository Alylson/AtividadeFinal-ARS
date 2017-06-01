#Visualização de rede e alteração de atributos visuais -a apenas web of science#
library(bibliometrix)
library(easyPubMed)
library(sna)
library(shiny)
library(igraph)

arq_wos<-""
setwd("C:/Users/Saraiva/Desktop/TESTES")
arq_wos <-readFiles("UserStudy&Experience_Resumido.txt")

arq_wosf <-convert2df(arq_wos, dbsource = "isi", format = "plaintext")

arq_wos_para_matriz <- as.matrix(biblioNetwork(arq_wosf, analysis = "collaboration", network = "authors", sep = ";"))

rede_wos <-graph_from_adjacency_matrix((arq_wos_para_matriz),"undirected")

#Geracao de metricas da rede para montagem do painel#

#quantidade de linhas
num_linhas<-E(rede_wos)
#quantidade de nós
num_nos<-vcount(rede_wos)
#coeficiente de Assortatividade #
coef_assort<-assortativity.degree(rede_wos)
#coeficiente de Transitividade#
coef_transit<-transitivity(rede_wos)
# Diâmetro da Rede#
diam_rede<-diameter(rede_wos)
# Distância média da rede#
dist_media_rede<- average.path.length(rede_wos)
#retira loops#
rede_wos<- simplify(rede_wos)
plot(rede_wos, layout=layout.fruchterman.reingold, vertex.size = 0.1, edge.arrow.size = 0.1, edge.color = "green")
#define modularidade da rede#
walk_rede <- walktrap.community(rede_wos, modularity=TRUE)
memb_rede <- cutat(walk_rede, steps=2)
modularidade<-modularity(rede_wos,memb_rede)
#maximo de elementos conectados#
components(rede_wos)

# plota componente gigante#
comp_gigante <- decompose.graph(rede_wos)
largest <- which.max(sapply(comp_gigante, vcount))
plot(comp_gigante[[largest]], layout=layout.fruchterman.reingold)

#plota rede com os nós dimensionados de acordo com seu grau
grau<-degree(rede_wos, mode="all")  
V(rede_wos)$size<-grau*4 
plot(rede_wos) 

#plotar rede com destaque para o componente gigante
library(Cairo)
library(igraph)
comp_gigante <- fastgreedy.community(simplify(as.undirected(rede_wos)))
Q <- round(max(fcs$modularity), 3)
l <- layout.fruchterman.reingold(rede_wos)

plot(rede_wos, layout=l, vertex.size=3, vertex.label=NA, vertex.color="#ff000033",
     vertex.frame.color="#ff000033", edge.color="#55555533", edge.arrow.size=0.3,
     rescale=FALSE, xlim=range(l[,1]), ylim=range(l[,2]),
     main=paste(sep="", "Fast greedy community detection,\nQ=", Q))




# Centralidade de grau#
#centr_grau<-
# Centralidade de Intermediação#
#centr_inter<-
# Centralidade de autovetor#
#centr_autvetor<-#


