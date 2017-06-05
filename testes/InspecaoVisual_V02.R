#Visualização de rede e alteração de atributos visuais -a apenas web of science#
library(bibliometrix)
library(easyPubMed)
library(igraph)


# TRANSFORMA O TEXTO DO ARQUIVO EM DATAFRAME (WEB OF SCIENCE OU SCOPUS)
TransformaTextoEmDataframe = function(){
  my_dbsource = "isi" 
  my_format = "plaintext" 
  my_papers_df <<-convert2df(my_lines_papers, dbsource=my_dbsource, format=my_format) #definir formato e font
  my_papers_df <<- metaTagExtraction(my_papers_df, Field = "AU_CO", sep = ";")
  my_papers_df <<- metaTagExtraction(my_papers_df, Field = "CR_AU", sep = ";")
  my_papers_df <<- metaTagExtraction(my_papers_df, Field = "CR_SO", sep = ";")
  my_papers_df <<- metaTagExtraction(my_papers_df, Field = "AU_UN", sep = ";")
}
LeArquivoTexto = function(my_file){
  my_lines_papers<<-readFiles(my_file)
}

my_file = "UserStudy&Experience_Corrigido.txt"
LeArquivoTexto (my_file)
my_file.info = paste ("Arquivo:", c(my_file), "Num. linhas:",length(my_lines_papers))
my_file.info

TransformaTextoEmDataframe()
my_netDegree <- 50
my_analysis = "co-citation"
my_network = "authors"
my_NetMatrix=""
my_NetMatrix <- biblioNetwork(my_papers_df, analysis = my_analysis, network = my_network, sep = ";")
diag <- Matrix::diag 
my_NetMatrix <- my_NetMatrix[diag(my_NetMatrix) >= my_netDegree,diag(my_NetMatrix) >= my_netDegree]
#diag(my_NetMatrix) <- 0
my_graph <<- graph.adjacency(my_NetMatrix,mode = "undirected")


#quantidade de linhas
num_linhas<-E(my_graph)
num_linhas

#quantidade de nós
num_nos<-vcount(my_graph)
num_nos

#coeficiente de Assortatividade 
coef_assort<-assortativity.degree(my_graph)
coef_assort

#coeficiente de Transitividade
coef_transit<-transitivity(my_graph)
coef_transit

# Diâmetro da rede
diam_rede<-diameter(my_graph)
diam_rede

# Distância média da rede
dist_media_rede<- average.path.length(my_graph)

#retira loops
my_graph<- simplify(my_graph)

#calcula a modularidade da rede
walk_rede <- walktrap.community(my_graph, modularity=TRUE)
memb_rede <- cutat(walk_rede, steps=2)
modularidade<-modularity(my_graph,memb_rede)
modularidade

#maximo de elementos conectados
count_components(my_graph)
is_connected(my_graph)

is_directed(my_graph)

#plota o grafo
my_graph<-simplify(my_graph)
is_simple(my_graph)
plot(my_graph,edge.arrow.size=0.1, edge.color = "grey",edge.curved= 0,
     layout = layout.fruchterman.reingold,
     vertex.color="orange",vertex.frame.color = 'black',
     #vertex.label="",
     vertex.label.dist = 0,
     vertex.size = 5,
     vertex.label.color = 'black',
     vertex.label.font = 15, vertex.label = V(my_graph)$name, vertex.label.cex = 0.5)


# plota componente gigante#
comp_gigante <- decompose.graph(my_graph)
largest <- which.max(sapply(comp_gigante, vcount))
plot.new()
plot(comp_gigante[[largest]], edge.arrow.size=0.1,edge.color="grey",layout=layout.fruchterman.reingold,
     vertex.color="orange",vertex.frame.color = 'black',
     #vertex.label="",
     vertex.label.dist = 0,
     vertex.size = 5,
     vertex.label.color = 'black',
     vertex.label.font = 15, vertex.label = V(my_graph)$name, vertex.label.cex = 0.5)

#plota rede com os nós dimensionados de acordo com seu grau
grau<-degree(my_graph, mode="all")  
V(my_graph)$size<-grau*0.6
plot.new()
plot(my_graph,edge.arrow.size=0.1, edge.color="grey",layout=layout.fruchterman.reingold,
     vertex.color="orange",vertex.frame.color = 'blue',
     vertex.label.dist = 0,
     vertex.label.color = 'black',
     vertex.label.font = 1, vertex.label = V(my_graph)$name, vertex.label.cex = 0.7)

#plotar rede com destaque para o componente gigante 
library(Cairo)
library(igraph)
comp_gigante <- fastgreedy.community(simplify(as.undirected(my_graph)))
Q <- round(max(comp_gigante$modularity), 2)
l <- layout.fruchterman.reingold(my_graph)
plot.new()
plot(my_graph, layout=layout.fruchterman.reingold, 
     vertex.size=5, vertex.color="orange",
     vertex.frame.color="black", edge.color="grey",
     edge.arrow.size=0.1,vertex.label.font = 1)
    

## precisa posicionar manualmente, pra nao sobrepor...
g2<-comp_gigante
l2 <- l[ which(comp_gigante$membership==comp_gigante$membership[1]), ]
plot(g2, layout=l2, vertex.size=50, vertex.label=V(g2)$name,
     vertex.size=2, vertex.color="orange",
     vertex.frame.color="black", edge.color="grey",
     edge.arrow.size=0.1,vertex.label.font = 1,
     rescale=FALSE)

#### plota os subgrupos com cores diferentes
SCC <- clusters(my_graph, mode="strong")  
V(my_graph)$color <- rainbow(SCC$no)[SCC$membership]
plot(my_graph, mark.groups = split(1:vcount(my_graph), SCC$membership),
     vertex.size=0.5,  vertex.frame.color="black", edge.color="grey",
     edge.arrow.size=0.1, vertex.label.font= 1)







