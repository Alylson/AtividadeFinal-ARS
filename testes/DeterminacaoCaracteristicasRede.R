#Análise de redes aplicada a artigos científicos
library(bibliometrix)
library(easyPubMed)
library(igraph)

#Search on PubMed

#Termos de busca comuns
#Affiliation	  [AD]	  First listed author's institutional affiliation and address
#Author	  [AU]	  Author name #Corporate Name as Author	  [CN]	  Corporate names as authors
#Full Author Name	  [FAU]	  Full author name (2002 onward, if available)
#Issue	  [IP]	  Journal issue number #Journal Title	  [TA]	  Title of journal, journal title abbreviation, ISSN
#Language	  [LA]	  Language of the article (not the abstract) #Last Author Name	  [LASTAU]  	  Last personal author in a citation
#MeSH Major Topic	  [MAJR]	  Main topic of an article #MeSH Subheading	  [SH]	  MeSH Subheading
#MeSH Terms	  [MH]	  MeSH Term #Pagination	  [PG]	  First page number of a journal article
#Personal Name as Subject	  [PS]	  Person as the subject of an article, not as an author
#Publication Date	  [PD]	  Date an article was published #Publication Type	  [PT]	  Format of an article (letter, clinical trial, etc) rather than content
#Substance Name	  [NM]	  Chemical and substance names discussed in an article
#Text Word	  [TW]	  Textual fields of PubMed records #Title	  [TI]	  Article title
#Title or Abstract	  [TIAB]	  Words in an article title or an abstract #Volume	  [VI]	  Volume number of a particular journal
#Year	  [DP]	  The year when a journal article was published. #Search a date range, type in   2000:2009[DP]

############################## Dados Site PubMed Web #######################
#Busca no Site PubMed Web
query_string <- "INFORMATION SCIENCE AND (USER STUDY OR USER EXPERIENCE) [TIAB]" #fazer o ajuste da busca conforme necessidade
on_pubmed <- get_pubmed_ids(query_string)
papers <- fetch_pubmed_data(on_pubmed, format = "xml")
papers_list <- articles_to_list(papers)
#Transformar Lista em Data Frame
papers_df<-c()
for (i in 1:length(papers_list)){
  papers_df<-rbind(papers_df, article_to_df(papers_list[[i]], autofill = TRUE, max_chars = 500))
}
#Transforma Data Frame em Rede
edge_list<-cbind(papers_df$pmid, paste(papers_df$lastname, papers_df$firstname, sep = ", "))
colnames(edge_list)<-c("pmid", "AU")
gPub<-graph_from_data_frame(edge_list, directed = FALSE)
#Checando se é um grafo de dois modos e classificadno em tipo 1 e 2
bipartite_mapping(gPub)$res
V(gPub)$type<-as.logical((1:vcount(gPub)<=vcount(gPub)-length(unique(edge_list[,2]))))
#verificar o tamanho das redes transformadas antes de realizar a transforma??o
bipartite_projection_size(gPub)
#Criar as redes de um modo com base na rede de dois modos
gPubAU<-bipartite_projection(gPub)[[1]]
gPubID<-bipartite_projection(gPub)[[2]]
plot (gPub)

############################## Dados Web of Science ou Scopus#######################
library(bibliometrix)
library(easyPubMed)
library(igraph)
## Retorna o diretório corrente
getwd() 
##Seta o diretório
setwd("NetworkAnalysis")
#papers<-readFiles("savedrecs.txt")
papers<-readFiles("UserStudy&Experience_Corrigido.txt")
papers_df<-convert2df(papers, dbsource = "isi", format = "plaintext") #definir formato e fonte
papers_matrix <- as.matrix(biblioNetwork(papers_df, analysis = "collaboration", network = "authors", sep = ";"))
#gPubAU<-graph_from_adjacency_matrix(papers_matrix)
my_graph <-graph_from_adjacency_matrix(papers_matrix)

############################## Dados da rede VOS/Gephi no formato Graphml#######################
library(igraph)
## Retorna o diretório corrente
getwd() 
##Seta o diretório para o diretório onde está p arquivo graphml.
setwd("NetworkAnalysis") 
##Lista os arquivos do diretório
dir()
#Lê o grafo salvo do Gephi no formato graphml. O arquivo deve estar no diretório setado ant
my_graph <-read.graph("UserStudy&Experience_Co-CitationAuthorOnlyFirst_418_Gephi.graphml", format("graphml"))
##Mostra a descrição do grafo
str(my_graph)

###############################4.2.1 Grau dos vértices (Vertex Degree)
# Grau = soma grau de entrada + grau de saída
my_graph.degree <- degree(my_graph)
my_graph.degree.summary <-summary(my_graph.degree)
my_graph.degree.sd <-sd(my_graph.degree)
#hist(my_graph.degree,col="lightblue", ylim=c(0, 100),xlim=c(0, 300),xlab="Grau dos vértices", ylab="Frequência", main="Frequência de grau dos vértices", axes="TRUE")
hist(my_graph.degree,col="lightblue", xlab="Grau dos vértices", ylab="Frequência", main="Frequência de grau dos vértices", axes="TRUE")
legend("topright", c(paste("Mínimo =", my_graph.degree.summary[1]), 
                     paste("Máximo=", my_graph.degree.summary[6]), 
                     paste("Média=", my_graph.degree.summary[4]),
                     paste("Mediana=", my_graph.degree.summary[3]),
                     #paste("1º quartil=", my_graph.degree.summary[2]),
                     #paste("3º quartil=", my_graph.degree.summary[5])),
                     paste("Desvio Padrão=", round(my_graph.degree.sd[1],2))),
                     pch = 1, title = "Grau")

#4.1 Grau de entrada
my_graph.indegree <- degree(my_graph, mode = c("in"))
my_graph.indegree.summary <-summary(my_graph.indegree)
my_graph.indegree.sd <-sd(my_graph.indegree)
hist(my_graph.indegree,col="lightblue", xlab="Grau de entrada dos vértices", ylab="Frequência", main="Frequência do grau de entrada dos vértices", axes="TRUE")
legend("topright", c(paste("Mínimo =", my_graph.indegree.summary[1]), 
                     paste("Máximo=", my_graph.indegree.summary[6]), 
                     paste("Média=", my_graph.indegree.summary[4]),
                     paste("Mediana=", my_graph.indegree.summary[3]),
                     #paste("1º quartil=", my_graph.indegree.summary[2]),
                     #paste("3º quartil=", my_graph.indegree.summary[5])),
                     paste("Desvio Padrão=", round(my_graph.indegree.sd[1],2))),
       pch = 1, title = "Grau entrada")

#4.1 Grau de saída
my_graph.outdegree <- degree(my_graph, mode = c("out"))
#mode = c("all", "out", "in", "total")
my_graph.outdegree.summary <-summary(my_graph.outdegree)
my_graph.outdegree.sd <-sd(my_graph.outdegree)
#hist(my_graph.outdegree,col="lightblue", ylim=c(0, 100),xlim=c(0, 300),xlab="Grau de saída dos vértices", ylab="Frequência", main="Frequência de grau de saída dos vértices", axes="TRUE")
hist(my_graph.outdegree,col="lightblue", xlab="Grau de saída dos vértices", ylab="Frequência", main="Frequência do grau de saída dos vértices", axes="TRUE")
legend("topright", c(paste("Mínimo =", my_graph.outdegree.summary[1]), 
                     paste("Máximo=", my_graph.outdegree.summary[6]), 
                     paste("Média=", my_graph.outdegree.summary[4]),
                     paste("Mediana=", my_graph.outdegree.summary[3]),
                     #paste("1º quartil=", my_graph.outdegree.summary[2]),
                     #paste("3º quartil=", my_graph.outdegree.summary[5])),
                     paste("Desvio Padrão=", round(my_graph.outdegree.sd[1],2))),
pch = 1, title = "Grau saída")

#4.2 Força dos vértices
#Força = força de entrada + força de saída
my_graph.strengh <- graph.strength(my_graph)
my_graph.strengh.summary<-summary(my_graph.strengh)
my_graph.strengh.sd=sd(my_graph.strengh)
#hist(my_graph.strengh, col="pink",ylim=c(0, 350),xlim=c(0, 7000),xlab="Força dos vértices", ylab="Frequência", main="Frequência ponderada da força dos vértices")
hist(my_graph.strengh, col="pink",xlab="Força dos vértices", ylab="Frequência", main="Frequência ponderada da força dos vértices")
legend("topright", c(paste("Mínimo =", my_graph.strengh.summary[1]), 
                     paste("Máximo=", my_graph.strengh.summary[6]), 
                     paste("Média=", my_graph.strengh.summary[4]),
                     paste("Mediana=", my_graph.strengh.summary[3]),
                     #paste("1º quartil=", my_graph.strengh.summary[2]),
                     #paste("3º quartil=", my_graph.strengh.summary[5])),
                     paste("Desvio Padrão=", round(my_graph.strengh.sd[1],2))),
       pch = 1, title = "Força")

#Força de entrada
my_graph.instrengh <- graph.strength(my_graph, mode =c("in"))
my_graph.instrengh.summary<-summary(my_graph.instrengh)
my_graph.instrengh.sd=sd(my_graph.instrengh)
hist(my_graph.instrengh, col="pink",xlab="Força de entrada dos vértices", ylab="Frequência", main="Frequência da força do grau de entrada dos vértices")
legend("topright", c(paste("Mínimo =", my_graph.instrengh.summary[1]), 
                     paste("Máximo=", my_graph.instrengh.summary[6]), 
                     paste("Média=", my_graph.instrengh.summary[4]),
                     paste("Mediana=", my_graph.instrengh.summary[3]),
                     #paste("1º quartil=", my_graph.instrengh.summary[2]),
                     #paste("3º quartil=", my_graph.instrengh.summary[5])),
                     paste("Desvio Padrão=", round(my_graph.instrengh.sd[1],2))),
       pch = 1, title = "Força entrada")

#Força de saída
my_graph.outstrengh <- graph.strength(my_graph, mode =c("out"))
my_graph.outstrengh.summary<-summary(my_graph.outstrengh)
my_graph.outstrengh.sd=sd(my_graph.outstrengh)
hist(my_graph.outstrengh, col="pink",xlab="Força de saída dos vértices", ylab="Frequência", main="Frequência da força do grau de saída dos vértices")
legend("topright", c(paste("Mínimo =", my_graph.outstrengh.summary[1]), 
                     paste("Máximo=", my_graph.outstrengh.summary[6]), 
                     paste("Média=", my_graph.outstrengh.summary[4]),
                     paste("Mediana=", my_graph.outstrengh.summary[3]),
                     #paste("1º quartil=", my_graph.outstrengh.summary[2]),
                     #paste("3º quartil=", my_graph.outstrengh.summary[5])),
                     paste("Desvio Padrão=", round(my_graph.outstrengh.sd[1],2))),
       pch = 1, title = "Força saída")
