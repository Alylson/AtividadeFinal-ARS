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

#Busca no Site PubMed Web
query_string <- "Zika Virus[TIAB]" #fazer o ajuste da busca conforme necessidade
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

#Dados Web of Science ou Scopus
papers<-readFiles("savedrecs.txt")
papers_df<-convert2df(papers, dbsource = "isi", format = "plaintext") #definir formato e fonte
papers_matrix <- as.matrix(biblioNetwork(papers_df, analysis = "collaboration", network = "authors", sep = ";"))
gPubAU<-graph_from_adjacency_matrix(papers_matrix)
