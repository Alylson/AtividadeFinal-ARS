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

# clear all variables
rm(list = ls(all.names = TRUE))

#####Lê o arquivo de artigos científicos salvos da Web of Science ou Scopus
my_file = "UserStudy&Experience_Corrigido.txt"
#my_name_file = "savedrecs.txt"
my_lines_papers<-readFiles(my_file)
my_file.info = paste ("Arquivo:", c(my_file), "Num. linhas:",length(my_lines_papers))
my_file.info

##### Transforma o arquivo texto em dataframe.
##### Escolher o dbsource ("isi" ou "scopus")
##### Escolher o format do texto ("plaintext" ou  "bibtex")
my_dbsource = "isi" 
my_format = "plaintext" 
my_papers_df<-convert2df(my_lines_papers, dbsource=my_dbsource, format=my_format) #definir formato e font
# Extraindo informações adicionais que não são padrão da Web Of Science e Scopus.
# As informações são extraídas usando a função metaTagExtraction
# Authors' countries (Field = "AU_CO");
my_papers_df <- metaTagExtraction(my_papers_df, Field = "AU_CO", sep = ";")
# First author of each cited reference (Field = "CR_AU")
my_papers_df <- metaTagExtraction(my_papers_df, Field = "CR_AU", sep = ";")
# Publication source of each cited reference (Field = "CR_SO")
my_papers_df <- metaTagExtraction(my_papers_df, Field = "CR_SO", sep = ";")
# and Authors' affiliations (Field = "AU_UN")
my_papers_df <- metaTagExtraction(my_papers_df, Field = "AU_UN", sep = ";")
# Contando o número de artigos colocados no dataframe
my_papers_df.info = paste ("Num. artigos:", c(nrow(my_papers_df)))
my_papers_df.info

##### Análise descritiva do data frame de informações bibliográficas 
##### Usando as funções do Bibliometrix
##### Site de referencia http://rstudio-pubs-static.s3.amazonaws.com/261646_2d50d19852ba4e728d76041d58b80a18.html
#Análise Geral 
my_results <- biblioAnalysis(my_papers_df, sep = ";")
my_results ##Impossível de ler na tela.

##### A seguir são mostrados calculadas as medidas e listados os mais importantes.
##### Informe a quantidade de elementos que serão mostrados (Top Ten).
# Esse número será usado em todas as estatísticas do bibliometrix abaixo.
my_num_k=20 ##

# Functions summary and plot
my_S=summary(object = my_results, k = my_num_k, pause = FALSE)
plot(x = my_results, k = my_num_k, pause = FALSE)

##### Análise das referências citadas
# Os artigos mais frequentemente citados
my_CR <- citations(my_papers_df,field="article", sep = ";")
my_CR$Cited[1:my_num_k]
# Os autores mais frequentemente citados (primeiro autor)
my_CR <- citations(my_papers_df, field = "author", sep = ";")
my_CR$Cited[1:my_num_k]
# Os autores dos locais mais frequentemente citados
my_CR <-localCitations(my_papers_df, my_results, sep = ";")
my_CR[1:my_num_k]

##### The function dominance calculates the authors' dominance ranking as proposed 
##### by Kumar & Kumar, 2008. Function arguments are: results (object of class 
##### bibliometrix) obtained by biblioAnalysis; and k (the number of authors to 
##### consider in the analysis).
##### The Dominance Factor is a ratio indicating the fraction of multi authored 
##### articles in which a scholar appears as first author.
my_DF <- dominance(my_results, k = my_num_k)
my_DF

##### Authors' hindex.The index is based on the set of the scientist's most cited 
##### papers and the number of citations that they have received in other publications.
#Informar o autor para criaçao do hindex
my_indices <- Hindex(my_papers_df, authors=c("SPINK,A"), sep = ";")
# Bornmann's impact indices:
my_indices$H
my_indices$CitationList

##### To calculate the hindex of the first most productive authors (in this collection):
authors=gsub(","," ",names(my_results$Authors)[1:my_num_k])
my_indices <-Hindex(my_papers_df, authors, sep = ";")
my_indices$H

##### Lotka's Law coefficient estimation
##### The function lotka estimates Lotka's law coefficients for scientific 
##### productivity (Lotka A.J., 1926). Lotka's law describes the frequency of publication by authors in any given field as an inverse square law, where the number of authors publishing a certain number of articles is a fixed ratio to the number of authors publishing a single article. This assumption implies that the theoretical beta coefficient of Lotka's law is equal to 2. Using lotka function is possible to estimate the Beta coefficient of our bibliographic collection and assess,through a statistical test, the similarity of this empirical distribution with the theoretical one.
my_L <- lotka(my_results)
# Author Productivity. Empirical Distribution
my_L$AuthorProd
# Beta coefficient estimate
my_L$Beta
# Goodness of fit
my_L$R2
# P-value of K-S two sample test
my_L$p.value

##### You can compare the two distributions using plot function:
# Observed distribution
Observed=my_L$AuthorProd[,3]
# Theoretical distribution with Beta = 2
Theoretical=10^(log10(my_L$C)-2*log10(my_L$AuthorProd[,1]))
plot(my_L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), xlab="Articles",ylab="Freq. of
     Authors",main="Scientific Productivity")
lines(my_L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"),col=c("red","blue"),lty =
         c(1,1,1),cex=0.6,bty="n")

##### Bibliometric network matrices
##### Manuscript's attributes are connected to each other through the manuscript 
##### itself: author(s) to journal, keywords to publication date, etc. These 
##### connections of different attributes generate bipartite networks that can be 
##### represented as rectangular matrices (Manuscripts x Attributes). Furthermore, 
#####scientific publications regularly contain references to other scientific works. 
##### This generates a further network, namely, cocitation or coupling network. 
##### These networks are analysed in order to capture meaningful properties of the 
##### underlying research system, and in particular to determine the influence of 
##### bibliometric units such as scholars and journals.
##### Bipartite networks
##### cocMatrix is a general function to compute a bipartite network selecting 
##### one of the metadata attributes. For example, to create a network 
##### Manuscript x Publication Source you have to use the field tag "SO"
##### For a complete list of field tags see https://images.webofknowledge.com/WOKRS410B4/help/WOS/h_fieldtags.html

# my_A is a rectangular binary matrix, representing a bipartite network where 
# rows and columns are manuscripts and sources respectively. The generic element 
# is 1 if the manuscript has been published in source , 0 otherwise. The column 
# sum is the number of manuscripts published in source. Sorting, in decreasing order,
# the column sums of A, you can see the most relevant publication sources:
my_A <-cocMatrix(my_papers_df, Field = "SO", sep = ";")
sort(Matrix::colSums(my_A), decreasing = TRUE)[1:my_num_k]
##### Following this approach, you can compute several bipartite networks
# Citation network
my_A <- cocMatrix(my_papers_df, Field = "CR", sep = ". ")
sort(Matrix::colSums(my_A), decreasing = TRUE)[1:my_num_k]
# Author network
my_A <- cocMatrix(my_papers_df, Field = "AU", sep = ";")
sort(Matrix::colSums(my_A), decreasing = TRUE)[1:my_num_k]
# Authors' countries (Field = "AU_CO");
my_A <- cocMatrix(my_papers_df, Field = "AU_CO", sep = ";")
sort(Matrix::colSums(my_A), decreasing = TRUE)[1:my_num_k]
# First author of each cited reference (Field = "CR_AU")
my_A <- cocMatrix(my_papers_df, Field = "CR_AU", sep = ";")
sort(Matrix::colSums(my_A), decreasing = TRUE)[1:my_num_k]
# Publication source of each cited reference (Field = "CR_SO")
my_A <- cocMatrix(my_papers_df, Field = "CR_SO", sep = ";")
sort(Matrix::colSums(my_A), decreasing = TRUE)[1:my_num_k]
# and Authors' affiliations (Field = "AU_UN")
my_A <- cocMatrix(my_papers_df, Field = "AU_UN", sep = ";")
sort(Matrix::colSums(my_A), decreasing = TRUE)[1:my_num_k]
# Author keyword network
my_A <-cocMatrix(my_papers_df, Field = "DE", sep = ";")
sort(Matrix::colSums(my_A), decreasing = TRUE)[1:my_num_k]
# Keyword Plus network
my_A <-cocMatrix(my_papers_df, Field = "ID", sep = ";")
sort(Matrix::colSums(my_A), decreasing = TRUE)[1:my_num_k]

##### Bibliographic coupling Two articles are said to be bibliographically coupled 
##### if at least one cited source appears in the bibliographies or reference lists 
##### of both articles (Kessler, 1963). A coupling network can be obtained using 
##### the general formulation: B=A.ATB=A.AT. Where A is a bipartite network.
##### The function biblioNetwork calculates, starting from a bibliographic data frame, 
##### the most frequently used coupling networks: Authors, Sources, and Countries.
##### biblioNetwork uses two arguments to define the network to compute:
##### analysis argument can be "cocitation", "coupling", "collaboration", or "cooccurrences".
##### network argument can be "authors", "references", "sources", "countries", "universities", "keywords", "author_keywords", "titles" and "abstracts".

# Depois implementar o menu e as estruturas de repetiçao
# ##### Collaboration Networks #####
# my_analysis = "collaboration"
# my_network = "authors"
# my_network = "universities"
# my_network = "countries"
# ##### Co-citation Networks #####
# my_analysis = "co-citation" 
# my_network = "authors"
# my_network = "references"
# my_network = "sources"
# ##### Coupling Networks #####
# my_analysis = "coupling"
# my_network = "references"
# my_network = "authors"
# my_network = "sources"
# my_network = "countries"
# ##### Co-occurrences Networks #####
# my_analysis = "co-occurrences"
# my_network = "authors"
# my_network = "sources"
# my_network = "keywords"
# my_network = "author_keywords"
# my_network = "titles"
# my_network = "abstracts"

##### Criando a rede de acordo com o tipo de análise e o tipo de rede 
##### biblioNetwork creates different bibliographic networks from a bibliographic data frame.
my_analysis = "coupling"
my_network = "references"
my_NetMatrix <-biblioNetwork(my_papers_df, analysis = my_analysis, network = my_network, sep = ". ")
networkPlot(my_NetMatrix, n = my_num_k, Title = paste (c(my_analysis),c(my_network)), type = "fruchterman", size=FALSE, remove.multiple=TRUE)

##### Articles with only a few references, therefore, would tend to be more weakly 
##### bibliographically coupled, if coupling strength is measured simply according 
##### to the number of references that articles contain in common. This suggests 
##### that it might be more practicable to switch to a relative measure of 
##### bibliographic coupling. couplingSimilarity function calculates Jaccard or 
##### Salton similarity coefficient among vertices of a coupling network.
my_analysis = "coupling"
my_network = "authors"
my_NetMatrix <-biblioNetwork(my_papers_df, analysis = my_analysis, network = my_network, sep = ". ")
networkPlot(my_NetMatrix, n = my_num_k, Title = paste (c(my_analysis),c(my_network)), type = "fruchterman", size=FALSE, remove.multiple=TRUE)

# calculate jaccard similarity coefficient
my_S <- couplingSimilarity(my_NetMatrix, type="jaccard")
# plot authors' similarity (first 20 authors)
networkPlot(my_S, n = my_num_k, Title = paste (c(my_analysis),c(my_network)), remove.multiple=TRUE)

##### Bibliographic cocitation We talk about cocitation of two articles when both are 
##### cited in a third article. Thus, cocitation can be seen as the counterpart of 
##### bibliographic coupling. A cocitation network can be obtained using the 
##### general formulation: C=At.AC=At.A. where A is a bipartite network.
my_analysis = "co-citation"
my_network = "references"
my_NetMatrix <-biblioNetwork(my_papers_df, analysis = my_analysis, network = my_network, sep = ". ")
networkPlot(my_NetMatrix, n = my_num_k, Title = paste (c(my_analysis),c(my_network)), type = "fruchterman", size=FALSE, remove.multiple=TRUE)

##### Bibliographic collaboration Scientific collaboration network is a network 
##### where nodes are authors and links are coauthorships as the latter is one of
##### the most well documented forms of scientific collaboration (Glanzel, 2004). 
##### An author collaboration network can be obtained using the general formulation: 
##### AC=At.AAC=At.A. where A is a bipartite network Manuscripts x Authors.
my_analysis = "collaboration"
my_network = "authors"
my_NetMatrix <-biblioNetwork(my_papers_df, analysis = my_analysis, network = my_network, sep = ". ")
networkPlot(my_NetMatrix, n = my_num_k, Title = paste (c(my_analysis),c(my_network)), type = "fruchterman", size=FALSE, remove.multiple=TRUE)

my_analysis = "collaboration"
my_network = "countries"
my_NetMatrix <-biblioNetwork(my_papers_df, analysis = my_analysis, network = my_network, sep = ". ")
networkPlot(my_NetMatrix, n = my_num_k, Title = paste (c(my_analysis),c(my_network)), type = "circle", size=FALSE, remove.multiple=TRUE)

my_analysis = "co-citation"
my_network = "references"
my_NetMatrix <-biblioNetwork(my_papers_df, analysis = my_analysis, network = my_network, sep = ". ")
networkPlot(my_NetMatrix, n = my_num_k, Title = paste (c(my_analysis),c(my_network)), type = "circle", size=FALSE, remove.multiple=TRUE)

my_analysis = "co-occurrences"
my_network = "keywords"
my_NetMatrix <-biblioNetwork(my_papers_df, analysis = my_analysis, network = my_network, sep = ". ")
networkPlot(my_NetMatrix, n = my_num_k, Title = paste (c(my_analysis),c(my_network)), type = "circle", size=FALSE, remove.multiple=TRUE)

##### CoWord Analysis: Conceptual structure of a field The aim of the coword analysis
##### is to map the conceptual structure of a framework using the word cooccurrences 
##### in a bibliographic collection. The analysis can be performed through 
##### dimensionality reduction techniques such as Multidimensional Scaling (MDS) or 
##### Multiple Correspondence Analysis (MCA). Here, we show an example using the 
##### function conceptualStructure that performs a MCA to draw a conceptual 
##### structure of the field and Kmeans clustering to identify clusters of documents
##### which express common concepts. Results are plotted on a twodimensional map. 
##### conceptualStructure includes natural language processing (NLP) routines 
##### (see the function termExtraction) to extract terms from titles and abstracts. 
##### In addition, it implements the Porter's stemming algorithm to reduce inflected 
##### (or sometimes derived) words to their word stem, base or root form.

# Conceptual Structure using keywords
my_CS <- conceptualStructure(my_papers_df, field="ID", minDegree=4, k.max=5, stemming=FALSE)

##### Historical Co-Citation Network Historiographic map is a graph proposed by 
##### E. Garfield to represent a chronological network map of most relevant 
##### co-citations resulting from a bibliographic collection. The function generates
##### a chronological co-citation network matrix which can be plotted using histPlot:

# Create a historical co-citation network
my_histResults <- histNetwork(my_papers_df, n =  my_num_k, sep = ";")
