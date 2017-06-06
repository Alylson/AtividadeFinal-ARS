library(shiny)
library(bibliometrix)
#library(easyPubMed)
library(igraph)
library(igraphinshiny)
options(shiny.maxRequestSize=30*1024^2) 
options(shiny.port = 3000)

my_papers_df = data.frame()          # VARIÁVEL QUE ARMAZENA O DATAFRAME PRINCIPAL CONSTRUÍDO PELO BIBLIOMETRIX
my_graph =""                         # VARIÁVEL QUE ARMAZENA O GRAFO GERADO 
my_results = ""                      # VARIAVEL QUE ARMAZENA ANALISE FEITA PELO BIBLIOMETRIX
my_NetMatrix = ""
output_df = data.frame() 
# Funcoes    ######

CriaAnaliseBibliometrica = function() { 
  my_results <<- biblioAnalysis(my_papers_df, sep = ";")
  
  
}

TransformaTextoEmDataframe = function(entrada){
  my_dbsource = "isi" 
  my_format = "plaintext" 
  my_papers_df <<-convert2df(entrada, dbsource=my_dbsource, format=my_format) #definir formato e font
  my_papers_df <<- metaTagExtraction(my_papers_df, Field = "AU_CO", sep = ";")
  my_papers_df <<- metaTagExtraction(my_papers_df, Field = "CR_AU", sep = ";")
  my_papers_df <<- metaTagExtraction(my_papers_df, Field = "CR_SO", sep = ";")
  my_papers_df <<- metaTagExtraction(my_papers_df, Field = "AU_UN", sep = ";")
}

observacoes = readLines("/home/jeronimo/Projetos/AnaliseRedes/TrabFinal/AtividadeFinal-ARS/testes/usec.txt")
TransformaTextoEmDataframe(observacoes)

CriaAnaliseBibliometrica()


CR <- citations(my_papers_df, field = "article", sep = ".  ")
publicacoes = capture.output(CR$Cited[2:10])
publicacao <- c()
qtd_citacoes <- c() 
i=1
saida = ""
j=2
while(j <= length(publicacoes)){
  publicacao <- append(publicacao, publicacoes[j] )
  qtd_citacoes <- append(qtd_citacoes , publicacoes[j+1])
  #saida = paste(saida, "<tr><td>",i,"</td><td>" ,publicacoes[j], "</td><td>" ,publicacoes[j+1], "</td></tr>")
  print(i)
  j=j+2
  i=i+1
}
print(summary(output_df))
output_df <<- data.frame(publicacao,qtd_citacoes)
output_df 
