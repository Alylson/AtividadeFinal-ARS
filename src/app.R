library(shiny)
library(shinyjs)

library(bibliometrix)
#library(easyPubMed)
library(igraph)
library(igraphinshiny)

library("tm")
library("SnowballC")
library("wordcloud")

options(shiny.maxRequestSize=30*1024^2) 
options(shiny.port = 3000)
Sys.setenv(LANG = "pt_BR.UTF-8")

# Variaveis  ######

my_papers_df = data.frame()          # VARIÁVEL QUE ARMAZENA O DATAFRAME PRINCIPAL CONSTRUÍDO PELO BIBLIOMETRIX
my_graph =""                         # VARIÁVEL QUE ARMAZENA O GRAFO GERADO 
my_results = ""                      # VARIAVEL QUE ARMAZENA ANALISE FEITA PELO BIBLIOMETRIX
my_NetMatrix = ""
my_graph_metrics = data.frame()


# Funcoes    ######
#formata numeros com decimais m informa numero e qtd de casas decimais apos a virgula 

fmtd = function(n,d){ prettyNum(round(n,d), big.mark = ".", big.interval = 3, decimal.mark = ",", nsmall = 2) }


###  Apoio a Exibicao de plots e tabs na aba estatiticas de rede 
EscondePlotEstatisticas = function() {
  hide("out_plot_statistics") 
  hide("out_table_metrics")

  hide(selector = "#painel_estatisticas li a[data-value=tab_estatisticas_plot]") 
  hide(selector = "#painel_estatisticas li a[data-value=tab_estatisticas_dados]") 
  show(selector = "#painel_estatisticas li a[data-value=tab_estatisticas_info]") 
  
  show("out_text_statistics") 

}

RestauraSaidasEstatisticas = function() {
  hide("out_text_statistics")

  show(selector = "#painel_estatisticas li a[data-value=tab_estatisticas_plot]") 
  show(selector = "#painel_estatisticas li a[data-value=tab_estatisticas_dados]") 
  hide(selector = "#painel_estatisticas li a[data-value=tab_estatisticas_info]") 
  
  show("out_plot_statistics") 
  show("out_table_metrics")

}

ExibeTodasSaidasEstatisticas = function() {

  show(selector = "#painel_estatisticas li a[data-value=tab_estatisticas_plot]") 
  show(selector = "#painel_estatisticas li a[data-value=tab_estatisticas_dados]") 
  show(selector = "#painel_estatisticas li a[data-value=tab_estatisticas_info]") 

  show("out_text_statistics")  
  show("out_plot_statistics") 
  show("out_table_metrics")

}



###  Analise Bibliomtrica
PublicacoesMaisReferenciadas = function (qtd=11){
  print("Inicio PublicacaoesMaisReferenciadas")
  CR <- citations(my_papers_df, field = "article", sep = ".  ")
  publicacoes = capture.output(CR$Cited[2:qtd])
  publicacao <- c()
  qtd_citacoes <- c() 
  i=1
  saida = ""
  j=2
  while(j <= length(publicacoes)){
    publicacao <- append(publicacao, publicacoes[j] )
    qtd_citacoes <- append(qtd_citacoes , publicacoes[j+1])
    print(i)
    j=j+2
    i=i+1
  }
  #print(summary(output_df))
  #output_df <<- cbind("Publicação" = publicacao, "Quantidade Citações" = qtd_citacoes )
  #output_df 
  saida <- data.frame(publicacao, qtd_citacoes)
  names(saida) <- c("Publicação" , "Quantidade de Citações") 
  print("Fim PublicacaoesMaisReferenciadas")

  saida

}

# AUTORES MAIS CITADOS
AutoresMaisCitados = function(n=10,ano="Todos"){

  if(ano != "Todos") {autores = my_papers_df$AU[my_papers_df$PY==ano]}
  else {autores = my_papers_df$AU}
  vautores = c()
  print("Arrumando autores")
  for(i in autores){

    for(j in strsplit(i, ";"))   vautores = c(vautores, trimws(j))
  }
  print("Fim autores")
  t=sort(table(vautores), decreasing = TRUE)
  autores = names(t)
  qtds = as.vector(t)
  #saida <-  cbind("Autores" = autores , "Numero de Citações" = qtds)
  saida <- data.frame(autores,qtds) 
  names(saida) <- c( "Autores" , "Numero de Citações")
  #for(i in 1:n){
  #  saida =   paste(saida,"<tr><td>",autores[i],"</td><td align='right'>",fns(qtds[i]),"</td></tr>")
  #}
  saida
}


PalavrasChavesMaisUtilizadas = function(n=10, ano = "Todos"){
  print("Inicio PalavrasChavesMaisUtilizadas")
  pc = c()
  analise = my_papers_df$DE
  if(ano != "Todos"){
    analise = my_papers_df$DE[my_papers_df$PY == ano]
  }
  if(n == "Todos"){
    n = length(my_papers_df$PY)
  }
  for(i in analise){
    for(j in strsplit(i, ";")){
        pc = c(pc, trimws(j))
    }
  }
  pc = pc[pc != ""]
  x= sort(table(pc), decreasing = TRUE)
  pc = names(x)
  valores = c(as.vector(x))
  saida <- data.frame(pc,valores)
  names(saida) <- c("Palavras Chave" , "Quantidade de utilizações")
  print("Fim PalavrasChavesMaisUtilizadas")

  saida
}

LinguasPublicacao = function(){
  print("Inicio LinguasPublicacao")
  x= sort(table (my_papers_df$LA), decreasing = TRUE)
  linguas = c(names(x))
  valores = c(as.vector(x))
  
  saida <- data.frame(linguas,valores)
  names(saida) <- c("Lingua" , "Quantidade de Publicações")
  print("Fim LinguasPublicacao")
  saida
}

# LISTA PER?ODICOS QUE MAIS PUBLICAM
PeriodicosQueMaisPublicam = function(n,ano="Todos"){
  print("Inicio Periodico")
  if(ano != "Todos") {periodicos = my_papers_df$PU[my_papers_df$PY==ano]}
  else {periodicos = my_papers_df$PU}
  t=sort(table(periodicos), decreasing = TRUE)
  nomes = names(t)
  qtds = as.vector(t)

  saida <- data.frame(nomes,qtds)
  names(saida) <- c("Periódico" , "Quantidade de Publicações")
  print("Fim Periodico")

  saida
}

TiposDePublicacao = function(){
  print("Inicio de TiposDePublicacao")
  x= sort(table (my_papers_df$DT), decreasing = TRUE)
  tiposDePublicacao = c(names(x))
  valores = c(as.vector(x))
 
  saida <- data.frame(tiposDePublicacao,valores)
  names(saida) <- c("Tipo de Publicacao" , "Quantidade")
  print("Fim de TiposDePublicacao")


  saida
}

CriaNuvemDePalavras = function() {
  #x= sort(table (my_papers_df$LA), decreasing = TRUE)
  d =   my_papers_df$DE[!is.na(my_papers_df$DE)] 
  conjunto_palavras = Corpus(VectorSource(d))
  ##conjunto_palavras <- tm_map(saida, content_transformer(tolower))
  ##conjunto_palavras <- tm_map(saida, removePunctuation)
  ##conjunto_palavras <- tm_map(saida, PlainTextDocument)
  ##conjunto_palavras <- tm_map(saida, removeWords, stopwords('english'))
  ##conjunto_palavras <- tm_map(saida, stemDocument)

  tdm <- TermDocumentMatrix(conjunto_palavras)
  matrix <- as.matrix(tdm)
  v <- sort(rowSums(matrix) , decreasing=TRUE )
  d <- data.frame(word = names(v) , freq=v)

  saida <- wordcloud(d$word , d$freq ,  scale = c(6, 0.5) , max.words = 100, min.freq=2 ,  random.order = FALSE, colors=brewer.pal(8, "Set1"))
  saida
}


###  Fim Analise Bibliomtrica

TransformaTextoEmDataframe = function(entrada){
  my_dbsource = "isi" 
  my_format = "plaintext" 
  my_papers_df <<- convert2df(entrada, dbsource=my_dbsource, format=my_format) #definir formato e font
  my_papers_df <<- metaTagExtraction(my_papers_df, Field = "AU_CO", sep = ";")
  my_papers_df <<- metaTagExtraction(my_papers_df, Field = "CR_AU", sep = ";")
  my_papers_df <<- metaTagExtraction(my_papers_df, Field = "CR_SO", sep = ";")
  my_papers_df <<- metaTagExtraction(my_papers_df, Field = "AU_UN", sep = ";")
  # VETOR DE TERMOS PARA EXTRAÇÃO
  my_keep.terms <<- c()
  # VETOR DE TERMOS A SER REMOVIDO
  my_remove.terms <<- c()
  # VETOR DE SINÔNIMOS
  my_synonyms <<- c("study; studies", "system; systems", "library;libraries", "user;users","MODEL;MODELS" )
  #my_papers_df <<- termExtraction(my_papers_df, Field = "TI", synonyms=my_synonyms, remove.numbers=TRUE,  
  #                                remove.terms=my_remove.terms, keep.terms=my_keep.terms, verbose=FALSE)
  #my_papers_df <<- termExtraction(my_papers_df, Field = "TI",  remove.numbers=TRUE,  
  #                                remove.terms=my_remove.terms, keep.terms=my_keep.terms, verbose=FALSE)
  #my_papers_df <<- termExtraction(my_papers_df, Field = "AB", synonyms=my_synonyms, remove.numbers=TRUE,
  #                                remove.terms=my_remove.terms, keep.terms=my_keep.terms, verbose=FALSE)
  #my_papers_df <<- termExtraction(my_papers_df, Field = "DE", synonyms=my_synonyms, remove.numbers=TRUE,
  #                                remove.terms=my_remove.terms, keep.terms=my_keep.terms, verbose=FALSE)
  #my_papers_df <<- termExtraction(my_papers_df, Field = "ID", synonyms=my_synonyms, remove.numbers=TRUE,





  my_papers_df <<- termExtraction(my_papers_df, Field = "TI",  remove.numbers=TRUE,  
                                  remove.terms=my_remove.terms, keep.terms=my_keep.terms, verbose=FALSE)
  my_papers_df <<- termExtraction(my_papers_df, Field = "AB",  remove.numbers=TRUE,
                                  remove.terms=my_remove.terms, keep.terms=my_keep.terms, verbose=FALSE)
  my_papers_df <<- termExtraction(my_papers_df, Field = "DE",  remove.numbers=TRUE,
                                  remove.terms=my_remove.terms, keep.terms=my_keep.terms, verbose=FALSE)
  my_papers_df <<- termExtraction(my_papers_df, Field = "ID",  remove.numbers=TRUE,
                                  remove.terms=my_remove.terms, keep.terms=my_keep.terms, verbose=FALSE)
}

CriaAnaliseBibliometrica = function() { 
  my_results <<- biblioAnalysis(my_papers_df, sep = ";")
}

QuantidadePublicacoes = function(){ length(my_papers_df[[1]]) }

TransformaDataframeEmGrafo = function(my_analysis, my_network, my_netDegree){
  my_NetMatrix <<-""
  my_graph <<-""
  if (my_network=="history references"){
    my_graph_his <-histNetwork(my_papers_df,sep = ";")
    my_NetMatrix <- my_graph_his$NetMatrix
  }
  else
  {
    my_NetMatrix <- biblioNetwork(my_papers_df, analysis = my_analysis, network = my_network, sep = ";")
  }
  diag <- Matrix::diag 
  my_NetMatrix <-as.matrix( my_NetMatrix[diag(my_NetMatrix) >= my_netDegree,diag(my_NetMatrix) >= my_netDegree])
  #diag(my_NetMatrix) <- 0
  #my_NetMatrix <<-my_NetMatrix
  #my_graph <<- graph.adjacency(my_NetMatrix)
  #my_graph <<- graph.adjacency(my_NetMatrix,mode = "directed")
  my_graph <<-graph_from_adjacency_matrix(my_NetMatrix, weighted=TRUE,mode = "directed")
  my_NetMatrix <<-my_NetMatrix
  my_graph.description <<- paste ("Network of ",my_analysis," of ",my_network, " with threshold ", my_netDegree,". With ", vcount(my_graph), " nodes and ", ecount(my_graph), " edges.",  sep = "")
  #my_graph.description
}

CalculaMedidasCentralidade = function(){
  my_graph_metrics<<-""
  my_graph_metrics<- data.frame(V(my_graph)$name)
  my_graph_metrics["name"]<- data.frame(V(my_graph)$name)
  
  # Grau, grau de entrada e grau de saída
  V(my_graph)$degree <- c(degree(my_graph))
  my_graph_metrics["degree"]<-(V(my_graph)$degree)
  my_graph.degree.summary <<- summary(V(my_graph)$degree)
  my_graph.degree.sd <<-sd(V(my_graph)$degree)
  my_graph.degree.var <<-var(V(my_graph)$degree)
  
  # Grau de entrada
  V(my_graph)$indegree <- c(degree(my_graph, mode = "in"))
  my_graph_metrics["indegree"]<-(V(my_graph)$indegree)
  my_graph.indegree.summary <<-summary(V(my_graph)$indegree)
  my_graph.indegree.sd <<-sd(V(my_graph)$indegree)
  my_graph.indegree.var <<-var(V(my_graph)$indegree)
  
  # Grau de saida 
  V(my_graph)$outdegree <- c(degree(my_graph, mode = "out"))
  my_graph_metrics["outdegree"] <- (V(my_graph)$outdegree)
  my_graph.outdegree.summary <<-summary(V(my_graph)$outdegree)
  my_graph.outdegree.sd <<-sd(V(my_graph)$outdegree)
  my_graph.outdegree.var <<-var(V(my_graph)$outdegree)
  my_graph_metrics<<-my_graph_metrics
  # 4.2 Força dos vértices
  # Força = força de entrada + força de saída
  V(my_graph)$strength <- c(graph.strength(my_graph))
  my_graph_metrics["strength"]<- (V(my_graph)$strength)
  my_graph.strength.summary <<-summary(V(my_graph)$strength)
  my_graph.strength.sd <<-sd(V(my_graph)$strength)
  
  # Força de entrada
  V(my_graph)$instrength <- c(graph.strength(my_graph, mode =c("in")))
  my_graph_metrics["instrength"]<- (V(my_graph)$instrength)
  my_graph.instrength.summary <<-summary(V(my_graph)$instrength )
  my_graph.instrength.sd <<-sd(V(my_graph)$instrength )
  my_graph.instrength.var <<-var(V(my_graph)$instrength)
  
  # Força de saída
  V(my_graph)$outstrength <- c(graph.strength(my_graph, mode =c("out")))
  my_graph_metrics["outstrength"]<- (V(my_graph)$outstrength)
  my_graph.outstrength.summary <<-summary(V(my_graph)$outstrength)
  my_graph.outstrength.sd <<-sd(V(my_graph)$outstrength)
  my_graph_metrics<<-my_graph_metrics   #### ?????????
  
  # 4.7 log log
  my_graph.degree.distribution <<- c(degree.distribution(my_graph))
  my_d <<- 1:max(V(my_graph)$degree)-1
  my_ind <<- (V(my_graph)$degree.distribution != 0) 
  
  #4.8 - knn Calculate the average nearest neighbor degree of the given vertices and the same quantity in the function of vertex degree
  #my_graph.a.nn.deg <<- graph.knn(my_graph,V(my_graph))$knn
  
  # Diameter - distância geodesica
  my_graph.diameter <<-diameter(my_graph, directed = TRUE, unconnected=TRUE, weights = NULL)
  
  ##Retorna os caminho com diametro atual
  my_graph.get_diameter <<-get_diameter(my_graph)
  
  ##Retorna os 2 vértices que são conectados pelo diâmetro
  my_graph.farthest_vertices <<-farthest_vertices(my_graph)
  
  # 4.Proximidade - A centralidade de proximidade mede quantas etapas são necessárias para acessar cada outro vértice de um determinado vértice.
  V(my_graph)$closeness <- c(closeness(my_graph))
  my_graph_metrics["closeness"]<<-(V(my_graph)$closeness)
  my_graph.closeness <<- centralization.closeness(my_graph)
  my_graph.closeness.res.summary <<- summary (my_graph.closeness$res)
  my_graph.closeness.res.sd <<-sd(my_graph.closeness$res)
  
  # 4.Intermediação
  V(my_graph)$betweenness <- betweenness(my_graph)
  my_graph_metrics["betweenness"]<<- (V(my_graph)$betweenness)
  my_graph.betweenness <<- centralization.betweenness(my_graph)
  my_graph.betweenness.res.summary <<-summary (my_graph.betweenness$res)
  my_graph.betweenness.res.sd <<-sd(my_graph.betweenness$res)
  
  # 4.Excentricidade
  V(my_graph)$eccentricity <-eccentricity(my_graph)
  my_graph_metrics["eccentricity"]<<- (V(my_graph)$eccentricity)
  my_graph.eccentricity.res.summary <<- summary (V(my_graph)$eccentricity)
  my_graph.eccentricity.res.sd <<-sd(V(my_graph)$eccentricity)
  
  # 4.eigen_centrality
  my_graph.eigen <<-eigen_centrality(my_graph)
  V(my_graph)$eigen <-(eigen_centrality(my_graph))$vector
  my_graph_metrics["eigen"] <<-  (V(my_graph)$eigen)
  my_graph_metrics.eigen.summary <<-summary(V(my_graph)$eigen)
  my_graph_metrics.eigen.summary.sd <<-sd(V(my_graph)$eigen)





  # 4.Densidade
  my_graph.density <<-graph.density(my_graph)
  
  # Modularidade
  wtc <- cluster_walktrap(my_graph)
  #modularity(wtc)
  my_graph.modularity <<-modularity(my_graph, membership(wtc))
  my_graph.modularity.matrix <<-modularity_matrix(my_graph,membership(wtc))
  
  # Page Rank
  V(my_graph)$pagerank <-(page.rank(my_graph))$vector
  my_graph_metrics["pagerank"]<<- (V(my_graph)$pagerank)
  my_graph_metrics.pagerank.summary <<-summary(V(my_graph)$pagerank)
  my_graph_metrics.pagerank.summary.sd <<-sd(V(my_graph)$pagerank)
##
##  my_graph_clusters <<-clusters(my_graph, mode="strong" )
##  V(my_graph)$clustering <-(clusters(my_graph))$membership
##  my_graph_metrics$clustering<<- data.frame(V(my_graph)$clustering)
##  my_graph_metrics.clustering.summary <<-summary(V(my_graph)$clustering)
##
  my_graph_clusters <<-clusters(my_graph, mode="strong" )
  V(my_graph)$clustering <-(clusters(my_graph))$membership
  my_graph_metrics["clustering"] <<-  (V(my_graph)$clustering)
  my_graph_metrics.clustering.summary <<-summary(V(my_graph)$clustering)
  my_graph_metrics.clustering.summary.sd <<-sd(V(my_graph)$clustering)

}

ImprimeGrau = function(){
  #
  # GRAU DOS VERTICES
  hist(my_graph.degree,col="lightblue",xlim=c(0, max(my_graph.degree)),xlab="Grau dos vértices", ylab="Frequência", main="", axes="TRUE")
  legend("topright", c(paste("Mínimo =", round(my_graph.degree.summary[1],2)), 
                       paste("Máximo=", round(my_graph.degree.summary[6],2)), 
                       paste("Média=", round(my_graph.degree.summary[4],2)),
                       paste("Mediana=", round(my_graph.degree.summary[3],2)),
                       paste("Desvio Padrão=", round(my_graph.degree.sd[1],2))),
         pch = 1, title = "Grau")
  
  # GRAU DE ENTRADA
  hist(my_graph.indegree,col="lightblue", xlab="Grau de entrada", ylab="Frequência", main="", axes="TRUE")
  legend("topright", c(paste("Mínimo =", round(my_graph.indegree.summary[1],2)), 
                       paste("Máximo=", round(my_graph.indegree.summary[6],2)), 
                       paste("Média=", round(my_graph.indegree.summary[4],2)),
                       paste("Mediana=", round(my_graph.indegree.summary[3],2)),
                       paste("Desvio Padrão=", round(my_graph.indegree.sd[1],2))),
         pch = 1, title = "Grau entrada")
  
  # GRAU DE SAÍDA
  hist(my_graph.outdegree,col="lightblue", xlab="Grau de saída", ylab="Frequência", main="", axes="TRUE")
  legend("topright", c(paste("Mínimo =", round(my_graph.outdegree.summary[1],2)), 
                       paste("Máximo=", round(my_graph.outdegree.summary[6],2)), 
                       paste("Média=", round(my_graph.outdegree.summary[4],2)),
                       paste("Mediana=", round(my_graph.outdegree.summary[3],2)),
                       paste("Desvio Padrão=", round(my_graph.outdegree.sd[1],2))),
         pch = 1, title = "Grau saída")
  
  # BOXPLOT: GRAU ENTRADA, GRAU SAÍDA E GRAU TOTAL
  boxplot(my_graph.indegree, my_graph.outdegree, my_graph.degree, notch = FALSE, ylab = 'Grau', 
          names = c('Grau entrada', 'Grau saída', 'Grau total'), 
          main = 'Boxplot do grau dos vértices', col = c('blue', 'red', 'orange'),shrink=0.8, textcolor="red")
  
}

##ConvArqParaDataFrame = function(entrada){
##  trab_df <<- convert2df(entrada, dbsource="isi", format="plaintext")
##  trab_analise_bbl <<- biblioAnalysis(trab_df, sep = ";"

##}



ui <- fluidPage(
  useShinyjs() ,
  titlePanel("Análise de Redes de Colaboração Científica com R"),
  
  tabsetPanel(
    tabPanel("Apresentação", 
             fluidRow(
               column(3,
                      br() ,
                      br() ,
                      br() ,
                      br() ,
                      br() ,
                      img(src = 'r_logo.png', height = '300px', width = '300px')
                      
               ),
               column(1,br(" ")) ,
               column(6,
                      h3("Atividade Final da Disciplina"),
                      h4("Professor Ricardo Barros") ,
                      p("Objetivo: Este programa visa auxiliar na medição e análise de redes de relacionamentos entre pesquisadores. Estas redes são geradas a partir de pesquisas realizadas no site Web Of Science , que após serem armazenadas em arquivos texto, são usados como insumo pelo programa."),
                      br(),
                      br(),
                      h3("Grupo 1:"),
                      p("Jerônimo Avelar Filho"),
                      p("Lena Lúcia de Moraes"),
                      p("Leda Alves")
               ) ,
               column(2,"")
             )
             
    ) ,           
    tabPanel("Informações Bibliométricas", 
      sidebarPanel( 
        fileInput("arqtrab", "Selecione arquivo WoS", multiple = FALSE, accept = NULL, width = NULL,
        buttonLabel = "Browse...", placeholder = "No file selected") ,
        textOutput('dadosArquivo'),  
        br() ,
        actionButton("show0", "Publicaçoes referenciadas" , class = "btn-primary btn-block"  ),
        actionButton("show1", "Autores Citados", class = "btn-primary  btn-block"  ),
        actionButton("show2", "Palavras Chaves Utilizadas", class = "btn-primary  btn-block"  ),
        actionButton("show3", "Linguas Utilizadas Para Publicacao" , class = "btn-primary  btn-block" ),
        actionButton("show4", "Periodicos Utilizados Para Publicação" , class = "btn-primary  btn-block" ),
        actionButton("show5", "Tipos de Publicação Utilizados" , class = "btn-primary  btn-block" ),
        actionButton("show6", "Nuvem de Palavras" , class = "btn-primary  btn-block" )


      ) ,
      mainPanel(    
        dataTableOutput('painel_1') ,
        plotOutput("nuvem_de_palavras_chave", width = "100%")
      )    
         

    ),
    
    tabPanel("Criação da rede",
             sidebarLayout(
               sidebarPanel(column(12,
                                   isolate(selectInput("in_tp_analysis", "Escolha o tipo de análise:",
                                                       list(`COLABORAÇÃO` = c("Colaboração Autores", "Colaboração Países", "Colaboração Instituições"),
                                                            `CITAÇÃO` = c("Citação Documetos", "Citação Autores", "Citação Fontes", "Citação Países"),
                                                            `CO-CITAÇÃO` = c("Co-citação Documentos", "Co-citação Autores", "Co-citação Fontes", "Co-citação Histórico Referências"),
                                                            `CO-OCORRÊNCIA` = c("Co-ocorrência Autores", "Co-ocorrência Fontes", "Co-ocorrência Palavra-chave","Co-ocorrência Palavra-chave autor", "Co-ocorrência Título", "Co-ocorrência Resumo")
                                                       ))),
                                   isolate(selectInput("in_tp_layout", "Escolha o layout:",
                                                       c("Estrela" ="star",
                                                         "Círculo" ="circle", ## coords <- layout_in_circle(karate, order = order(membership(karate_groups)))
                                                         "Multidimensional" ="mds", #layout_with_mds(g) layout_nicely(graph, dim = 2, ...)
                                                         "Amigável" ="nicely",# layout_nicely(graph, dim = 2, ...
                                                         "With dh" ="With_dh",# # plot(g_8, layout=layout_with_dh, vertex.size=5, vertex.label=NA)
                                                         "With Gem" ="with_gem",# plot(g, layout=layout_with_gem)
                                                         "Grid" ="on_grid",# plot(g, layout=layout_on_grid) rglplot(g, layout=layout_on_grid(g, dim = 3))
                                                         "Esfera" ="layout.sphere",
                                                         "Kamada kawai"="layout.kamada.kawai", # plot(g, layout=layout_with_kk, edge.label=E(g)$weight)
                                                         "Fruchterman-Reingold" ="layout.fruchterman.reingold",
                                                         "Lgl"='layout.lgl',
                                                         "Árvore" ="tree"#plot(tree, layout=layout_as_tree(tree, circular=TRUE))
                                                      
                                                       ))),    
                                   # isolate(selectInput('in_tp_com', 'Visualizar Comunidades:',
                                   #                     c('Componente gigante'='comp_gigante',
                                   #                       'Diferenciar subgrupos pela cor'='sub_grupo',
                                   #                       'Nao destacar'='nao_dest'))),
                                   isolate(checkboxInput('in_tp_cluster', "Visualizar clusters",FALSE)),
                                   isolate(checkboxInput('in_tp_gigante', "Visualizar componente gigante",FALSE)),
                                   isolate(checkboxInput('in_tp_linha', "Espessura das arestas de acordo com o peso",FALSE)),
                                   #isolate(checkboxInput('in_st_exibe_diametro', "Destacar diâmetro",FALSE)),
                                   # isolate(selectInput('in_tp_linha', 'Espessura da linha de acordo com o peso:',
                                   #                     c('Sim'='linha_com_peso',
                                   #                       'Nao'='linha_normal'))),
                                   isolate(radioButtons('in_tp_dim','Dimensionar nós de acordo com:',
                                                       c('Grau'= 'all',
                                                         'Grau de entrada'='in',
                                                         'Grau de saída'='out')
                                                         )),
                                   # isolate(selectInput('in_tp_cor','Colorir nos de acordo com:',
                                   #                     c('Numero de Publicacoes'='num_pub',
                                   #                       'Numero de Citacoes'='num_citacao',
                                   #                       'Todos os nos da mesma cor'='no_nao_colorido'))),
                                   isolate(numericInput("in_num_freq", "Escolha o treshold da rede:",1,1, 100, 1)),
                                   isolate(actionButton("in_cria_rede", "Gera Rede"))
                          )#fim columm
               
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Plot",plotOutput("out_plot_net")),
                   tabPanel("Summary",verbatimTextOutput("info")), 
                   tabPanel("Dados", dataTableOutput('out_table_graph'))
                 )
               )
             )
    ) ,    
    tabPanel("Estatísticas da Rede",
             sidebarLayout(
               sidebarPanel(column(12,
                                   radioButtons("in_tp_metrica", "Escolha a métrica:",
                                                c("Grau médio" = "netdegree",
                                                  "Grau ponderado médio" = "strength",
                                                  "Diâmetro da rede" = "diameter",
                                                  "Densidade do grafo" = "density",
                                                  "Modularidade" = "modularity",
                                                  "PageRank" = "pagerank",
                                                  "Intermediação" = "betweenness", 
                                                  #"Componentes conectados" = "collaborationUniversities",
                                                  "Coeficiente de clustering " = "clustering",
                                                  "Centralidade de autovetor" = "eigen",
                                                  "Excentricidade" = "eccentricity"
                                                  #"Comprimento médio de caminho" = "strength"

                                                  )
                                                )

                                  )
                            
               ),
               mainPanel(
                 id="painel_estatisticas" ,
                 tabsetPanel(
                   #Novo saida de plots 
                   tabPanel(
                    title="Plot",
                    value="tab_estatisticas_plot",
                    plotOutput("out_plot_statistics")
                   ), 
                   #tabPanel("Plot",plotOutput("out_plot_degree")), 
                   #tabPanel("Summary", plotOutput("out_plot_indegree")), 
                   tabPanel(
                    title="Dados", 
                    value="tab_estatisticas_dados",
                    dataTableOutput('out_table_metrics')),
                   tabPanel(
                    title="Informações" , 
                    value="tab_estatisticas_info" ,
                    htmlOutput("out_text_statistics")
                   )
               )
             )
          )  
    ) 
    
  )
)
server <- function(input, output) {
  
  output$dadosArquivo = renderText({
    arquivoEntrada <- input$arqtrab
    if (is.null(arquivoEntrada))
      return(NULL)
    observacoes = readLines(arquivoEntrada$datapath)
    TransformaTextoEmDataframe(observacoes)
    CriaAnaliseBibliometrica()
    print(paste("Quantidade de Publicações lidas: ", QuantidadePublicacoes()))

    
  })
  

  # Analise Bibliometrica
  observeEvent(input$show0, {
    hide("nuvem_de_palavras_chave")
    show("painel_1")    
    output$painel_1 = renderDataTable(

     PublicacoesMaisReferenciadas() 
     , options = list(lengthMenu = c(10, 10, 50,100), pageLength = 10)
    )
  })

  observeEvent(input$show1, {
    hide("nuvem_de_palavras_chave")
    show("painel_1")    
    output$painel_1 =  renderDataTable(
     AutoresMaisCitados()  
     , options = list(lengthMenu = c(10, 10, 50,100), pageLength = 10) 
    )
  })  
  
 observeEvent(input$show2, {
    hide("nuvem_de_palavras_chave")
    show("painel_1")  
    output$painel_1 =  renderDataTable(
     PalavrasChavesMaisUtilizadas()  
     , options = list(lengthMenu = c(10, 10, 50,100), pageLength = 10) 
    )
  })  
 
 observeEvent(input$show3, {
    hide("nuvem_de_palavras_chave")
    show("painel_1")  
    output$painel_1 =  renderDataTable(
     LinguasPublicacao()
     , options = list(lengthMenu = c(10, 10, 50,100), pageLength = 10) 
    )
  })  
 
 observeEvent(input$show4, {
    hide("nuvem_de_palavras_chave")
    show("painel_1")  
    output$painel_1 =  renderDataTable(
     PeriodicosQueMaisPublicam()
     , options = list(lengthMenu = c(10, 10, 50,100), pageLength = 10) 
    )
  })    

 observeEvent(input$show5, {
    hide("nuvem_de_palavras_chave")
    show("painel_1")
    output$painel_1 =  renderDataTable(
     TiposDePublicacao()
     , options = list(lengthMenu = c(10, 10, 50,100), pageLength = 10) 
    )
  })  

 observeEvent(input$show6, {
    hide("painel_1")
    show("nuvem_de_palavras_chave")
    output$nuvem_de_palavras_chave =  renderPlot({ 
       #wordcloud(CriaMapaDePalavras() , max.words = 100, random.order = FALSE, colors=brewer.pal(8, "Dark2"))
      CriaNuvemDePalavras() 
    })
  })  

  # Fim Analise Bibliometrica
  
  observeEvent(input$in_cria_rede,{
    output$out_table_graph = renderDataTable({
      dd <-my_graph_metrics[c("name","degree","indegree","outdegree")]
      dd[order(-dd$degree),]
    }, options = list(lengthMenu = c(10, 20, 50,100), pageLength = 10))
  })
  
  observeEvent(input$in_cria_rede, {
    output$out_plot_net = renderPlot({
      if (isolate(input$in_tp_analysis)=="Colaboração Autores"){
        my_analysis <- "collaboration"
        my_network <- "authors"
      }
      if (isolate(input$in_tp_analysis)=="Colaboração Países"){
        my_analysis <- "collaboration"
        my_network <- "countries"
      }
      if (isolate(input$in_tp_analysis)=="Colaboração Instituições"){
        my_analysis <- "collaboration"
        my_network <- "universities"
      }
      if (isolate(input$in_tp_analysis)=="Citação Documetos"){
        my_analysis <- "coupling"
        my_network <- "references"
      }
      if (isolate(input$in_tp_analysis)=="Citação Autores"){
        my_analysis <- "coupling"
        my_network <- "authors"
      }
      if (isolate(input$in_tp_analysis)=="Citação Fontes"){
        my_analysis <- "coupling"
        my_network <- "sources"
      }
      if (isolate(input$in_tp_analysis)=="Citação Países"){
        my_analysis <- "coupling"
        my_network <- "countries"
      }
      if (isolate(input$in_tp_analysis)=="Co-citação Documentos"){
        my_analysis <- "co-citation"
        my_network <- "references"
      }
      if (isolate(input$in_tp_analysis)=="Co-citação Autores"){
        my_analysis <- "co-citation"
        my_network <- "authors"
      }
      if (isolate(input$in_tp_analysis)=="Co-citação Fontes"){
        my_analysis <- "co-citation"
        my_network <- "sources"
      }
      if (isolate(input$in_tp_analysis)=="Co-citação Histórico Referências"){
        my_analysis <- "co-citation"
        my_network <- "history references"
      }
      if (isolate(input$in_tp_analysis)=="Co-ocorrência Autores"){
        my_analysis <- "co-occurrences"
        my_network <- "authors"
      }
      if (isolate(input$in_tp_analysis)=="Co-ocorrência Fontes"){
        my_analysis <- "co-occurrences"
        my_network <- "sources"
      }
      if (isolate(input$in_tp_analysis)=="Co-ocorrência Palavra-chave"){
        my_analysis <- "co-occurrences"
        my_network <- "keywords"
      }
      if (isolate(input$in_tp_analysis)=="Co-ocorrência Palavra-chave autor"){
        my_analysis <- "co-occurrences"
        my_network <- "author_keywords"
      }
      if (isolate(input$in_tp_analysis)=="Co-ocorrência Título"){
        my_analysis <- "co-occurrences"
        my_network <- "titles"
      }
      if (isolate(input$in_tp_analysis)=="Co-ocorrência Resumo"){
        my_analysis <- "co-occurrences"
        my_network <- "abstracts"
      }
      
      my_analysis <<- my_analysis
      my_network <<- my_network
      my_netDegree <<- isolate(input$in_num_freq)
      TransformaDataframeEmGrafo(my_analysis, my_network, my_netDegree)
      CalculaMedidasCentralidade()
      

      # if (isolate(input$in_st_exibe_diametro)==TRUE){
      #   if (length(diameter(my_graph)>=2)){
      #     my_graph.nodes.diameter<-get.diameter(my_graph)
      #     V(my_graph)[my_graph.nodes.diameter]$color<-"darkgreen"
      #     V(my_graph)[my_graph.nodes.diameter]$size<-10
      #     V(my_graph)[my_graph.nodes.diameter]$label.color<-"white"
      #     E(my_graph)$color<-"grey"
      #     E(my_graph,my_graph.nodes.diameter)$color<-"darkgreen"
      #     E(my_graph,my_graph.nodes.diameter)$width<-2
      #     #Edges in the diameter will be darkgreen and a little extra wide
      #   }
      # }
      # else{
      #   E(my_graph)$color<-"grey"
      # }
      
      # karate_groups <- cluster_optimal(karate)
      # coords <- layout_in_circle(karate, order = order(membership(karate_groups)))
      # V(karate)$label <- sub("Actor ", "", V(karate)$name)
      # V(karate)$label.color <- membership(karate_groups)
      # V(karate)$shape <- "none"
      # plot(karate, layout = coords)
      
      if (isolate (input$in_tp_linha)==TRUE){
        E(my_graph)$width <- E(my_graph)$weight/mean(E(my_graph)$weight)
        my_graph.description <<-paste (my_graph.description, " Edges width with weight.", sep = "")
      }
      else{
        E(my_graph)$width <- 1
      }
      
      if (isolate (input$in_tp_dim)=="in"){
        V(my_graph)$size<-my_graph_metrics$indegree * 0.5  
        my_graph.description <<- paste (my_graph.description, " Dimension node with indegree.",  sep = "")
      }
      else if (isolate (input$in_tp_dim)=="out"){
        V(my_graph)$size<--my_graph_metrics$outdegree * 0.5 
        my_graph.description <<- paste (my_graph.description, "Dimension node with outdegree.",sep = "")
      }
      else {
            V(my_graph)$size<-my_graph_metrics$degree * 0.5 
            my_graph.description <<- paste (my_graph.description, " Dimension node with degree.", sep = "")
      }
      
      if (isolate (input$in_tp_cluster)==TRUE){
        V(my_graph)$color <- rainbow(my_graph_clusters$no)[my_graph_clusters$membership]
        my_graph.description <<- paste (my_graph.description, " Number of clusters:", my_graph_clusters$no, ".", sep = "")
      }
      else{
        V(my_graph)$color <- "orange"
      }
      
      if (isolate (input$in_tp_gigante)==TRUE){
        comp_gigante = which(my_graph_clusters$membership == which.max(my_graph_clusters$csize))
        V(my_graph)[comp_gigante]$color = "#00FFFF"
      }
      
        coords <- as.matrix(layout_(my_graph, as_star()))
        my_coords<-norm_coords(coords, xmin = -1, xmax = 1, ymin = -1, ymax = 1, zmin = -1, zmax = 1)
        
        if (isolate (input$in_tp_layout)=="star"){
          coords <- as.matrix(layout_(my_graph, as_star()))
          my_coords<-norm_coords(coords, xmin = -1, xmax = 1, ymin = -1, ymax = 1, zmin = -1, zmax = 1)
        }
        if (isolate (input$in_tp_layout)=="tree"){
        coords <-as.matrix(layout_as_tree(my_graph, circular=TRUE))
        my_coords<-norm_coords(coords, xmin = -1, xmax = 1, ymin = -1, ymax = 1, zmin = -1, zmax = 1)
        }
        if (isolate (input$in_tp_layout)=="circle"){ ## coords <- layout_in_circle(karate, order = order(membership(karate_groups)))
          coords <- as.matrix( layout_in_circle(my_graph))
          my_coords<-norm_coords(coords, xmin = -1, xmax = 1, ymin = -1, ymax = 1, zmin = -1, zmax = 1)
        }
        if (isolate (input$in_tp_layout)=="mds"){ #layout_with_mds(g) layout_nicely(graph, dim = 2, ...)
          coords <- as.matrix( layout_with_mds(my_graph))
          my_coords<-norm_coords(coords, xmin = -1, xmax = 1, ymin = -1, ymax = 1, zmin = -1, zmax = 1)
        }
        if (isolate (input$in_tp_layout)=="nicely"){ #layout_nicely(graph, dim = 2, ...
          coords <- as.matrix( layout_nicely(my_graph))
          my_coords<-norm_coords(coords, xmin = -1, xmax = 1, ymin = -1, ymax = 1, zmin = -1, zmax = 1)
        }
        if (isolate (input$in_tp_layout=="With_dh")){ #plot(g_8, layout=layout_with_dh, vertex.size=5, vertex.label=NA)
          coords <- as.matrix( layout_with_dh(my_graph))
          my_coords<-norm_coords(coords, xmin = -1, xmax = 1, ymin = -1, ymax = 1, zmin = -1, zmax = 1)
        }
        if (isolate (input$in_tp_layout)=="with_gem"){ #plot(g, layout=layout_with_gem)
          coords <- as.matrix( layout_with_gem(my_graph))
          my_coords<-norm_coords(coords, xmin = -1, xmax = 1, ymin = -1, ymax = 1, zmin = -1, zmax = 1)
        }
        if (isolate (input$in_tp_layout)=="on_grid"){ # plot(g, layout=layout_on_grid) rglplot(g, layout=layout_on_grid(g, dim = 3))
          coords <- as.matrix(layout_on_grid(my_graph))
          my_coords<-norm_coords(coords, xmin = -1, xmax = 1, ymin = -1, ymax = 1, zmin = -1, zmax = 1)
        }
        if (isolate (input$in_tp_layout)=="layout.kamada.kawai"){ ## plot(g, layout=layout_with_kk, edge.label=E(g)$weight)
          coords <- as.matrix( layout_with_kk(my_graph))
          my_coords<-norm_coords(coords, xmin = -1, xmax = 1, ymin = -1, ymax = 1, zmin = -1, zmax = 1)
        }
        if (isolate (input$in_tp_layout)=="Fruchterman-Reingold"){
          coords <- as.matrix(layout.fruchterman.reingold(my_graph))
          my_coords<-norm_coords(coords, xmin = -1, xmax = 1, ymin = -1, ymax = 1, zmin = -1, zmax = 1)
        }
        if (isolate (input$in_tp_layout)=="layout.sphere"){
          coords <- as.matrix(layout_on_sphere(my_graph))
          my_coords<-norm_coords(coords, xmin = -1, xmax = 1, ymin = -1, ymax = 1, zmin = -1, zmax = 1)
        }
        
        my_graph.description <<- paste (my_graph.description, " Layout ",isolate (input$in_tp_layout), ".",sep = "")
        #dev.off()
        plot.new()
        par(mar=c(.1,.1,.1,.1))
        plot(my_graph,
             edge.arrow.size=0.5, 
             #edge.color = "grey",
             edge.curved= 0,
             layout = my_coords,#layout.fruchterman.reingold,
             #vertex.color=V(my_graph)$color,
             vertex.frame.color = 'black',
             vertex.label.dist = 0,
             #vertex.size = V(my_graph)$size,
             vertex.label.color = 'black',
             vertex.label.font = 15, 
             #vertex.label = V(my_graph)$name, 
             vertex.label.cex = 0.5)
  })
  })
  
    ## ESTATISTICAS DE REDE 
  #ENTRADA 
  #in_tp_metrica

  #OPCOES
  #netdegree 
  #strength
  #diameter
  #density
  #modularity
  #pagerank
  #collaborationUniversities
  #clustering
  #eigen
  #strength  

  #SAIDA 
  ###out_plot_statistics
  #out_plot_degree  
  #out_plot_indegree 
  #out_table_metrics 

  
  #output$out_plot_degree <- renderPlot({    

  # Eventos geradores de gráficos
  observeEvent(input$in_tp_metrica,{ 
    ## GRAU DA REDE 
    if (input$in_tp_metrica=="netdegree"){ 
      RestauraSaidasEstatisticas()
      output$out_plot_statistics <- renderPlot({
  
          #hist(V(my_graph)$degree,col="lightblue",xlim=c(0, max(V(my_graph)$degree)),xlab="Grau dos vértices", ylab="Frequência", main="", axes="TRUE")
          hist(my_graph_metrics$degree,col="lightblue",xlim=c(0, max(my_graph_metrics$degree)),xlab="Grau dos vértices", ylab="Frequência", main="", axes="TRUE")
          legend("topright", c(paste("Mín.=", round(my_graph.degree.summary[1],2)),
                               paste("Máx.=", round(my_graph.degree.summary[6],2)),
                               paste("Média=", round(my_graph.degree.summary[4],2)),
                               paste("Mediana=", round(my_graph.degree.summary[3],2)),
                               paste("D. padrão=", round(my_graph.degree.sd[1],2))),
                 pch = 1, title = "Grau")

      })
    }

    ## GRAU PONDERADO DA REDE
    if (input$in_tp_metrica=="strength"){ 
      RestauraSaidasEstatisticas()
      output$out_plot_statistics <- renderPlot({
  
          #hist(V(my_graph)$degree,col="lightblue",xlim=c(0, max(V(my_graph)$degree)),xlab="Grau dos vértices", ylab="Frequência", main="", axes="TRUE")
          hist(my_graph_metrics$strength,col="lightblue",xlim=c(0, max(my_graph_metrics$strength)),xlab="Grau Ponderado dos vértices", ylab="Frequência", main="", axes="TRUE")
          legend("topright", c(paste("Mín.=", round(my_graph.strength.summary[1],2)),
                               paste("Máx.=", round(my_graph.strength.summary[6],2)),
                               paste("Média=", round(my_graph.strength.summary[4],2)),
                               paste("Mediana=", round(my_graph.strength.summary[3],2)),
                               paste("D. padrão=", round(my_graph.strength.sd[1],2))),
                 pch = 1, title = "Grau Ponderado")

      })
    }


    if (input$in_tp_metrica=="pagerank"){ 
      RestauraSaidasEstatisticas()
      print(my_graph_metrics.pagerank.summary)
      output$out_plot_statistics <- renderPlot({
  
          #hist(V(my_graph)$degree,col="lightblue",xlim=c(0, max(V(my_graph)$degree)),xlab="Grau dos vértices", ylab="Frequência", main="", axes="TRUE")
          hist(my_graph_metrics$pagerank,col="lightblue",xlim=c(0, max(my_graph_metrics$pagerank)),xlab="Pagerank", ylab="Frequência", main="", axes="TRUE")
          legend("topright", c(paste("Mín.=", round(my_graph_metrics.pagerank.summary[1],4)),
                               paste("Máx.=", round(my_graph_metrics.pagerank.summary[6],4)),
                               paste("Média=", round(my_graph_metrics.pagerank.summary[4],4)),
                               paste("Mediana=", round(my_graph_metrics.pagerank.summary[3],4))
                               ,
                               paste("D. padrão=", round(my_graph_metrics.pagerank.summary.sd[1],4))
                               ),
                 pch = 1, title = "Pagerank")

      })
    }



    if (input$in_tp_metrica=="clustering"){ 
      RestauraSaidasEstatisticas()
      print("CLUSTERING J")
      print(head(my_graph_metrics$clustering))

      print(class(my_graph_metrics$clustering))

      output$out_plot_statistics <- renderPlot({
        print("Clustering output")
        print(my_graph_metrics$clustering)
          hist(my_graph_metrics$clustering,col="lightblue",xlim=c(0, max(my_graph_metrics$clustering)),xlab="Clustering", ylab="Frequência", main="", axes="TRUE")
          

          legend("topright", c(paste("Mín.=", round(my_graph_metrics.clustering.summary[1],4)),
                               paste("Máx.=", round(my_graph_metrics.clustering.summary[6],4)),
                               paste("Média=", round(my_graph_metrics.clustering.summary[4],4)),
                               paste("Mediana=", round(my_graph_metrics.clustering.summary[3],4))
                               ,
                               paste("D. padrão=", round(my_graph_metrics.clustering.summary.sd[1],4))
                               ),
                 pch = 1, title = "Clustering")

      })
    }


    if (input$in_tp_metrica=="eigen"){ 
      RestauraSaidasEstatisticas()
      output$out_plot_statistics <- renderPlot({
          hist(my_graph_metrics$eigen,col="lightblue",xlim=c(0, max(my_graph_metrics$eigen)),xlab="Centralidade", ylab="Frequência", main="", axes="TRUE")
          

          legend("topright", c(paste("Mín.=", round(my_graph_metrics.eigen.summary[1],4)),
                               paste("Máx.=", round(my_graph_metrics.eigen.summary[6],4)),
                               paste("Média=", round(my_graph_metrics.eigen.summary[4],4)),
                               paste("Mediana=", round(my_graph_metrics.eigen.summary[3],4))
                               ,
                               paste("D. padrão=", round(my_graph_metrics.eigen.summary.sd[1],4))
                               ),
                 pch = 1, title = "Centralidade")

      })
    }

    if (input$in_tp_metrica=="betweenness"){ 
      RestauraSaidasEstatisticas()

      output$out_plot_statistics <- renderPlot({
          hist(my_graph_metrics$betweenness,col="lightblue",xlim=c(0, max(my_graph_metrics$betweenness)),xlab="Centralidade", ylab="Frequência", main="", axes="TRUE")
          

          legend("topright", c(paste("Mín.=", round(my_graph.betweenness.res.summary[1],4)),
                               paste("Máx.=", round(my_graph.betweenness.res.summary[6],4)),
                               paste("Média=", round(my_graph.betweenness.res.summary[4],4)),
                               paste("Mediana=", round(my_graph.betweenness.res.summary[3],4))
                               ,
                               paste("D. padrão=", round(my_graph.betweenness.res.sd[1],4))
                               ),
                 pch = 1, title = "Centralidade")

      })
    }


    if (input$in_tp_metrica=="eccentricity"){ 
      RestauraSaidasEstatisticas()

      output$out_plot_statistics <- renderPlot({
          hist(my_graph_metrics$eccentricity,col="lightblue",xlim=c(0, max(my_graph_metrics$eccentricity)),xlab="Excentricidade", ylab="Frequência", main="", axes="TRUE")
          

          legend("topright", c(paste("Mín.=", round(my_graph.eccentricity.res.summary[1],4)),
                               paste("Máx.=", round(my_graph.eccentricity.res.summary[6],4)),
                               paste("Média=", round(my_graph.eccentricity.res.summary[4],4)),
                               paste("Mediana=", round(my_graph.eccentricity.res.summary[3],4))
                               ,
                               paste("D. padrão=", round(my_graph.eccentricity.res.sd[1],4))
                               ),
                 pch = 1, title = "Excentricidade")

      })
    }    


    # Eventos geradores de Informações  
    #Diametro 
    if (input$in_tp_metrica=="diameter"){ 
      EscondePlotEstatisticas()
        output$out_text_statistics = renderText(
          #print(class(my_graph.diameter)) 

          #print(paste("Diametro da rede: ", my_graph.diameter))
          print(paste("<br><br><b>Diametro da Rede: </b>", my_graph.diameter))
        )
    } 

    #  my_graph.density <<-graph.density(my_graph)

    #Densidade da rede  
    if (input$in_tp_metrica=="density"){ 
      EscondePlotEstatisticas()
        output$out_text_statistics = renderText(
          #print(class(my_graph.diameter)) 

          #print(paste("Diametro da rede: ", my_graph.diameter))
          print(paste("<br><br><b>Densidade da Rede: </b>", my_graph.density))
        )
    }


    #modularity 
    if (input$in_tp_metrica=="modularity"){ 
      EscondePlotEstatisticas()
      output$out_text_statistics = renderText(
        print(paste("<br><br><b>Modularidade da Rede: </b>", my_graph.modularity))
      )
    } 
    
 
    
  



  })



  # Eventos geradores de tabelas 
  observeEvent(input$in_tp_metrica,{
    ## GRAU DA REDE 
    if(input$in_tp_metrica == "netdegree") { 
      output$out_table_metrics = renderDataTable({
        dd <-my_graph_metrics[c("name","degree","indegree","outdegree")]
        dd[order(-dd$degree),]
      }, options = list(lengthMenu = c(10, 20, 50,100), pageLength = 10))
    }

    ## GRAU PONDERADO DA REDE
    if(input$in_tp_metrica == "strength") { 
      output$out_table_metrics = renderDataTable({
        dd <-my_graph_metrics[c("name","strength","instrength","outstrength")]
        dd[order(-dd$strength),]
      }, options = list(lengthMenu = c(10, 20, 50,100), pageLength = 10))
    }

    if(input$in_tp_metrica == "pagerank") { 
      output$out_table_metrics = renderDataTable({

        dd <-my_graph_metrics[c("name","pagerank")]

        dd[order(-dd$pagerank),]
      }, options = list(lengthMenu = c(10, 20, 50,100), pageLength = 10))
    }

    if(input$in_tp_metrica == "clustering") { 
      output$out_table_metrics = renderDataTable({

        dd <-my_graph_metrics[c("name","clustering")]

        dd[order(-dd$clustering),]
      }, options = list(lengthMenu = c(10, 20, 50,100), pageLength = 10))
    }


    if(input$in_tp_metrica == "eigen") { 
      output$out_table_metrics = renderDataTable({

        dd <-my_graph_metrics[c("name","eigen")]

        dd[order(-dd$eigen),]
      }, options = list(lengthMenu = c(10, 20, 50,100), pageLength = 10))
    }


    if(input$in_tp_metrica == "betweenness") { 
      output$out_table_metrics = renderDataTable({

        dd <-my_graph_metrics[c("name","betweenness")]

        dd[order(-dd$betweenness),]
      }, options = list(lengthMenu = c(10, 20, 50,100), pageLength = 10))
    }

    if(input$in_tp_metrica == "eccentricity") { 
      output$out_table_metrics = renderDataTable({

        dd <-my_graph_metrics[c("name","eccentricity")]

        dd[order(-dd$eccentricity),]
      }, options = list(lengthMenu = c(10, 20, 50,100), pageLength = 10))
    }

  })
  


} 
##
# sem isso nao consegue carregar graficos do diretorio www
# os arquivos de imagem precisam estar com permissao 664
#shinyAppDir(".")
shinyAppDir(".")

shinyApp(ui = ui, server = server)

