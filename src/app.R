library(shiny)
library(bibliometrix)
#library(easyPubMed)
library(igraph)
library(igraphinshiny)
options(shiny.maxRequestSize=30*1024^2) 
options(shiny.port = 3000)
Sys.setenv(LANG = "pt_BR.UTF-8")

# Variaveis  ######
#trab_df = data.frame() 
#trab_analise_bbl = ""

my_papers_df = data.frame()          # VARIÁVEL QUE ARMAZENA O DATAFRAME PRINCIPAL CONSTRUÍDO PELO BIBLIOMETRIX
my_graph =""                         # VARIÁVEL QUE ARMAZENA O GRAFO GERADO 
my_results = ""                      # VARIAVEL QUE ARMAZENA ANALISE FEITA PELO BIBLIOMETRIX
my_NetMatrix = ""
my_graph_metrics = data.frame()
#output_df = matrix()

# Funcoes    ######

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
  my_papers_df <<- termExtraction(my_papers_df, Field = "TI", synonyms=my_synonyms, remove.numbers=TRUE,  
                                  remove.terms=my_remove.terms, keep.terms=my_keep.terms, verbose=FALSE)
  my_papers_df <<- termExtraction(my_papers_df, Field = "AB", synonyms=my_synonyms, remove.numbers=TRUE,
                                  remove.terms=my_remove.terms, keep.terms=my_keep.terms, verbose=FALSE)
  my_papers_df <<- termExtraction(my_papers_df, Field = "DE", synonyms=my_synonyms, remove.numbers=TRUE,
                                  remove.terms=my_remove.terms, keep.terms=my_keep.terms, verbose=FALSE)
  my_papers_df <<- termExtraction(my_papers_df, Field = "ID", synonyms=my_synonyms, remove.numbers=TRUE,
                                  remove.terms=my_remove.terms, keep.terms=my_keep.terms, verbose=FALSE)
}

CriaAnaliseBibliometrica = function() { 
  my_results <<- biblioAnalysis(my_papers_df, sep = ";")
}

QuantidadePublicacoes = function(){ length(my_papers_df[[1]]) }

TransformaDataframeEmGrafo = function(my_analysis, my_network, my_netDegree){
  my_NetMatrix <<-""
  my_graph <<-""
  my_NetMatrix <- biblioNetwork(my_papers_df, analysis = my_analysis, network = my_network, sep = ";")
  diag <- Matrix::diag 
  my_NetMatrix <-as.matrix( my_NetMatrix[diag(my_NetMatrix) >= my_netDegree,diag(my_NetMatrix) >= my_netDegree])
  #diag(my_NetMatrix) <- 0
  #my_NetMatrix <<-my_NetMatrix
  #my_graph <<- graph.adjacency(my_NetMatrix)
  #my_graph <<- graph.adjacency(my_NetMatrix,mode = "directed")
  my_graph <<-graph_from_adjacency_matrix(my_NetMatrix, weighted=TRUE,mode = "directed")
  my_NetMatrix <<-my_NetMatrix
  my_graph.description <- paste ("Network of",my_analysis,"of",my_network, "with threshold:", my_netDegree,"G=(", vcount(my_graph), ",", ecount(my_graph), ").")
  my_graph.description
}

CalculaMedidasCentralidade = function(){
  
  my_graph_metrics<- data.frame(V(my_graph)$name)
  my_graph_metrics["name"]<- data.frame(V(my_graph)$name)
  # Grau, grau de entrada e grau de saída
  V(my_graph)$degree <- c(degree(my_graph))
  my_graph_metrics["degree"]<-(V(my_graph)$degree)
  my_graph.degree.summary <- summary(V(my_graph)$degree)
  my_graph.results.degree.sd <-sd(V(my_graph)$degree)
  my_graph.results.degree.var <-var(V(my_graph)$degree)
  
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
  V(my_graph)$strengh <- c(graph.strength(my_graph))
  my_graph_metrics["strengh"]<- (V(my_graph)$strengh)
  my_graph.strengh.summary <<-summary(V(my_graph)$strengh)
  my_graph.strengh.sd <<-sd(V(my_graph)$strengh)
  
  # Força de entrada
  V(my_graph)$instrengh <- c(graph.strength(my_graph, mode =c("in")))
  my_graph_metrics["instrengh"]<- (V(my_graph)$instrengh)
  my_graph.instrengh.summary <<-summary(V(my_graph)$instrengh )
  my_graph.instrengh.sd <<-sd(V(my_graph)$instrengh )
  my_graph.instrengh.var <<-var(V(my_graph)$instrengh)
  
  # Força de saída
  V(my_graph)$outstrengh <- c(graph.strength(my_graph, mode =c("out")))
  my_graph_metrics["outstrengh"]<- (V(my_graph)$outstrengh)
  my_graph.outstrengh.summary <<-summary(V(my_graph)$outstrengh)
  my_graph.outstrengh.sd <<-sd(V(my_graph)$outstrengh)
  
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
  my_graph_metrics["closeness"]<-(V(my_graph)$closeness)
  my_graph.closeness <<- centralization.closeness(my_graph)
  my_graph.closeness.res.sumary <<- summary (my_graph.closeness$res)
  my_graph.closeness.res.sd <<-sd(my_graph.closeness$res)
  
  # 4.Intermediação
  V(my_graph)$betweenness <- betweenness(my_graph)
  my_graph_metrics["betweenness"]<- (V(my_graph)$betweenness)
  my_graph.betweenness <<- centralization.betweenness(my_graph)
  my_graph.betweenness.res.sumary <<-summary (my_graph.betweenness$res)
  my_graph.betweenness.res.sd <<-sd(my_graph.betweenness$res)
  
  # 4.Excentricidade
  V(my_graph)$eccentricity <-eccentricity(my_graph)
  my_graph_metrics["eccentricity"]<- (V(my_graph)$eccentricity)
  my_graph.eccentricity.sumary <<- summary (V(my_graph)$eccentricity)
  my_graph.eccentricity.sd <<-sd(V(my_graph)$eccentricity)
  
  # 4.eigen_centrality
  my_graph.eigen <<-eigen_centrality(my_graph)
  
  # 4.Densidade
  my_graph.density <<-graph.density(my_graph)
  
  # Modularidade
  wtc <- cluster_walktrap(my_graph)
  #modularity(wtc)
  my_graph.modularity <<-modularity(my_graph, membership(wtc))
  my_graph.modularity.matrix <<-modularity_matrix(my_graph,membership(wtc))
  
  # Page Rank
  V(my_graph)$pagerank <-(page.rank(my_graph))$vector
  my_graph_metrics["pagerank"]<- (V(my_graph)$pagerank)
  #Clusterring
  V(my_graph)$clustering <-(clusters(my_graph))$membership
  my_graph_metrics$clustering<- data.frame(V(my_graph)$clustering)
  my_graph_metrics<<-my_graph_metrics
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
  titlePanel("Analise de Redes Sociais com R - Professor Ricardo Barros"),
  
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
               column(6,
                      h3("Atividade Final da Disciplina"),
                      p("Objetivo: Lorem ipsum dolor sit amet, consectetur adipiscing elit. Pellentesque porttitor nibh ligula, in mattis massa fringilla nec. Curabitur luctus mauris ac mauris venenatis ullamcorper. Duis porta urna mauris, quis varius est aliquam id. Morbi lacinia odio ut nisl posuere cursus eu at dui. Aenean neque lorem, commodo ut pharetra vel, pellentesque ut nisi. Etiam ullamcorper facilisis tortor vitae suscipit. Sed bibendum ex vitae ultricies aliquet."),
                      br(),
                      br(),
                      br(),
                      h3("Componentes do Grupo:"),
                      p("LA"),
                      p("LM"),
                      p("JAF")
               ) ,
               column(3,"")
             )
             
    ) ,           
    tabPanel("Informações Bibliométricas", 
      sidebarPanel( 
        fileInput("arqtrab", "Selecione arquivo WoS", multiple = FALSE, accept = NULL, width = NULL,
        buttonLabel = "Browse...", placeholder = "No file selected") ,
        textOutput('dadosArquivo'),  
        actionButton("show0", "Publicaçoes referenciadas" , class = "btn-primary btn-block"  ),
        actionButton("show1", "Autores Citados", class = "btn-primary  btn-block"  ),
        actionButton("show2", "Palavras Chaves Utilizadas", class = "btn-primary  btn-block"  ),
        actionButton("show3", "Linguas Utilizadas Para Publicacao" , class = "btn-primary  btn-block" ),
        actionButton("show4", "Periodicos Utilizados Para Publicação" , class = "btn-primary  btn-block" ),
        actionButton("show5", "Tipos de Publicação Utilizados" , class = "btn-primary  btn-block" )

      ) ,
      mainPanel(    
        dataTableOutput('painel_1')
         
      )    
         

            
    ),
    
    
    tabPanel("Tratamento de dados",
             sidebarLayout(
               sidebarPanel(column(12,
                                   radioButtons("in_tp_analysis", "Escolha o tipo de análise:",
                                                c("Colaboração de AUTORES" = "collaborationAuthors",
                                                  "Colaboração de PAÍSES" = "collaborationCountries",
                                                  "Colaboração de INSTITUIÇÕES" = "collaborationUniversities",
                                                  "Citação DOCUMENTOS " = "couplingReferences",
                                                  "Citação AUTORES " = "couplingAuthors",
                                                  "Citação FONTES " = "couplingSources",
                                                  "Citação PAÍSES " = "couplingCountries",
                                                  "Co-citação DOCUMENTOS " = "co-citationReferences",
                                                  "Co-citação AUTORES " = "co-citationAuthors",
                                                  "Co-citação FONTES " = "co-citationSources",
                                                  "Co-occurrences AUTORES" = "co-occurrencesAuthors",
                                                  "Co-occurrences FONTES" = "co-occurrencesSources",
                                                  "Co-occurrences PALAVRAS-CHAVE" = "co-occurrencesKeywords",
                                                  "Co-occurrences PALAVRAS-CHAVE AUTOR" = "co-occurrencesAuthor_keywords",
                                                  "Co-occurrences TÍTULO " = "co-occurrencesTitles",
                                                  "Co-occurrences RESUMO " = "co-occurrencesAbstracts")),
                                   textOutput('out_tp_analysis'),
                                   actionButton("in_cria_rede", "Gera Rede"),
                                   "Teste1")
                 
               ),
               mainPanel(
                 tabsetPanel(
                   # tabPanel("Plot",
                   #          column(,
                   #                 plotOutput("out_plot_net"),
                   #                 verbatimTextOutput("info"),
                   #                 dataTableOutput('mytable')
                   #          )
                   # ),
                   tabPanel("Plot",plotOutput("out_plot_net")),
                   tabPanel("Table", dataTableOutput('mytable')),
                   tabPanel("Summary",verbatimTextOutput("info")) 
                   #tabPanel("Table", dataTableOutput('mytable'))
                 )
                 # verticalLayout(plotOutput("out_plot_net"),
                 #             verbatimTextOutput("info"),
                 #             tableOutput('mytable')
                 # )
             )
          )
    ) ,
    
    tabPanel("Determinação de caracteristicas",
             sidebarLayout(
               sidebarPanel(column(12,
                                   radioButtons("in_tp_metrica", "Escolha a métrica:",
                                                c("Grau médio" = "netdegree",
                                                  "Grau ponderado médio" = "strength",
                                                  "Diâmetro da rede" = "diameter",
                                                  "Densidade do grafo" = "density",
                                                  "Modularidade" = "modularity",
                                                  "PageRank" = "pagerank",
                                                  "Componentes conectados" = "collaborationUniversities",
                                                  "Coeficiente de clustering médio" = "clustering",
                                                  "Centralidade de autovetor" = "eigen",
                                                  "Comprimento médio de caminho" = "strength")),
                                                #"Componentes conectados" = "collaborationUniversities",
                                                #"Coeficiente de clustering médio" = "strength",
                                                #"Centralidade de autovetor" = "collaborationUniversities",
                                   textOutput('out_tp_metrica'),
                                   "Teste2")
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Plot",plotOutput("out_plot_degree")), 
                   tabPanel("Summary", plotOutput("out_plot_indegree")), 
                   tabPanel("Table", plotOutput("out_plot_boxdegree"))
                 )
                 # verticalLayout(plotOutput("out_plot_degree"),
                 #            plotOutput("out_plot_indegree"),
                 #            plotOutput("out_plot_outdegree"),
                 #            plotOutput("out_plot_boxdegree")
                 #            #verbatimTextOutput("info"),
                 #            #tableOutput('mytable')
                 # )
             )
          )
             
    ) ,
    
    tabPanel("Inspeção visual"
             
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
    
    my_analysis <<- "collaboration"
    my_network <<- "authors"
    my_netDegree <<- 1 
    TransformaDataframeEmGrafo(my_analysis, my_network, my_netDegree)
    CalculaMedidasCentralidade()
    #mudando de posiçao para ser a ultima mensagem a ser exibida
    print(paste("Quantidade de Publicações lidas: ", QuantidadePublicacoes()))
    
  })

  # Analise Bibliometrica
  observeEvent(input$show0, {
    output$painel_1 = renderDataTable(
     PublicacoesMaisReferenciadas() 
     , options = list(lengthMenu = c(10, 10, 50,100), pageLength = 10)
    )
  })

  observeEvent(input$show1, {
    output$painel_1 =  renderDataTable(
     AutoresMaisCitados()  
     , options = list(lengthMenu = c(10, 10, 50,100), pageLength = 10) 
    )
  })  
  
 observeEvent(input$show2, {
    output$painel_1 =  renderDataTable(
     PalavrasChavesMaisUtilizadas()  
     , options = list(lengthMenu = c(10, 10, 50,100), pageLength = 10) 
    )
  })  
 
 observeEvent(input$show3, {
    output$painel_1 =  renderDataTable(
     LinguasPublicacao()
     , options = list(lengthMenu = c(10, 10, 50,100), pageLength = 10) 
    )
  })  
 
 observeEvent(input$show4, {
    output$painel_1 =  renderDataTable(
     PeriodicosQueMaisPublicam()
     , options = list(lengthMenu = c(10, 10, 50,100), pageLength = 10) 
    )
  })    

 observeEvent(input$show5, {
    output$painel_1 =  renderDataTable(
     TiposDePublicacao()
     , options = list(lengthMenu = c(10, 10, 50,100), pageLength = 10) 
    )
  })  
  # Fim Analise Bibliometrica

  output$out_tp_analysis = renderText({
    input$in_tp_analysis
  })
  
 # observeEvent(input$in_cria_rede, )
  
  re <- eventReactive(input$in_cria_rede, {input$in_tp_analysis})
  
    #input$go,{input$a})
  output$b <- renderText({
    re()
  })
              
  output$info <- renderText({
    print(paste("Network of",my_analysis,"of",my_network, "with threshold:", my_netDegree,"G=(", vcount(my_graph), ",", ecount(my_graph), ")."))
    # paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })

  output$mytable = renderDataTable({
    # dd[ order(-dd[,4], dd[,1]), ]
    dd <-my_graph_metrics[c("name","degree","indegree","outdegree")]
    dd[order(-dd$degree),]
   }, options = list(lengthMenu = c(10, 10, 50,100), pageLength = 10))

  output$out_tp_metrica <- renderText({
    input$in_tp_metrica
  })
  
  output$out_plot_net <- renderPlot({
    if (input$in_tp_analysis=="collaborationAuthors"){
      my_analysis <- "collaboration"
      my_network <- "authors"
    }
    if (input$in_tp_analysis=="collaborationCountries"){
      my_analysis <- "collaboration"
      my_network <- "countries"
    }
    if (input$in_tp_analysis=="collaborationUniversities"){
      my_analysis <- "collaboration"
      my_network <- "universities"
    }
    if (input$in_tp_analysis=="couplingReferences"){
      my_analysis <- "coupling"
      my_network <- "references"
    }
    if (input$in_tp_analysis=="couplingAuthors"){
      my_analysis <- "coupling"
      my_network <- "authors"
    }
    if (input$in_tp_analysis=="couplingSources"){
      my_analysis <- "coupling"
      my_network <- "sources"
    }
    if (input$in_tp_analysis=="couplingCountries"){
      my_analysis <- "coupling"
      my_network <- "countries"
    }
    if (input$in_tp_analysis=="co-citationReferences"){
      my_analysis <- "co-citation"
      my_network <- "references"
    }
    if (input$in_tp_analysis=="co-citationAuthors"){
      my_analysis <- "co-citation"
      my_network <- "authors"
    }
    if (input$in_tp_analysis=="co-citationSources"){
      my_analysis <- "co-citation"
      my_network <- "sources"
    }
    if (input$in_tp_analysis=="co-occurrencesAuthors"){
      my_analysis <- "co-occurrences"
      my_network <- "authors"
    }
    if (input$in_tp_analysis=="co-occurrencesSources"){
      my_analysis <- "co-occurrences"
      my_network <- "sources"
    }
    if (input$in_tp_analysis=="co-occurrencesKeywords"){
      my_analysis <- "co-occurrences"
      my_network <- "keywords"
    }
    if (input$in_tp_analysis=="co-occurrencesAuthor_keywords"){
      my_analysis <- "co-occurrences"
      my_network <- "author_keywords"
    }
    if (input$in_tp_analysis=="co-occurrencesTitles"){
      my_analysis <- "co-occurrences"
      my_network <- "titles"
    }
    if (input$in_tp_analysis=="co-occurrencesAbstracts"){
      my_analysis <- "co-occurrences"
      my_network <- "abstracts"
    }
    my_analysis <<- my_analysis
    my_network <<- my_network
    my_netDegree <<- 5
    TransformaDataframeEmGrafo(my_analysis, my_network, my_netDegree)
    CalculaMedidasCentralidade()
    #networkPlot(my_NetMatrix, n = 30, Title = paste (c(my_analysis),c(my_network)), type = "fruchterman", size=FALSE, remove.multiple=TRUE)
    # Usando a função do Igraph
    #deg <- degree(net, mode="all")
    V(my_graph)$size <- my_graph_metrics$degree/5
    # We could also use the audience size value:
    #V(net)$size <- V(net)$audience.size*0.6
    plot(my_graph,edge.arrow.size=.1, edge.curved=.1,layout = layout.sphere, #layout = layout.sphere, #layout.sphere #layout.fruchterman.reingold,
         vertex.size = V(my_graph)$size,
         vertex.color="orange",vertex.frame.color = 'blue',
         vertex.label.dist = 0.5,
         vertex.label.color = 'black',
         edge.color="gray",
         vertex.label.font = 1, vertex.label = V(my_graph)$name, vertex.label.cex = 0.7)
    
  })
  
  output$out_plot_degree <- renderPlot({
    if (input$in_tp_metrica=="netdegree"){
      hist(V(my_graph)$degree,col="lightblue",xlim=c(0, max(V(my_graph)$degree)),xlab="Grau dos vértices", ylab="Frequência", main="", axes="TRUE")
      legend("topright", c(paste("Mín.=", round(my_graph.degree.summary[1],2)),
                           paste("Máx.=", round(my_graph.degree.summary[6],2)),
                           paste("Média=", round(my_graph.degree.summary[4],2)),
                           paste("Mediana=", round(my_graph.degree.summary[3],2)),
                           paste("D. Padrão=", round(my_graph.degree.sd[1],2))),
             pch = 1, title = "Grau")
    }
  })
  
  output$out_plot_indegree <- renderPlot({
    if (input$in_tp_metrica=="netdegree"){
      hist(V(my_graph)$indegree,col="lightblue", xlab="Grau de entrada", ylab="Frequência", main="", axes="TRUE")
      legend("topright", c(paste("Mínimo =", round(my_graph.indegree.summary[1],2)), 
                           paste("Máximo=", round(my_graph.indegree.summary[6],2)), 
                           paste("Média=", round(my_graph.indegree.summary[4],2)),
                           paste("Mediana=", round(my_graph.indegree.summary[3],2)),
                           paste("D. Padrão=", round(my_graph.indegree.sd[1],2))),
             pch = 1, title = "Grau entrada")
    }
  })
  
  output$out_plot_outdegree <- renderPlot({
    if (input$in_tp_metrica=="netdegree"){
      hist(V(my_graph)$outdegree,col="lightblue", xlab="Grau de saída", ylab="Frequência", main="", axes="TRUE")
      legend("topright", c(paste("Mínimo =", round(my_graph.outdegree.summary[1],2)), 
                           paste("Máximo=", round(my_graph.outdegree.summary[6],2)), 
                           paste("Média=", round(my_graph.outdegree.summary[4],2)),
                           paste("Mediana=", round(my_graph.outdegree.summary[3],2)),
                           paste("D.Padrão=", round(my_graph.outdegree.sd[1],2))),
             pch = 1, title = "Grau saída")
    }
  })
  
  output$out_plot_boxdegree <- renderPlot({
    if (input$in_tp_metrica=="netdegree"){
      boxplot(V(my_graph)$indegree, V(my_graph)$outdegree, V(my_graph)$degree, notch = FALSE, ylab = 'Grau', 
              names = c('Grau entrada', 'Grau saída', 'Grau total'), 
              main = '', col = c('blue', 'red', 'orange'),shrink=0.8, textcolor="red")
    }
  })

}
# sem isso nao consegue carregar graficos do diretorio www
# os arquivos de imagem precisam estar com permissao 664
#shinyAppDir(".")
shinyAppDir(".")

shinyApp(ui = ui, server = server)

