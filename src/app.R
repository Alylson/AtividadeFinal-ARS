library(shiny)
library(bibliometrix)
#library(easyPubMed)
library(igraph)
library(igraphinshiny)
options(shiny.maxRequestSize=30*1024^2) 
options(shiny.port = 3000)

# Variaveis  ######
#trab_df = data.frame() 
#trab_analise_bbl = ""

my_papers_df = data.frame()          # VARIÁVEL QUE ARMAZENA O DATAFRAME PRINCIPAL CONSTRUÍDO PELO BIBLIOMETRIX
my_graph =""                         # VARIÁVEL QUE ARMAZENA O GRAFO GERADO 
my_results = ""                      # VARIAVEL QUE ARMAZENA ANALISE FEITA PELO BIBLIOMETRIX
my_NetMatrix = ""
output_df = data.frame() 
# Funcoes    ######

###  Analise Bibliomtrica
PublicacoesMaisReferenciadas = function (qtd=10){
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
    #saida = paste(saida, "<tr><td>",i,"</td><td>" ,publicacoes[j], "</td><td>" ,publicacoes[j+1], "</td></tr>")
    print(i)
    j=j+2
    i=i+1
  }
  print(summary(output_df))
  output_df <<- data.frame(publicacao,qtd_citacoes)
  output_df 
    
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

CriaAnaliseBibliometrica = function() { 
  my_results <<- biblioAnalysis(my_papers_df, sep = ";")


}

QuantidadePublicacoes = function(){ length(my_papers_df[[1]]) }

CalculaMedidasCentralidade = function(){
  
  # Grau = grau de entrada + grau de saída
  my_graph.degree <<- degree(my_graph)
  my_graph.degree.summary <<-summary(my_graph.degree)
  my_graph.degree.sd <<-sd(my_graph.degree)
  my_graph.degree.var <<-var(my_graph.degree)
  
  # Grau de entrada
  my_graph.indegree <<- degree(my_graph, mode = c("in"))  #mode = c("all", "out", "in", "total")
  my_graph.indegree.summary <<-summary(my_graph.indegree)
  my_graph.indegree.sd <<-sd(my_graph.indegree)
  my_graph.indegree.var <<-var(my_graph.indegree)
  
  # Grau de saida 
  my_graph.outdegree <<- degree(my_graph, mode = c("out")) #mode = c("all", "out", "in", "total")
  my_graph.outdegree.summary <<-summary(my_graph.outdegree)
  my_graph.outdegree.sd <<-sd(my_graph.outdegree)
  my_graph.outdegree.var <<-var(my_graph.outdegree)
  
  # 4.2 Força dos vértices
  # Força = força de entrada + força de saída
  my_graph.strengh <<-graph.strength(my_graph)
  my_graph.strengh.summary <<-summary(my_graph.strengh)
  my_graph.strengh.sd <<-sd(my_graph.strengh)
  
  # Força de entrada
  my_graph.instrengh <<- graph.strength(my_graph, mode =c("in"))
  my_graph.instrengh.summary <<-summary(my_graph.instrengh)
  my_graph.instrengh.sd <<-sd(my_graph.instrengh)
  
  # Força de saída
  my_graph.outstrengh <<- graph.strength(my_graph, mode =c("out"))
  my_graph.outstrengh.summary <<-summary(my_graph.outstrengh)
  my_graph.outstrengh.sd <<-sd(my_graph.outstrengh)
  
  # 4.7 log log
  my_graph.degree.distribution <<- degree.distribution(my_graph)
  my_d <<- 1:max(my_graph.degree)-1
  my_ind <<- (my_graph.degree.distribution != 0) 
  
  #4.8 - knn Calculate the average nearest neighbor degree of the given vertices and the same quantity in the function of vertex degree
  #my_graph.a.nn.deg <<- graph.knn(my_graph,V(my_graph))$knn
  
  # Diameter - distância geodesica
  my_graph.diameter <<-diameter(my_graph, directed = TRUE, unconnected=TRUE, weights = NULL)
  
  ##Retorna os caminho com diametro atual
  my_graph.get_diameter <<-get_diameter(my_graph)
  
  ##Retorna os 2 vértices que são conectados pelo diâmetro
  my_graph.farthest_vertices <<-farthest_vertices(my_graph)
  
  # 4.Proximidade - A centralidade de proximidade mede quantas etapas são necessárias para acessar cada outro vértice de um determinado vértice.
  my_graph.closeness <<- closeness(my_graph)
  my_graph.closeness <<- centralization.closeness(my_graph)
  my_graph.closeness.res.sumary <<- summary (my_graph.closeness$res)
  my_graph.closeness.res.sd <<-sd(my_graph.closeness$res)
  
  # 4.Intermediação
  my_graph.betweenness <<- betweenness(my_graph)
  my_graph.betweenness <<- centralization.betweenness(my_graph)
  my_graph.betweenness.res.sumary <<-summary (my_graph.betweenness$res)
  my_graph.betweenness.res.sd <<-sd(my_graph.betweenness$res)
  
  # 4.Excentricidade
  my_graph.eccentricity <<-eccentricity(my_graph)
  my_graph.eccentricity.sumary <<- summary (my_graph.eccentricity)
  my_graph.eccentricity.sd <<-sd(my_graph.eccentricity)
  
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
  my_graph.pagerank <<-page.rank(my_graph)
  
  #Clusterring
  my_graph.clustering <<-clusters(my_graph)
  
}

##ConvArqParaDataFrame = function(entrada){
##  trab_df <<- convert2df(entrada, dbsource="isi", format="plaintext")
##  trab_analise_bbl <<- biblioAnalysis(trab_df, sep = ";")
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
      tabPanel("Definição da rede", 
        sidebarPanel( 
          fileInput("arqtrab", "Selecione arquivo WoS", multiple = FALSE, accept = NULL, width = NULL,
          buttonLabel = "Browse...", placeholder = "No file selected") ,
          textOutput('dadosArquivo'),  
          actionButton("show0", "Publicaçoes mais referenciadas" , class = "btn-primary btn-block"  ),
          actionButton("show1", "Exibir painel de mensagens", class = "btn-primary  btn-block"  ),
          actionButton("show2", "Exibir painel de mensagens", class = "btn-primary  btn-block"  ),
          actionButton("show3", "Exibir painel de mensagens" , class = "btn-primary  btn-block" )
        ) ,
        mainPanel(    
          tableOutput('painel_1') 
        )    
           

              
      ),


      tabPanel("Tratamento de dados"

      ) ,

      tabPanel("Determinação de caracteristicas" 

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

    print(paste("Quantidade de Publicações lidas: ", QuantidadePublicacoes()))
  })

  ##output$mytable = renderTable({
  ##  as.table(sort(my_graph.degree,decreasing = TRUE)[1:100])
  ##})

  observeEvent(input$show0, {
    PublicacoesMaisReferenciadas() 
    print(output_df)

    output$painel_1 = renderDataTable({ 
     output_df 
    })
    

  })

  observeEvent(input$show1, {
    showModal(modalDialog(
      title = "Saida de Dados para o evento 1 ",
      "Dados para Exibicao ",
      easyClose = TRUE,
      footer = NULL
    ))
  })


}
# sem isso nao consegue carregar graficos do diretorio www
# os arquivos de imagem precisam estar com permissao 664
shinyAppDir(".")

shinyApp(ui = ui, server = server)
