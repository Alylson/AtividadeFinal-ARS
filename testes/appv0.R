library(shiny)
library(bibliometrix)
#library(easyPubMed)
library(igraph)
library(igraphinshiny)

ui <- fluidPage(
  titlePanel("Atividade Final da Disciplina Analise de Redes Sociais com R - Professor Ricardo Barros"),
 
  tabsetPanel(
        tabPanel("Apresentação", 
           fluidRow(
                   column(12, h3("Workflow Analise de Redes")),
                   column(12,h4()),
                   column(12),                   
                   column(12,h4("Grupo A:")),
                   column(12,h4("LA")),
                   column(12,h4("LM")),
                   column(12,h4("JAF"))
           )         
                                   
                   
          ) ,           
      tabPanel("Definição da rede", 
        fluidRow(
          column(3,
            selectInput(
              "in_origem_dados",
              label=h4("Origem dos dados"),
              choices=list(
                "Graphml" = "graphml" ,
                "Web of Science ou Scopus" = "wos" , 
                "PubMed Web" = "pubmed" 
              ),
              selected = 1 
            ),
            selectInput(
              "in_formato_dados",
              label = h4("Formato dos dados"),
              choices = list(
                "XML" = "xml" ,
                "TXT" = "txt" ,
                "CSV" = "csv" 
              ),
              selected = 3 
            ),
            textAreaInput("in_query_string", "Texto para pesquisa", "", height = "50px") ,
            actionButton(inputId = "submit", label = "Executar")
          ),
          column(9,
            plotOutput("grafico1")
          )

        )     
      ),


      tabPanel("Tratamento de dados", 
               sidebarLayout( 
                 sidebarPanel(
                 ),
                 mainPanel(
                 )
               )
      ) ,
      tabPanel("Determinação de caracteristicas", 
               sidebarLayout( 
                       sidebarPanel(
                               
                       ),
                       mainPanel(
                               
                       )
               )
      ) ,
      tabPanel("Inspeção visual", 
               sidebarLayout( 
                       sidebarPanel(
                               sliderInput(inputId = "num4", 
                                           label = "Choose a new value", 
                                           value = 25, min = 1, max = 100)
                               
                       ),
                       mainPanel(
                               plotOutput("hist4")      
                               
                       )
               )
      ) 
      
  )
)
server <- function(input, output) {

  ## Tab definicao da rede
  dados <- reactiveValues()
  setwd("/Users/jeronimo/J/projeto/Mestrado/AnaliseDeRedes/AtividadeFinal/AtividadeFinal-ARS/testes")
  dados$my_lines_papers <- readFiles("usec.txt")
  
  dados$my_dbsource = "isi" 
  dados$my_format = "plaintext" 

  
  
  

  observeEvent(input$submit, {
    dados$my_papers_df<-convert2df(dados$my_lines_papers, dbsource=dados$my_dbsource, format=dados$my_format) #definir formato e font
    # Extraindo informa??es adicionais que n?o s?o padr?o da Web Of Science e Scopus.
    # As informa??es s?o extra?das usando a fun??o metaTagExtraction
    # Authors' countries (Field = "AU_CO");
    dados$my_papers_df <- metaTagExtraction(dados$my_papers_df, Field = "AU_CO", sep = ";")
    # First author of each cited reference (Field = "CR_AU")
    dados$my_papers_df <- metaTagExtraction(dados$my_papers_df, Field = "CR_AU", sep = ";")
    # Publication source of each cited reference (Field = "CR_SO")
    dados$my_papers_df <- metaTagExtraction(dados$my_papers_df, Field = "CR_SO", sep = ";")
    # and Authors' affiliations (Field = "AU_UN")
    dados$my_papers_df <- metaTagExtraction(dados$my_papers_df, Field = "AU_UN", sep = ";")
          
    dados$results   <- biblioAnalysis(dados$my_papers_df, sep = ";")
    #my_results ##Imposs?vel de ler na tela.

    ##### A seguir s?o mostrados calculadas as medidas e listados os mais importantes.
    ##### Informe a quantidade de elementos que ser?o mostrados (Top Ten).
    # Esse n?mero ser? usado em todas as estat?sticas do bibliometrix abaixo.
    dados$my_num_k=20 ##

    # Functions summary and plot
    dados$my_S=summary(object = dados$results, k = dados$my_num_k, pause = FALSE)
  })

output$grafico1 <- renderPlot({      
  plot(x = dados$results, k = dados$my_num_k, pause = FALSE)

})


  output$hist <- renderPlot({
    hist(rnorm(input$num))
    })
  output$hist2 <- renderPlot({
          hist(rnorm(input$num2))
  })
  output$hist3 <- renderPlot({
          hist(rnorm(input$num3))
  })  
  output$hist4 <- renderPlot({
          hist(rnorm(input$num4))
  })  
}

shinyApp(ui = ui, server = server)
