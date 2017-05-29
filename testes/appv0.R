library(shiny)
library(bibliometrix)
library(easyPubMed)
library(igraph)

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
            submitButton(text = "Executar", icon = NULL, width = NULL)
          ),
          column(9,
            plotOutput("rede")
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
output$rede <- ({      
    query_string <- input$in_query_string 
    on_pubmed <- get_pubmed_ids(query_string)
    papers <- fetch_pubmed_data(on_pubmed, format = input$in_formato_dados)
    papers_list <- articles_to_list(papers)
    #Transformar Lista em Data Frame
    papers_df<-c()
    for (i in 1:10){
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
  })

  ##output$texto <- renderText(input$query_string )





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
