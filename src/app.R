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
              "origem_dados",
              label=h4("Origem dos dados"),
              choices=list(
                "Graphml" = "graphml" ,
                "Web of Science ou Scopus" = "wos" , 
                "PubMed Web" = "pubmed" 
              ),
              selected = 1 
            ),
            selectInput(
              "formato_dados",
              label = h4("Formato dos dados"),
              choices = list(
                "XML" = "xml" ,
                "TXT" = "txt" ,
                "CSV" = "csv" 
              ),
              selected = 3 
            ),
            textAreaInput("query_string", "Texto para pesquisa", "", height = "50px")
          ),
          column(9,
            h4(textOutput("texto"))
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
  output$texto <- renderText(input$query_string )
##




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
