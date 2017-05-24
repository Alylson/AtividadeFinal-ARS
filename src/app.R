library(shiny)
ui <- fluidPage(
  titlePanel("Atividade Final da Disciplina Analise de Redes Sociais com R - Professor Ricardo Barros"),
 
  tabsetPanel(
        tabPanel("Apresentação", 
           fluidRow(
                   column(12, h3("Workflow Analise de Redes")),
                   column(12,h4()),
                   column(12),                   
                   column(12,h4("Grupo A:")),
                   column(12,h4("Leda Alves")),
                   column(12,h4("Lena Moraes")),
                   column(12,h4("Jeronimo Avelar Filho"))
           )         
                                   
                   
          ) ,           
      tabPanel("Definição da rede", 
        sidebarLayout( 
          sidebarPanel(
                  fluidRow(
                        column(12,
                               selectInput(
                                       "Selecione",
                                       label=h4("Base de dados a ser importada:"),
                                       choices=list(
                                               "Graphml" = 1 ,
                                               "Web of Science ou Scopus" = 2 ,
                                               "PubMed Web" = 3 
                                       ),
                                       selected = 1
                               ),
                               selectInput(
                                       "Selecione",
                                       label=h4("Formato dos dados:"),
                                       choices=list(
                                               "TXT" = 1 ,
                                               "CSV" = 2 
                                       ),
                                       selected = 1
                               ) ,                              
                               selectInput(
                                       "Selecione",
                                       label=h4("Tipo de Distribuicao de Rede:"),
                                       choices=list(
                                               "AAAAAA" = 1 ,
                                               "BBBBBB" = 2 
                                       ),
                                       selected = 1
                               )
                               
                        )
                  )
 
          ),
          mainPanel(
                h1("")  
          )
        )    
      ),


      tabPanel("Tratamento de dados", 
               sidebarLayout( 
                 sidebarPanel(
                   sliderInput(inputId = "num2", 
                               label = "Choose a new value", 
                               value = 25, min = 1, max = 100)
                   
                 ),
                 mainPanel(
                         plotOutput("hist2")      

                 )
               )
      ) ,
      tabPanel("Determinação de caracteristicas", 
               sidebarLayout( 
                       sidebarPanel(
                               sliderInput(inputId = "num3", 
                                           label = "Choose a new value", 
                                           value = 25, min = 1, max = 100)
                               
                       ),
                       mainPanel(
                               plotOutput("hist3")      
                               
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