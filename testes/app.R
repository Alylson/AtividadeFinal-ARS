library(shiny)
ui <- fluidPage(
  titlePanel("Workflow Analise de Redes"),
  tabsetPanel(
      tabPanel("Definição da rede", 
        sidebarLayout( 
          sidebarPanel(
             sliderInput(inputId = "num", 
               label = "Choose a number", 
               value = 25, min = 1, max = 100)
      
          ),
          mainPanel(
            plotOutput("hist")
          )
        )
      ) ,

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