ui <- fluidPage(
  sliderInput(inputId = "num",
              label = "Escolha a quantidade de nos",
              value = 25, min = 1, max = 100),
  
  actionButton(inputId = "barabasi", label = "Barabasi" ),
  
  actionButton(inputId = "watts", label = "Watts & Strogatz"),
  
  actionButton(inputId = "erdos", label = "Erdos-Renyi"),

  plotOutput("rede"),
  
  verbatimTextOutput("path"),
  
  verbatimTextOutput("diametro"),
  
  verbatimTextOutput("transit"),
  
  verbatimTextOutput("assort")
  
)

server <- function(input, output) {
  
  library(igraphinshiny)

  dados <- reactiveValues(game = sample_pa(25, directed = FALSE))
  
  observeEvent(input$barabasi, {dados$game <- sample_pa(input$num, directed = FALSE)})
  
  observeEvent(input$watts, {dados$game <- watts.strogatz.game(1, input$num, 5, 0)})

  observeEvent(input$erdos, {dados$game <- sample_gnp(input$num, 0.3, directed = FALSE, loops = FALSE)})
    
  output$path <- renderPrint({print("Caminho Medio"); average.path.length(dados$game)  
  })
  
  output$diametro <- renderPrint({print("Diametro"); diameter(dados$game)  
  })
  
  output$transit <- renderPrint({print("Transitividade"); transitivity(dados$game)  
  })
  
  output$assort<- renderPrint({print("Coeficiente de Assortatividade"); assortativity.degree(dados$game)  
  })
  
  output$rede <- renderPlot({
    plot.igraph(dados$game, vertex.label=NA)
  })
  
  }

shinyApp(ui =ui, server = server)