library(shiny)

ui <- basicPage(
	fluidRow(
		column(3,
			fileInput("input_file", "Arquivo WoS", multiple = FALSE, accept = NULL, width = NULL, buttonLabel = "Selecione...", placeholder = "No file selected")
		) ,
		column(9,
			flowLayout(
				actionButton("show0", "Exibir painel de mensagens"),
				actionButton("show1", "Exibir painel de mensagens"),
				actionButton("show2", "Exibir painel de mensagens"),
				actionButton("show3", "Exibir painel de mensagens"),
				actionButton("show4", "Exibir painel de mensagens"),
				actionButton("show5", "Exibir painel de mensagens")
			)
		)
	)
) # the user interface

server <- function(input, output, session) { 
    observeEvent(input$show0, {
      showModal(modalDialog(
        title = "Saida de Dados para o evento 0",
        "Dados para Exibicao ",
        easyClose = TRUE,
        footer = NULL
      ))
    })

    observeEvent(input$show1, {
      showModal(modalDialog(
        title = "Saida de Dados para o evento 1 ",
        "Dados para Exibicao ",
        easyClose = TRUE,
        footer = NULL
      ))
    })

    observeEvent(input$show2, {
      showModal(modalDialog(
        title = "Saida de Dados para o evento 2 ",
        "Dados para Exibicao ",
        easyClose = TRUE,
        footer = NULL
      ))
    })


} #the server

shinyApp(ui = ui, server = server)  
