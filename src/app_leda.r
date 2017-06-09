library(shiny)
library(shinydashboard)
library(graphics)
library(shinythemes)
library(igraph)

#ATENCAO: A INSPECAO VISUAL DESSE PROGRAMA SUPORTA REDES COM ATE 500 NOS#

########################INICIO FUNCTIONS FRUTCHERMAN#######################################

#FRUCHTERMAN SEM COMP_GIGANTE OU SUBGRUPO, SEM NOS DIMENSIONADOS, SEM LINHAS DIMENSIONADOS
PlotaFruchterman_SubNao_NoNao_LinhaNao = function() { 
  plotOutput(my_graph,edge.arrow.size=0.01, edge.color = "grey",edge.curved= 0,
       layout = layout.fruchterman.reingold,
       vertex.color="orange",vertex.frame.color = 'black',
       #vertex.label="",
       vertex.label.dist = 0,
       vertex.size = 5,
       vertex.label.color = 'black',
       vertex.label.font = 15, vertex.label = V(my_graph)$name, vertex.label.cex = 0.5)
}

#FRUCHTERMAN SEM COMP_GIGANTE OU SUBGRUPO, SEM NOS DIMENSIONADOS, COM LINHAS DIMENSIONADAS
PlotaFruchterman_SubNao_NoNao_LinhasSim = function() { 
  E(my_graph)$width <- E(my_graph)$weight/6
  plotOutput(my_graph, edge.color = "grey",edge.curved= 0,
       layout = layout.fruchterman.reingold,
       vertex.color="orange",vertex.frame.color = 'black',
       #vertex.label="",
       vertex.label.dist = 0,
       vertex.size = 5,
       vertex.label.color = 'black',
       vertex.label.font = 15, vertex.label = V(my_graph)$name, vertex.label.cex = 0.5)
}

#FRUCHTERMAN SEM COMP_GIGANTE OU SUBGRUPO, COM NOS DIMENSIONADOS, SEM LINHAS DIMENSIONADAS
PlotaFruchterman_SubNao_NoSim_LinhaNao = function() { 
  grau<-degree(my_graph, mode="all")  
  V(my_graph)$size<-grau*0.6 
  plotOutput(my_graph,edge.arrow.size=0.01, edge.color = "grey",edge.curved= 0,
       layout = layout.fruchterman.reingold,
       vertex.color="orange",vertex.frame.color = 'black',
       #vertex.label="",
       vertex.label.dist = 0,
       vertex.label.color = 'black',
       vertex.label.font = 15, vertex.label = V(my_graph)$name, vertex.label.cex = 0.5)
}


#FRUCHTERMAN SEM COMP_GIGANTE OU SUBGRUPO, COM NOS DIMENSIONADOS, COM LINHAS DIMENSIONADOS
PlotaFruchterman_SubNao_NoSim_LinhaSim = function() { 
  E(my_graph)$width <- E(my_graph)$weight/6
  grau<-degree(my_graph, mode="all")  
  V(my_graph)$size<-grau*0.6 
  plotOutput(my_graph, edge.color = "grey",edge.curved= 0,
       layout = layout.fruchterman.reingold,
       vertex.color="orange",vertex.frame.color = 'black',
       #vertex.label="",
       vertex.label.dist = 0,
       vertex.label.color = 'black',
       vertex.label.font = 15, vertex.label = V(my_graph)$name, vertex.label.cex = 0.5)
}


#FRUCHTERMAN COM SUBGRUPO AGREGADO, COM NOS E LINHAS DIMENSIONADOS
PlotaFruchterman_SubSim_NoSim_LinhaSim = function() { 
  E(my_graph)$width <- E(my_graph)$weight/6
   grau<-degree(my_graph, mode="all")  
  V(my_graph)$size<-grau*0.6 
  SCC <- clusters(my_graph, mode="strong")  
  V(my_graph)$color <- rainbow(SCC$no)[SCC$membership]
   plotOutput(my_graph, mark.groups = split(1:vcount(my_graph), SCC$membership),
        layout = layout.fruchterman.reingold,
      vertex.frame.color="black", edge.color="grey",
      vertex.label.font= 1)
  }

#FRUCHTERMAN COM SUBGRUPO AGREGADO, COM NOS DIMENSIONADOS E LINHAS NAO DIMENSIONADAS
PlotaFruchterman_SubSim_NoSim_LinhaNao = function() { 
  grau<-degree(my_graph, mode="all")  
  V(my_graph)$size<-grau*0.6 
  SCC <- clusters(my_graph, mode="strong")  
  V(my_graph)$color <- rainbow(SCC$no)[SCC$membership]
  plotOutput(my_graph, mark.groups = split(1:vcount(my_graph), SCC$membership),
       layout = layout.fruchterman.reingold,
       vertex.frame.color="black", edge.color="grey", edge.arrow.size=0.01,
       vertex.label.font= 1)
}

#FRUCHTERMAN COM SUBGRUPO AGREGADO, COM NOS NAO DIMENSIONADOS E LINHAS DIMENSIONADOS
PlotaFruchterman_SubSim_NoNao_LinhaSim = function() { 
  E(my_graph)$width <- E(my_graph)$weight/6
   grau<-degree(my_graph, mode="all")  
  V(my_graph)$size<-grau*0.6 
  SCC <- clusters(my_graph, mode="strong")  
  V(my_graph)$color <- rainbow(SCC$no)[SCC$membership]
  plotOutput(my_graph, mark.groups = split(1:vcount(my_graph), SCC$membership),
       layout = layout.fruchterman.reingold,
       vertex.frame.color="black", edge.color="grey",
       vertex.label.font= 1)
}

#FRUCHTERMAN COM SUBGRUPO AGREGADO, SEM NOS E LINHAS DIMENSIONADOS
PlotaFruchterman_SubSim_NoNao_LinhaNao = function() { 
  SCC <- clusters(my_graph, mode="strong")  
  V(my_graph)$color <- rainbow(SCC$no)[SCC$membership]
  plotOutput(my_graph, mark.groups = split(1:vcount(my_graph), SCC$membership),
       layout = layout.fruchterman.reingold,
       vertex.frame.color="black", edge.color="grey",
       vertex.label.font= 1)
}
########################FIM FUNCTIONS FRUTCHERMAN#######################################


########################INICIO FUNCTIONS KAMADA KAWAI#######################################

#KAWAI SEM SUBGRUPO AGREGADO, SEM NOS DIMENSIONADOS, SEM LINHAS DIMENSIONADOS
PlotaKAWAI_SubNao_NoNao_LinhaNao = function() { 
  plotOutput(my_graph,edge.arrow.size=0.01, edge.color = "grey",edge.curved= 0,
       layout = layout.kamada.kawai,
       vertex.color="orange",vertex.frame.color = 'black',
       #vertex.label="",
       vertex.label.dist = 0,
       vertex.size = 5,
       vertex.label.color = 'black',
       vertex.label.font = 15, vertex.label = V(my_graph)$name, vertex.label.cex = 0.5)
}

#KAWAI SEM SUBGRUPO AGREGADO, SEM NOS DIMENSIONADOS, COM LINHAS DIMENSIONADAS
PlotaKAWAI_SubNao_NoNao_LinhasSim = function() { 
  E(my_graph)$width <- E(my_graph)$weight/6
   plotOutput(my_graph, edge.color = "grey",edge.curved= 0,
       layout = layout.kamada.kawai,
       vertex.color="orange",vertex.frame.color = 'black',
       #vertex.label="",
       vertex.label.dist = 0,
       vertex.size = 5,
       vertex.label.color = 'black',
       vertex.label.font = 15, vertex.label = V(my_graph)$name, vertex.label.cex = 0.5)
}

#KAWAI SEM SUBGRUPO AGREGADO, COM NOS DIMENSIONADOS, SEM LINHAS DIMENSIONADAS
PlotaKAWAI_SubNao_NoSim_LinhaNao = function() { 
  grau<-degree(my_graph, mode="all")  
  V(my_graph)$size<-grau*0.6 
  plotOutput(my_graph,edge.arrow.size=0.01, edge.color = "grey",edge.curved= 0,
       layout = layout.kamada.kawai,
       vertex.color="orange",vertex.frame.color = 'black',
       #vertex.label="",
       vertex.label.dist = 0,
       vertex.label.color = 'black',
       vertex.label.font = 15, vertex.label = V(my_graph)$name, vertex.label.cex = 0.5)
}


#KAWAI SEM SUBGRUPO AGREGADO, COM NOS DIMENSIONADOS, COM LINHAS DIMENSIONADOS
PlotaKAWAI_SubNao_NoSim_LinhaSim = function() { 
  E(my_graph)$width <- E(my_graph)$weight/6
  grau<-degree(my_graph, mode="all")  
  V(my_graph)$size<-grau*0.6 
  plotOutput(my_graph, edge.color = "grey",edge.curved= 0,
       layout = layout.kamada.kawai,
       vertex.color="orange",vertex.frame.color = 'black',
       #vertex.label="",
       vertex.label.dist = 0,
       vertex.label.color = 'black',
       vertex.label.font = 15, vertex.label = V(my_graph)$name, vertex.label.cex = 0.5)
}


#KAWAI COM SUBGRUPO AGREGADO, COM NOS E LINHAS DIMENSIONADOS
PlotaKAWAI_SubSim_NoSim_LinhaSim = function() { 
  E(my_graph)$width <- E(my_graph)$weight/6
   grau<-degree(my_graph, mode="all")  
  V(my_graph)$size<-grau*0.6 
  SCC <- clusters(my_graph, mode="strong")  
  V(my_graph)$color <- rainbow(SCC$no)[SCC$membership]
  plotOutput(my_graph, mark.groups = split(1:vcount(my_graph), SCC$membership),
       layout = layout.kamada.kawai,
       vertex.frame.color="black", edge.color="grey",
       vertex.label.font= 1)
}

#KAWAI COM SUBGRUPO AGREGADO, COM NOS DIMENSIONADOS E LINHAS NAO DIMENSIONADAS
PlotaKAWAI_SubSim_NoSim_LinhaNao = function() { 
  grau<-degree(my_graph, mode="all")  
  V(my_graph)$size<-grau*0.6 
  SCC <- clusters(my_graph, mode="strong")  
  V(my_graph)$color <- rainbow(SCC$no)[SCC$membership]
  plotOutput(my_graph, mark.groups = split(1:vcount(my_graph), SCC$membership),
       layout = layout.kamada.kawai,
       vertex.frame.color="black", edge.color="grey", edge.arrow.size=0.01,
       vertex.label.font= 1)
}

#KAWAI COM SUBGRUPO AGREGADO, COM NOS NAO DIMENSIONADOS E LINHAS DIMENSIONADOS
PlotaKAWAI_SubSim_NoNao_LinhaSim = function() { 
  E(my_graph)$width <- E(my_graph)$weight/6
  grau<-degree(my_graph, mode="all")  
  V(my_graph)$size<-grau*0.6 
  SCC <- clusters(my_graph, mode="strong")  
  V(my_graph)$color <- rainbow(SCC$no)[SCC$membership]
  plotOutput(my_graph, mark.groups = split(1:vcount(my_graph), SCC$membership),
       layout = layout.kamada.kawai,
       vertex.frame.color="black", edge.color="grey",
       vertex.label.font= 1)
}

#KAWAI COM SUBGRUPO AGREGADO, SEM NOS E LINHAS DIMENSIONADOS
PlotaKAWAI_SubSim_NoNao_LinhaNao = function() { 
  SCC <- clusters(my_graph, mode="strong")  
  V(my_graph)$color <- rainbow(SCC$no)[SCC$membership]
  plotOutput(my_graph, mark.groups = split(1:vcount(my_graph), SCC$membership),
       layout = layout.kamada.kawai,
       vertex.frame.color="black", edge.color="grey",
       vertex.label.font= 1)
}

########################FIM FUNCTIONS KAMADA KAWAI#######################################


########################INICIO FUNCTIONS CIRCLE#########################################

#CIRCLE SEM SUBGRUPO AGREGADO, SEM NOS DIMENSIONADOS, SEM LINHAS DIMENSIONADOS
PlotaCIRCLE_SubNao_NoNao_LinhaNao = function() { 
  plotOutput(my_graph,edge.arrow.size=0.01, edge.color = "grey",edge.curved= 0,
       layout = layout.circle,
       vertex.color="orange",vertex.frame.color = 'black',
       #vertex.label="",
       vertex.label.dist = 0,
       vertex.size = 5,
       vertex.label.color = 'black',
       vertex.label.font = 15, vertex.label = V(my_graph)$name, vertex.label.cex = 0.5)
}

#CIRCLE SEM SUBGRUPO AGREGADO, SEM NOS DIMENSIONADOS, COM LINHAS DIMENSIONADAS
PlotaCIRCLE_SubNao_NoNao_LinhasSim = function() { 
  E(my_graph)$width <- E(my_graph)$weight/6
  plotOutput(my_graph, edge.color = "grey",edge.curved= 0,
       layout = layout.circle,
       vertex.color="orange",vertex.frame.color = 'black',
       #vertex.label="",
       vertex.label.dist = 0,
       vertex.size = 5,
       vertex.label.color = 'black',
       vertex.label.font = 15, vertex.label = V(my_graph)$name, vertex.label.cex = 0.5)
}

#CIRCLE SEM SUBGRUPO AGREGADO, COM NOS DIMENSIONADOS, SEM LINHAS DIMENSIONADAS
PlotaCIRCLE_SubNao_NoSim_LinhaNao = function() { 
  grau<-degree(my_graph, mode="all")  
  V(my_graph)$size<-grau*0.6 
  plotOutput(my_graph,edge.arrow.size=0.01, edge.color = "grey",edge.curved= 0,
       layout = layout.circle,
       vertex.color="orange",vertex.frame.color = 'black',
       #vertex.label="",
       vertex.label.dist = 0,
       vertex.label.color = 'black',
       vertex.label.font = 15, vertex.label = V(my_graph)$name, vertex.label.cex = 0.5)
}


#CIRCLE SEM SUBGRUPO AGREGADO, COM NOS DIMENSIONADOS, COM LINHAS DIMENSIONADOS
PlotaCIRCLE_SubNao_NoSim_LinhaSim = function() { 
  E(my_graph)$width <- E(my_graph)$weight/6
  grau<-degree(my_graph, mode="all")  
  V(my_graph)$size<-grau*0.6 
  plotOutput(my_graph, edge.color = "grey",edge.curved= 0,
       layout = layout.circle,
       vertex.color="orange",vertex.frame.color = 'black',
       #vertex.label="",
       vertex.label.dist = 0,
       vertex.label.color = 'black',
       vertex.label.font = 15, vertex.label = V(my_graph)$name, vertex.label.cex = 0.5)
}


#CIRCLE COM SUBGRUPO AGREGADO, COM NOS E LINHAS DIMENSIONADOS
PlotaCIRCLE_SubSim_NoSim_LinhaSim = function() { 
  E(my_graph)$width <- E(my_graph)$weight/6
   grau<-degree(my_graph, mode="all")  
  V(my_graph)$size<-grau*0.6 
  SCC <- clusters(my_graph, mode="strong")  
  V(my_graph)$color <- rainbow(SCC$no)[SCC$membership]
  plotOutput(my_graph, mark.groups = split(1:vcount(my_graph), SCC$membership),
       layout = layout.circle,
       vertex.frame.color="black", edge.color="grey",
       vertex.label.font= 1)
}

#CIRCLE COM SUBGRUPO AGREGADO, COM NOS DIMENSIONADOS E LINHAS NAO DIMENSIONADAS
PlotaCIRCLE_SubSim_NoSim_LinhaNao = function() { 
  grau<-degree(my_graph, mode="all")  
  V(my_graph)$size<-grau*0.6 
  SCC <- clusters(my_graph, mode="strong")  
  V(my_graph)$color <- rainbow(SCC$no)[SCC$membership]
  plotOutput(my_graph, mark.groups = split(1:vcount(my_graph), SCC$membership),
       layout = layout.circle,
       vertex.frame.color="black", edge.color="grey", edge.arrow.size=0.01,
       vertex.label.font= 1)
}

#CIRCLE COM SUBGRUPO AGREGADO, COM NOS NAO DIMENSIONADOS E LINHAS DIMENSIONADOS
PlotaCIRCLE_SubSim_NoNao_LinhaSim = function() { 
  E(my_graph)$width <- E(my_graph)$weight/6
  grau<-degree(my_graph, mode="all")  
  V(my_graph)$size<-grau*0.6 
  SCC <- clusters(my_graph, mode="strong")  
  V(my_graph)$color <- rainbow(SCC$no)[SCC$membership]
  plotOutput(my_graph, mark.groups = split(1:vcount(my_graph), SCC$membership),
       layout = layout.circle,
       vertex.frame.color="black", edge.color="grey",
       vertex.label.font= 1)
}

#CIRCLE COM SUBGRUPO AGREGADO, SEM NOS E LINHAS DIMENSIONADOS
PlotaCIRCLE_SubSim_NoNao_LinhaNao = function() { 
  SCC <- clusters(my_graph, mode="strong")  
  V(my_graph)$color <- rainbow(SCC$no)[SCC$membership]
  plotOutput(my_graph, mark.groups = split(1:vcount(my_graph), SCC$membership),
       layout = layout.circle,
       vertex.frame.color="black", edge.color="grey",
       vertex.label.font= 1)
}



########################FIM FUNCTIONS CIRCLE############################################


########################FUNCOES ISOLADAS - APAGAR ######################################

# plota componente gigante#
PlotaCompgGigante = function(){
comp_gigante <- decompose.graph(my_graph)
largest <- which.max(sapply(comp_gigante, vcount))
plot.new()
plotOutput(comp_gigante[[largest]], edge.arrow.size=0.01,edge.color="grey",layout=layout.fruchterman.reingold,
     vertex.color="orange",vertex.frame.color = 'black',
     #vertex.label="",
     vertex.label.dist = 0,
     vertex.size = 5,
     vertex.label.color = 'black',
     vertex.label.font = 15, vertex.label = V(my_graph)$name, vertex.label.cex = 0.5)

}

#plotar rede com destaque para o componente gigante 
PlotaRedeCompGigante = function(){
library(Cairo)
library(igraph)
comp_gigante <- fastgreedy.community(simplify(as.undirected(my_graph)))
Q <- round(max(comp_gigante$modularity), 2)
l <- layout.fruchterman.reingold(my_graph)
plot.new()
plotOutput(my_graph, layout=layout.fruchterman.reingold, 
     vertex.size=5, vertex.color="orange",
     vertex.frame.color="black", edge.color="grey",
     edge.arrow.size=0.01,vertex.label.font = 1)
}

#plota rede com os n??s dimensionados de acordo com seu grau
PlotaNosDimensionados = function(){
grau<-degree(my_graph, mode="all")  
V(my_graph)$size<-grau*0.6
plot.new()
plotOutput(my_graph,edge.arrow.size=0.01, edge.color="grey",layout=layout.fruchterman.reingold,
     vertex.color="orange",vertex.frame.color = 'blue',
     vertex.label.dist = 0,
     vertex.label.color = 'black',
     vertex.label.font = 1, vertex.label = V(my_graph)$name, vertex.label.cex = 0.7)
}

#plota rede com os linhas dimensionadas de acordo com seu peso
PlotaLinhasDimensionadas = function(){
E(my_graph)$width <- E(my_graph)$weight/6
plot.new()
plotOutput(my_graph,edge.color="grey",layout=layout.fruchterman.reingold,
     vertex.color="orange",vertex.frame.color = 'blue',
     vertex.label.dist = 0,
     vertex.label.color = 'black',
     vertex.label.font = 1, vertex.label = V(my_graph)$name, vertex.label.cex = 0.7)
}

# plota  subgrupos com cores diferentes
PlotaSubgrupos = function() {
SCC <- clusters(my_graph, mode="strong")  
V(my_graph)$color <- rainbow(SCC$no)[SCC$membership]
plotOutput(my_graph, mark.groups = split(1:vcount(my_graph), SCC$membership),
     vertex.size=0.5,  vertex.frame.color="black", edge.color="grey",
     edge.arrow.size=0.01, vertex.label.font= 1)
}


########################## INTERFACE DA APLICACAO #####################################

ui <- fluidPage(                
                
  pageWithSidebar(h5("Opcoes de visualizacao"),
                  
  sidebarPanel(
    
    radioButtons('in_tp_dist', 'Distribuicao',
                 c('Fruchterman-Reingold' ='dist_Fruchterman,',
                   'Kamada kawai'='dist_kawai',
                   'Circle'='dist_circle')),    
    radioButtons('in_tp_com', 'Destacar subgrupos:',
                  c('Sim'='com_subgrupo',
                    'Nao'='sem_subgrupo')),
    radioButtons('in_tp_linha', 'Espessura da linha de acordo com o peso:',
                 c('Sim'='linha_com_peso',
                   'Nao'='linha_normal')),
    radioButtons('in_tp_no','Dimensionar no de acordo com grau:',
                 c('Sim'='no_com_grau',
                   'Nao'='no_sem_grau')),
# implementacao solicitada pelo professor
#                   c('Numero de Publicacoes'='pub',
#                   'Numero de Citacoes'='cit2',
#                   'Nao dimensionar'= 'nao_dim')),
      #  checkboxGroupInput('selecao', 'selecoes', choices = selecoes(data_set)),
    strong(actionButton('execute','Visualizar')),
    uiOutput('selecao_usuario')
  ),
  mainPanel(
    plotOutput('disPlot')
  )
  )
)

######################SERVIDOR DA APLICA????O ############################################

server <- function(input, output) {
 
   renderPlot('displot')
  
  #teste para o action button, trocar pelo plot do grafo com a selecao do usuario #
 
   observeEvent(input$execute, {
   showModal(modalDialog(
   title = "Ainda programando para plotar a rede...",
      easyClose = TRUE,
      footer = NULL
    ))
   
  #   dist <- switch(input$in_tp_dist,
  #                 dist_Fruchterman = dummy(),
  #                 dist_kawai = dummy(),
  #                 dist_circle = dummy()),
  #    subgrupo <- switch(input$in_tp_com,
  #                 dist_Fruchterman = dummy() ,
  #                 com_subgrupo = dummy(),
  #                 sem_subgrupo = dummy())
  #    linha <- switch(input$in_tp_linha,
  #                 linha_com_peso = dummy() ,
  #                 linha_normal = dummy())
  #     no    <- switch(input$in_tp_no,
  #                 no_com_grau = dummy() ,
  #                 no_sem_grau = dummy())
 
       
      # PRECISA COLETAR AS OPCOES DO USUARIO PARA CHAMAR A FUNCAO CORRESPONDENTE#
       
  })
        
}    

shinyApp(ui, server)