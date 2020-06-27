#
# Script que demonstra como ler os dados sobre COVID19
#        diretamente da API do município de Itajaí-SC
#
#
# @link: https://api.itajai.sc.gov.br/covid19
# @author: Eric S. Lucinger Ruiz <ruiz.eric@itajai.sc.gov.br>[https://github.com/e-ruiz]
# @version: 12-jun-2020
#


#
# Se necessário instala dependências do repo oficial
#
# if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
# if(!require(mapproj)) install.packages("mapproj", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
# # if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
# if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")

source("configs.R")
# source("helpers.R")
source("prepara_dados.R")
# source("mapas/bairros_itajai.R")
source('predicao_confirmados.R')


ui = dashboardPage(
  skin = "yellow",  
  dashboardHeader(
    title = "Itajaí | COVID-19",
    tags$li(
      actionLink("openModal", 
      label = strong(paste0(" Atualizado em ", DATA_ULTIMO_BOLETIM, " - ", APP_VERSION)), 
      icon = icon("file-alt")),
      class = "dropdown"
    )    
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Geral", tabName = "geral", icon = icon("dashboard")),
      menuItem("Confirmados", icon = icon("exclamation-triangle"),
        menuSubItem("Geral", tabName = "confirmados"),
        menuSubItem("Mapa", tabName = "confirmados-mapa")
      ),
      menuItem("Óbitos", tabName = "obitos", icon = icon("exclamation")),
      menuItem("Predições", tabName = "predicoes", icon = icon("search")),
      menuItem("Sobre", tabName = "sobre", icon = icon("info"))
    )
  ),

  dashboardBody(
    tabItems(

      # Painel 1
      tabItem(tabName = 'geral',
        # Boxes need to be put in a row (or column)
        fluidRow(
          valueBox(width = 3, boletins$confirmados_acumulados[boletins$data == max(boletins$data)], "Confirmados", 
              color = "orange", icon = icon("exclamation-triangle")),
          valueBox(width = 3, boletins$mortes_acumuladas[boletins$data == max(boletins$data)], "Óbitos", 
              color = "maroon", icon = icon("exclamation")),
          valueBox(width = 3, boletins$curados[boletins$data == max(boletins$data)], "Curados", 
              color = "green", icon = icon("thumbs-up")),
          valueBox(width = 3, 
                   (boletins$confirmados_acumulados[boletins$data == max(boletins$data)]
                   -boletins$mortes_acumuladas[boletins$data == max(boletins$data)]
                   -boletins$curados[boletins$data == max(boletins$data)])
                   , "Ativos", 
              color = "purple", icon = icon("search")),
          
          
          box(
            title = "Evolução dos casos",
            width = 12,
            status = 'primary', solidHeader = TRUE,
            h3("Evolução dos casos confirmados no tempo"),
            #p("Série temporal")
            plotlyOutput("casos_evolucao", height = 180)
          ),

          box(
            #title = "Novos casos por período",
            title = "Casos Ativos por período",
            width = 12,
            status = 'primary', solidHeader = TRUE,            
            h3("Evolução dos casos ativos no tempo"),
            #p("Série temporal")
            plotlyOutput("casos_ativos", height = 180)
          ),
          
        )
      ), # Painel 1


      # Painel 2
      tabItem(tabName = 'confirmados',

        box(
          title = "Casos confirmados",
          width = 12,
          status = 'warning', solidHeader = TRUE,
          tabBox(
            title = "",
            id = "tab-confirmados", 
            width = 12,
            height = "350px",
            tabPanel("Por gênero", 
                     #"Gráfico de barras/pizza dos casos confirmados por gênero"),
                     plotlyOutput("confimados_genero", height = 250)),
            #tabPanel("Faixa etária", "Gráfico de barras dos casos confirmados por faixa etária"),
            tabPanel("Idade", 
                     #"Gráfico de barras dos casos confirmados por idade"),
                     plotlyOutput("confirmados_idade",height = 250)),
            tabPanel("Por bairro", 
                     #"Gráfico dos casos confirmados por bairro")
                     plotlyOutput("confirmados_bairros",height = 250))
          ),
        )
      ), # Painel 2


      # Painel 3
      tabItem(
        tabName = "confirmados-mapa",
        box(
          title = "Mapa casos confirmados",
          width = 12,
          # status = 'primary', solidHeader = TRUE,
          # h3("Gráfico de linha"),
          leafletOutput("map1", height = "400px")
        )
      ), # Painel 3


      # Painel 4
      tabItem(tabName = "obitos",
        box(
          title = "Evolução dos casos",
          width = 12,
          status = 'primary', solidHeader = TRUE,
          h3("Evolução dos Óbitos no tempo"),
          #p("Série temporal"),
          plotlyOutput("obitos_evolucao", height = 180)
        ),
        box(
          title = "Casos óbitos",
          width = 12,
          # background = "maroon",
          status = 'danger', solidHeader = TRUE,
          tabBox(
            title = "",
            id = "tab-obitos", 
            width = 12,
            height = "250px",
            tabPanel("Por gênero", 
                #"Gráfico de barras/pizza dos casos de óbitos por gênero",
                plotlyOutput("obitos_genero", height = 200)),
            #tabPanel("Faixa etária", "Gráfico de barras dos casos de óbitos por faixa etária"),
            tabPanel("Idade", 
                     #"Gráfico de barras dos casos de óbitos por idade")
                plotlyOutput("obitos_idade",height = 200))
          )
        )
      ), # Painel 4


      # Painel 5
      tabItem(tabName = "predicoes",
        box(
          title = "Predição de Casos",
          width = 12,
          status = 'primary', solidHeader = TRUE,
          h3("Proximos 10 dias"),
          #p("Série temporal")
          plotlyOutput("predicao10dias", height = 300)
        ),
      ), # Painel 5



      # Painel 6
      tabItem(tabName = "sobre",
        box(
          # title = "",
          width = 12,
          # status = 'primary', solidHeader = TRUE,            
          h2("Sobre"),
          p("Dados sobre a motivação, elaboração do trabalho"),
          hr(),

          h2("Fontes de dados"),
          p("Especificação dos dados, fonte, etc."),
          hr(),

          h2("Tecnologias e licenças"),
          p("Dados sobre tecnologias aplicadas (R, Shiny, etc.) e suas licenças"),
          hr(),

          h2("Autores"),
          p("Dados dos autores"),
          hr(),

          h2("Repositório"),
          a("https://github.com/e-ruiz/itajai-covid19", 
            href="https://github.com/e-ruiz/itajai-covid19"), 
          hr()
        )
      ) # Painel 6
    )
  )
)

server = function(input, output) {
  # modal dialog com a img do boletim oficial
  observeEvent(input$openModal, {
    showModal(
      modalDialog(
        title = "Boletim epidemiológico",
        easyClose = TRUE,
        footer = NULL,
        img(src="https://intranet2.itajai.sc.gov.br/public/corona-virus/imagens/output.png",
          width="360px"
        )
      )
    )
  })

  # 
  # Casos confirmados por bairro
  # Fonte: GeoJSON API Itajaí
  # 
  bins <- c(0, 10, 20, 50, 100, 200, Inf)
  pal <- colorBin("YlOrRd", domain = confirmados_bairro$confirmados, bins = bins)

  output$map1 <- renderLeaflet({
    leaflet(confirmados_bairro) %>%
      setView(lat = -26.95, lng = -48.7, zoom = 11) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # addTiles() %>%
      addPolygons(
        fillColor = ~pal(confirmados),
        weight = 1,
        opacity = 1,
        color = "#444",
        stroke = TRUE,
        dashArray = 1, 
        fillOpacity = .6,
        # smoothFactor = 0.3, 
        label = ~paste0(bairro, ": ", confirmados ),
        highlight = highlightOptions(
          weight = 5,
          color = "#446",
          dashArray = "",
          fillOpacity = 1,
          bringToFront = TRUE)
      )
      # addLegend(pal = pal, values = ~log10(pop), opacity = 1.0,
      #   labFormat = labelFormat(transform = function(x) round(10^x)))
  })

  # output$map1 <- renderLeaflet({
  #   leaflet() %>% 
  #     # addProviderTiles("Stamen.Watercolor") %>% 
  #       setView(lng = -100, lat = 50, zoom = 2) %>%
  #       addTiles()

  #   })
  ################################################
  # Evolução dos óbitos
  # Grafico - serie tempora - mortes no periodo
  ################################################
  output$obitos_evolucao <- renderPlotly({
#    ggplot(data = boletins,
#                  aes(x = ymd(data), y = mortes_acumuladas, label = mortes_acumuladas)) +
#      geom_line(colour = "#97a0bd") +
#      geom_point(pch = 21, colour = "#97a0bd", fill = "#97a0bd", size = 1,
#                 alpha = 0.8) +
#      ## geom_text(nudge_y = 20) +
#      #facet_wrap(~ city, ncol = 2, scales = "free_y") +
#      labs(x = "Dias", y = "óbitos") +
#      scale_x_date(breaks = "10 days") +
#      theme_gray(base_size = 16)
    pObitos <- plot_ly(boletins,x = ~ymd(data), y = ~mortes_acumuladas,
                         type='scatter',mode='lines')
    pObitos <- pObitos %>% layout(xaxis=list(title='Período'),yaxis=list(title='Óbitos'))
    pObitos
    
  })
  
  ################################################
  # Evolução dos casos confirmados
  # Grafico - serie temporal - caos no periodo
  ################################################
  output$casos_evolucao <- renderPlotly({
    #ggplot(data = boletins,
    #       aes(x = ymd(data), y = confirmados_acumulados, label = confirmados_acumulados)) +
    #  geom_line(colour = "#97a0bd") +
    #  geom_point(pch = 21, colour = "#97a0bd", fill = "#97a0bd", size = 1,
    #             alpha = 0.8) +
    #  ## geom_text(nudge_y = 20) +
    #  #facet_wrap(~ city, ncol = 2, scales = "free_y") +
    #  labs(x = "Dias", y = "Casos Confirmados") +
    #  scale_x_date(breaks = "10 days") +
    #  theme_gray(base_size = 16)
    
    pEvolucao <- plot_ly(boletins,x = ~ymd(data), y = ~confirmados_acumulados,
                      type='scatter',mode='lines')
    pEvolucao <- pEvolucao %>% layout(xaxis=list(title='Período'),yaxis=list(title='Casos Confirmados'))
    pEvolucao
    
    
  })
 
  ################################################
  # Evolução dos Casos confirmados por Idade
  # Grafico - Barras - Casos confirmados por Idade
  ################################################
  output$confirmados_idade <- renderPlotly({
    pIdade <- plot_ly(count(confirmados,idade),x = ~idade, y = ~n,type='bar')
    pIdade <- pIdade %>% layout(xaxis=list(title='Idade'),yaxis=list(title='Casos Confirmados'))
    pIdade
  })
  
  ################################################
  # Evolução dos Casos confirmados por bairro
  # Grafico - Barras - Casos confirmados por bairro
  ################################################
  output$confirmados_bairros <- renderPlotly({
    pBairro <- plot_ly(count(confirmados,bairro),x = ~bairro, y = ~n,type='bar')
    pBairro <- pBairro %>% layout(xaxis=list(title='Bairros'),yaxis=list(title='Casos Confirmados'))
    pBairro
  })
  ################################################
  # Confirmados por genero
  # Grafico - pizza - Confirmados por genero
  ################################################
  output$confimados_genero <- renderPlotly({
      plot_ly(count(confirmados,sexo),labels = ~sexo,values= ~n,type='pie')
  }) 
  ################################################
  # óbitos por genero
  # Grafico - pizza - mortes por genero
  ################################################
  output$obitos_genero <- renderPlotly({
    #plot_ly(
    #  x=(aggregate(mortes$sexo,by=list(mortes$sexo),FUN=length))$Group.1,
    #  y=(aggregate(mortes$sexo,by=list(mortes$sexo),FUN=length))$x,
    #  name="Mortes por Gênero",type="bar")
    plot_ly(count(mortes,sexo),labels = ~sexo,values= ~n,type='pie')
  })    

  ################################################
  # Evolução dos öbitos por Idade
  # Grafico - Barras - öbitos por Idade
  ################################################
  output$obitos_idade <- renderPlotly({
    pObitosIdade <- plot_ly(count(mortes,idade),x = ~idade, y = ~n,type='bar')
    pObitosIdade <- pObitosIdade %>% layout(xaxis=list(title='Idade'),yaxis=list(title='Óbitos'))
    pObitosIdade
  })
  
  
  ################################################
  # Evolução casos Ativos
  # Grafico - Barras - Casos Ativos
  ################################################
  output$casos_ativos <- renderPlotly({
    #plot_ly(
    #  x=(aggregate(mortes$sexo,by=list(mortes$sexo),FUN=length))$Group.1,
    #  y=(aggregate(mortes$sexo,by=list(mortes$sexo),FUN=length))$x,
    #  name="Mortes por Gênero",type="bar")
    #plot_ly(count(mortes,sexo),labels = ~sexo,values= ~n,type='pie')
    pAtivos <- plot_ly(boletins,x = ~ymd(data), y = ~(confirmados_acumulados-mortes_acumuladas-curados),
            type = 'bar') #type='scatter',mode='lines')
    pAtivos <- pAtivos %>% layout(xaxis=list(title='Período'),yaxis=list(title='Casos Ativos'))
    pAtivos
  })

  output$predicao10dias <- renderPlotly({
    pPredicao <- plot_ly(mat.pred,x = ~ date, y = ~Pred.m,type='scatter',mode='lines',name='Previsto')
    pPredicao <- pPredicao %>% add_trace(x = ~ date, y = ~Upr,type='scatter',mode='lines',name='Max Previsto')
    pPredicao <- pPredicao %>% add_trace(x = ~ date, y = ~Lwr,type='scatter',mode='lines',name='Min Previsto')
    pPredicao <- pPredicao %>% add_trace( x = ~ df$data, y = ~df$confirmados_acumulados,type='scatter',mode='lines',name='Confirmados')
    
    pPredicao <- pPredicao %>% layout(xaxis=list(title='Período'),yaxis=list(title='Casos'))
    pPredicao
  })  
  
}



shinyApp(ui = ui, server = server)
