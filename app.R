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
if(!require(nlstools)) install.packages("nlstools", repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT", repos = "http://cran.us.r-project.org")

source("configs.R")
# source("helpers.R")
source("prepara_dados.R")
# source("mapas/bairros_itajai.R")

DATA_ULTIMO_BOLETIM = format(as.Date(max(boletins$data)), "%d/%m/%Y")


ui = dashboardPage(
  skin = "green",
  
  dashboardHeader(
    title = "Itajaí | COVID-19",
    tags$li(
      actionLink("openModal", 
        label = strong(paste0("Boletim de ", DATA_ULTIMO_BOLETIM, " - [ ", APP_VERSION, " ]")),
        icon = icon("file-alt")
      ),
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
      menuItem("Predições", icon = icon("search"),
               menuSubItem("Gráfico de predição", tabName = "predicoes"),
               menuSubItem("Tabela de predição", tabName = "predicoes-tabela")
      ),
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
            title = "Evolução dos casos confirmados",
            width = 12,
            # status = 'primary', solidHeader = TRUE,
            # h3("Evolução dos casos confirmados no tempo"),
            #p("Série temporal")
            plotlyOutput("casos_evolucao", height = 180)
          ),

          box(
            #title = "Novos casos por período",
            title = "Casos ativos por período",
            width = 12,
            # status = 'primary', solidHeader = TRUE,
            # h3("Evolução dos casos ativos no tempo"),
            #p("Série temporal")
            plotlyOutput("casos_ativos", height = 180)
          ),
          
        )
      ), # Painel 1
      
      
      # Painel 2
      tabItem(tabName = 'confirmados',

        box(
          title = "",
          span(
            h2(tags$i(class="fa fa-exclamation-triangle", style="color:rgba(0,0,0,0.15); margin-right:16px"), 
              boletins$confirmados_acumulados[boletins$data == max(boletins$data)], 
              span(" Confirmados", style="font-weight:normal"), 
              style="width:66.666%; margin:-24px 0px 32px -24px;font-weight:bold; color:white; background-color:#ff851b; padding:14px 32px;"),
          ),
          # h3("Confirmados por bairro", style="font-weight:bold; font-size:30px; margin:32px 0 16px 0"),
          width = 12,
          # status = 'warning', solidHeader = TRUE,
          tabBox(
            title = "",
            id = "tab-confirmados", 
            width = 12,
            # height = "350px",
            tabPanel("Por gênero", 
                     #"Gráfico de barras/pizza dos casos confirmados por gênero"),
                     plotlyOutput("confimados_genero")),
            #tabPanel("Faixa etária", "Gráfico de barras dos casos confirmados por faixa etária"),
            tabPanel("Por idade", 
                     #"Gráfico de barras dos casos confirmados por idade"),
                     plotlyOutput("confirmados_idade")),
            tabPanel("Por bairro", 
                     #"Gráfico dos casos confirmados por bairro")
                     plotlyOutput("confirmados_bairros"))
          ),
        )
      ), # Painel 2
      
      
      # Painel 3
      tabItem(
        tabName = "confirmados-mapa",
        box(
          title = "",
          span(
            h2(tags$i(class="fa fa-exclamation-triangle", style="color:rgba(0,0,0,0.15); margin-right:16px"), 
              boletins$confirmados_acumulados[boletins$data == max(boletins$data)], 
              span(" Confirmados", style="font-weight:normal"), 
              style="width:66.666%; margin:-24px 0px 32px -24px;font-weight:bold; color:white; background-color:#ff851b; padding:14px 32px;"),
          ),
          h3("Confirmados por bairro", style="font-weight:bold; font-size:30px; margin:32px 0 16px 0"),
          width = 12,
          # status = 'primary', solidHeader = TRUE,
          # h3("Gráfico de linha"),
          leafletOutput("map1")
        )
      ), # Painel 3
      
      
      # Painel 4
      tabItem(tabName = "obitos",
        box(
          title = "",
          span(
            h2(tags$i(class="fa fa-exclamation", style="color:rgba(0,0,0,0.15); margin-right:16px"), 
              boletins$mortes_acumuladas[boletins$data == max(boletins$data)], 
              span(" Óbitos", style="font-weight:normal"), 
              style="width:66.666%; margin:-24px 0px 32px -24px;font-weight:bold; color:white; background-color:#d81b60; padding:14px 32px;"), 
          ),
          width = 12,
          # background = "maroon",
          # status = 'danger', solidHeader = TRUE,
          tabBox(
            title = "",
            id = "tab-obitos", 
            width = 12,
            tabPanel("Linha do tempo",
              plotlyOutput("obitos_evolucao")
            ),
            tabPanel("Por gênero", 
                plotlyOutput("obitos_genero")
            ),
            tabPanel("Por idade", 
                plotlyOutput("obitos_idade")
            )
          )
        )
      ), # Painel 4
      
      
      # Painel 5
      tabItem(tabName = "predicoes",
        box(
          title = "",
          width = 12,
          span(
            h2(tags$i(class = "fa fa-search", style = "color:rgba(0,0,0,0.15); margin-right:16px"), 
              # span("", style="font-weight:normal"), 
              "Predição de casos",
              style = "width:66.666%; margin:-24px 0px 32px -24px;font-weight:bold; color:white; background-color:gray; padding:14px 32px;"), 
          ),
          p("Próximos 10 dias", style = "font-style:italic"),
          plotlyOutput("predicao10dias")
        ),
      ), # Painel 5
      

      # Painel Tabela de Predições
      tabItem(tabName = "predicoes-tabela",
        box(
          width = 12,
          title = "",
          span(
            h2(tags$i(class = "fa fa-search", style = "color:rgba(0,0,0,0.15); margin-right:16px"), 
              "Predição de casos",
              style = "width:66.666%; margin:-24px 0px 32px -24px;font-weight:bold; color:white; background-color:gray; padding:14px 32px;"), 
          ),
          p("Próximos 10 dias", style = "font-style:italic"),
          DT::dataTableOutput("tabelaPredicao")
        ),
      ), # Painel 5
      
      # Painel 6
      tabItem(tabName = "sobre",
        box(
          # title = "",
          width = 12,
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
        title = "",
          span(
            h2(tags$i(class = "fa fa-file-out", style = "color:rgba(0,0,0,0.15); margin-right:16px"), 
              "Boletim epidemiológico",
              style = "width:66%; margin:-24px 0px 32px -24px;font-weight:bold; color:white; background-color:orange; padding:14px 32px;"), 
          ),
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
    pObitos <- plot_ly(boletins, x = ~ymd(data), y = ~mortes_acumuladas,
        type = 'scatter', mode = 'lines',
        line = list(color = '#d81b60', width = 2)
    )
    pObitos <- pObitos %>% layout(xaxis=list(title='Período'),yaxis=list(title='Óbitos'))
    # pObitos
    
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
        type = 'scatter', mode = 'lines',
        line = list(color = "#ff851b")
    )
    pEvolucao <- pEvolucao %>% layout(xaxis=list(title='Período'),yaxis=list(title='Casos Confirmados'))
    # pEvolucao
  })
  

  ################################################
  # Evolução dos Casos confirmados por Idade
  # Grafico - Barras - Casos confirmados por Idade
  ################################################
  output$confirmados_idade <- renderPlotly({
    pIdade <- plot_ly(count(confirmados,idade), x = ~idade, y = ~n, type = 'bar',
      marker = list(color = "#ff851b")
    )
    pIdade <- pIdade %>% layout(xaxis=list(title='Idade'),yaxis=list(title='Casos Confirmados'))
    # pIdade
  })
  

  ################################################
  # Evolução dos Casos confirmados por bairro
  # Grafico - Barras - Casos confirmados por bairro
  ################################################
  output$confirmados_bairros <- renderPlotly({
    pBairro <- plot_ly(count(confirmados,bairro),x = ~bairro, y = ~n,type='bar',
      marker = list(color = "#ff851b")
    )
    pBairro <- pBairro %>% layout(xaxis=list(title='Bairros'),yaxis=list(title='Casos Confirmados'))
    # pBairro
  })


  ################################################
  # Confirmados por genero
  # Grafico - pizza - Confirmados por genero
  ################################################
  output$confimados_genero <- renderPlotly({
    plot_ly(count(confirmados,sexo), labels = ~sexo, values = ~n, type = 'pie',
      marker = list(colors = c("rgb(255, 148, 32)", "rgb(241, 113, 0)"))
    )
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
    plot_ly(count(mortes, sexo), labels = ~sexo, values = ~n, type = 'pie',
      marker = list(colors = c("pink", "#d81b60"), width = 2)
    )
  })    
  

  ################################################
  # Evolução dos öbitos por Idade
  # Grafico - Barras - öbitos por Idade
  ################################################
  output$obitos_idade <- renderPlotly({
    pObitosIdade <- plot_ly(count(mortes,idade), x = ~idade, y = ~n, type = 'bar',
      marker = list(color = '#d81b60', width = 2)
    )
    pObitosIdade <- pObitosIdade %>% layout(xaxis=list(title='Idade'),yaxis=list(title='Óbitos'))
    # pObitosIdade
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
    pAtivos <- plot_ly(boletins, x = ~ymd(data), y = ~(confirmados_acumulados-mortes_acumuladas-curados),
        type = 'bar',
        marker = list(color = "#605ca8")
    )
    pAtivos <- pAtivos %>% layout(xaxis=list(title='Período'),yaxis=list(title='Casos Ativos'))
    # pAtivos
  })   
  

  output$predicao10dias <- renderPlotly({
    ##
    # Check for tricks: https://stackoverflow.com/questions/46730394/r-set-plotly-hovermode-to-compare-data-on-hover
    #

    # Linha Lwr, intervalo de conviança 95% inferior
    pPredicao <- plot_ly(mat.pred, x = ~ date, y = ~Lwr, name = 'I.C.Inf 95%', 
        type = 'scatter', mode='lines',
          # config(
          #   spikemode = 'across'
          # ),
        #,fill = 'none', fillcolor='rgba(220,220,220,0.2)',
        line = list(color = 'rgb(212,212,212)',dash = 'dash',width = 3))
    
    # Linha Pred.m, valor predito
    pPredicao <- pPredicao %>% add_trace(x = ~ date, y = ~Pred.m, name = 'Predito',
        fill = 'tonexty', fillcolor = 'rgba(200,200,200,0.2)',
        line = list(color = 'rgb(152,152,152)', width = 4))
    
    # Linha Upr, intervalo de confiança 95% superior
    pPredicao <- pPredicao %>% add_trace(x = ~ date, y = ~Upr, name = 'I.C.Sup 95%',
        fill = 'tonexty', fillcolor = 'rgba(200,200,200,0.2)',
        line = list(color = 'rgb(192,192,192'))
    
    # Linha Confirmados acumulados
    pPredicao <- pPredicao %>% add_trace(x = ~ df$data, y = ~df$confirmados_acumulados, name='Confirmados',
        fill = 'none', line = list(color='orange', dash='solid'))
    
    # Linha 
    pPredicao <- pPredicao %>% layout(
          xaxis=list(title=paste0('Período\nRelação Predito x Observado: ',
                                  as.character(round(cor(df$confirmados_acumulados, df$Pred.m), 4)))
                     ),
          yaxis=list(title='Casos'),
          hovermode = 'compare'
          #legend=list(x=1,y=0.5)
          )
    pPredicao
  }) 


  source("predicao_confirmados.R")
  # output$predicao10dias <- renderPlotly(pPredicao)
  
  DT::renderDT({
    datatable(iris) %>% formatStyle(
      'Sepal.Width',
      backgroundColor = styleInterval(3.4, c('gray', 'yellow'))
    )
  }) 
  output$tabelaPredicao = DT::renderDataTable(tblPredicao)
}

shinyApp(ui = ui, server = server)
