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
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")


source("configs.R")
# source("helpers.R")
source("prepara_dados.R")
# source("mapas/bairros_itajai.R")
source("predicao_rebanho.R")


DATA_ULTIMO_BOLETIM = format(as.Date(max(boletins$data)), "%d/%b/%y")

# Cores referência
COR_ATIVOS = "#dc143c"
COR_CONFIRMADOS = "#ff851b"
COR_CONFIRMADOS_1 = "#ffbb55"
COR_CURADOS = "#00a65a"
COR_OBITOS = "#333333"
COR_OBITOS_1 = "#999999"


CustomHeader <- dashboardHeader(
  title = "Itajaí | COVID-19",
  
  tags$li(
    actionLink("openModalAtualizacao", 
      label = strong(paste0("Atualizado em ", DATA_ULTIMO_BOLETIM)),
      icon = icon("calendar-alt")
    ),
    class = "dropdown"
  ),

  tags$li(
    actionLink("openModalBoletimOficial", 
      # label = "", #strong(paste0("Atualizado em ", DATA_ULTIMO_BOLETIM, " - [ ", APP_VERSION, " ]")),
      label = strong(paste0("Boletim oficial")),
      icon = icon("file-alt")
    ),
    class = "dropdown"
  )
  
  # dropdownMenu(type = "notifications",
  #   notificationItem(
  #     text = "5 new users today",
  #     icon("users")
  #   )
  # )
)


ui = dashboardPage(
  skin = "green",
  # @see https://shiny.rstudio.com/articles/css.html
  
  # topbar, topnav
  CustomHeader,

  dashboardSidebar(
    sidebarMenu(
      menuItem("Geral", tabName = "geral", icon = icon("dashboard")),
      menuItem("Confirmados", icon = icon("exclamation"),
               menuSubItem("Geral", tabName = "confirmados"),
               menuSubItem("Mapa", tabName = "confirmados-mapa")
      ),
      menuItem("Óbitos", tabName = "obitos", icon = icon("times-circle")),
      menuItem("Predições", icon = icon("chart-bar"),
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

          # Casos ativos
          # CUSTOM ValueBox, default colors sucks!
          tags$div(class="col-sm-3",
            tags$div(class="small-box", style=paste0("background-color:", COR_ATIVOS, "; color:#fff"),
              tags$div(class="inner",
                h3((boletins$confirmados_acumulados[boletins$data == max(boletins$data)]
                   -boletins$mortes_acumuladas[boletins$data == max(boletins$data)]
                   -boletins$curados[boletins$data == max(boletins$data)])
                ),
                p("Ativos")
              ),
              tags$div(class="icon-large",
                tags$i(class="fa fa-exclamation-triangle")
              )
            )
          ),
          
          
          # Casos confirmados
          # CUSTOM ValueBox, default colors sucks!
          tags$div(class="col-sm-3",
            tags$div(class="small-box", style=paste0("background-color:", COR_CONFIRMADOS, "; color:#fff"),
              tags$div(class="inner",
                h3(boletins$confirmados_acumulados[boletins$data == max(boletins$data)]),
                p("Confirmados")
              ),
              tags$div(class="icon-large",
                tags$i(class="fa fa-exclamation")
              )
            )
          ),
          # valueBox(width = 3, boletins$confirmados_acumulados[boletins$data == max(boletins$data)], "Confirmados", 
          #     color = "orange", icon = icon("exclamation-triangle")),
          
          
          # Casos curados
          # CUSTOM ValueBox, default colors sucks!
          tags$div(class="col-sm-3",
            tags$div(class="small-box", style=paste0("background-color:", COR_CURADOS,"; color:#fff"),
              tags$div(class="inner",
                h3((boletins$curados[boletins$data == max(boletins$data)])
                ),
                p("Curados")
              ),
              tags$div(class="icon-large",
                tags$i(class="fa fa-hospital-o")
              )
            )
          ),
          # valueBox(width = 3, boletins$curados[boletins$data == max(boletins$data)], "Curados", 
          #     color = "green", icon = icon("thumbs-up")),


          # Casos de óbitos
          # CUSTOM ValueBox, default colors sucks!
          tags$div(class="col-sm-3",
            tags$div(class="small-box", style=paste0("background-color:", COR_OBITOS, "; color:#fff"),
              tags$div(class="inner",
                h3(boletins$mortes_acumuladas[boletins$data == max(boletins$data)]),
                p("Óbitos")
              ),
              tags$div(class="icon-large",
                tags$i(class="fa fa-times-circle")
              )
            )
          ),
          # valueBox(width = 3, boletins$mortes_acumuladas[boletins$data == max(boletins$data)], "Óbitos", 
          #     # color = "maroon", 
          #     # style="background-color:black !important;",
          #     icon = icon("times-circle")),


          box(
            # title = "Casos ativos por período",
            title = "",
            span(
              h2(tags$i(class="fa fa-search", style=paste0("color:", COR_ATIVOS,"; margin-right:16px")), 
                (boletins$confirmados_acumulados[boletins$data == max(boletins$data)]
                   -boletins$mortes_acumuladas[boletins$data == max(boletins$data)]
                   -boletins$curados[boletins$data == max(boletins$data)]), 
                span(" Ativos", style="font-weight:normal"), 
                style=paste0("width:66.666%; margin:-24px 0px 32px -10px;font-weight:bold; padding:14px 32px;border-bottom: 1px solid ", COR_ATIVOS,";")),
            ),
            width = 12,
            # status = 'primary', solidHeader = TRUE,
            # h3("Evolução dos casos ativos no tempo"),
            #p("Série temporal")
            plotlyOutput("casos_ativos", height = 180)
          ),

          box(
            title = "",
            span(
              h2(tags$i(class="fa fa-exclamation-triangle", style=paste0("color:", COR_CONFIRMADOS,"; margin-right:16px")), 
                boletins$confirmados_acumulados[boletins$data == max(boletins$data)], 
                span(" Confirmados", style="font-weight:normal"), 
                style=paste0("width:66.666%; margin:-24px 0px 32px -10px;font-weight:bold; padding:14px 32px;border-bottom: 1px solid ", COR_CONFIRMADOS,";")),
            ),
            width = 12,
            # status = 'primary', solidHeader = TRUE,
            # h3("Evolução dos casos confirmados no tempo"),
            #p("Série temporal")
            plotlyOutput("casos_evolucao", height = 180)
          ),
          
        )
      ), # Painel 1
      
      
      # Painel 2
      tabItem(tabName = 'confirmados',

        box(
          title = "",
          span(
            h2(tags$i(class="fa fa-exclamation-triangle", style="color:rgba(0,0,0,0.18); margin-right:16px"), 
              boletins$confirmados_acumulados[boletins$data == max(boletins$data)], 
              span(" Confirmados", style="font-weight:normal"), 
              style=paste0("width:66.666%; margin:-24px 0px 32px -20px;font-weight:bold; color:white; background-color:", COR_CONFIRMADOS,"; padding:14px 32px; border-bottom: 4px solid ", COR_CONFIRMADOS_1,";")
            ),
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
                     plotlyOutput("confirmados_genero")),
            tabPanel("Por faixa etária", #"Gráfico de barras dos casos confirmados por faixa etária"),
                     plotlyOutput("confirmados_faixa_etaria")),
            # tabPanel("Por idade", 
            #          #"Gráfico de barras dos casos confirmados por idade"),
            #          plotlyOutput("confirmados_idade")),
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
            h2(tags$i(class="fa fa-exclamation-triangle", style="color:rgba(0,0,0,0.18); margin-right:16px"), 
              boletins$confirmados_acumulados[boletins$data == max(boletins$data)], 
              span(" Confirmados", style="font-weight:normal"), 
              style=paste0("width:66.666%; margin:-24px 0px 32px -20px;font-weight:bold; color:white; background-color:", COR_CONFIRMADOS,"; padding:14px 32px; border-bottom: 4px solid ", COR_CONFIRMADOS_1,";")
            
            ),
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
            h2(tags$i(class="fa fa-exclamation", style="color:rgba(0,0,0,0.18); margin-right:16px"), 
              boletins$mortes_acumuladas[boletins$data == max(boletins$data)], 
              span(" Óbitos", style="font-weight:normal"), 
              style=paste0("width:66.666%; margin:-24px 0px 32px -20px;font-weight:bold; color:white; background-color:", COR_OBITOS,"; padding:14px 32px; border-bottom:4px solid ", COR_OBITOS_1,";")
            ), 
          ),
          width = 12,
          # background = "maroon",
          # status = 'danger', solidHeader = TRUE,
          tabBox(
            title = "",
            id = "tab-obitos", 
            width = 12,
            tabPanel("Por gênero", 
                plotlyOutput("obitos_genero")
            ),
            tabPanel("Por faixa etária",
                plotlyOutput("obitos_faixa_etaria")
            ),
            # tabPanel("Por idade", 
            #     plotlyOutput("obitos_idade")
            # ),
            tabPanel("Linha do tempo",
              plotlyOutput("obitos_evolucao")
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
            h2(tags$i(class = "fa fa-chart-bar", style = "color:rgba(0,0,0,0.18); margin-right:16px"), 
              # span("", style="font-weight:normal"), 
              "Predição de casos",
              style = "width:66.666%; margin:-24px 0px 32px -20px;font-weight:bold; color:white; background-color:#888; padding:14px 32px; border-bottom:4px solid #bbb;"), 
          ),
#          p("Próximos 10 dias", style = "font-style:italic"),
#          plotlyOutput("predicao10dias")
          tabBox(
            title = "",
            id = "tab-predicoes", 
            width = 12,
            tabPanel("Próximos dias",
                     p("Próximos 10 dias",style = "font-style:italic"), 
                     plotlyOutput("predicao10dias")
            ),
            tabPanel("Contágio da População",
                     p("Expectativa de dias para contágio de 50%, 70% e 100% da população",style = "font-style:italic"), 
                     plotlyOutput("predicao_rebanho")
            )
          )
        ),
      ), # Painel 5
      

      # Painel Tabela de Predições
      tabItem(tabName = "predicoes-tabela",
        box(
          width = 12,
          title = "",
          span(
            h2(tags$i(class = "fa fa-chart-bar", style = "color:rgba(0,0,0,0.18); margin-right:16px"), 
              "Predição de casos",
              style = "width:66.666%; margin:-24px 0px 32px -20px;font-weight:bold; color:white; background-color:#888; padding:14px 32px; border-bottom:4px solid #bbb;"), 
          ),
          tabBox(
            title = "",
            id = "tab-tblPredicoes", 
            width = 12,
            tabPanel("Próximos dias",
                     p("Próximos 10 dias", style = "font-style:italic"),
                     DT::dataTableOutput("tabelaPredicao")
            ),
            tabPanel("Contágio da População",
                     p("Estimativa de contágio da população em %", style = "font-style:italic"),
                     DT::dataTableOutput("tabelaLogPredicao")
            )
          )          
          
          
        ),
      ), # Painel 5
      
      # Painel 6
      tabItem(tabName = "sobre",
        box(
          title = "",
          span(
            h2(tags$i(class="fa fa-info-circle", style="color:#517; margin-right:16px"), 
              span(" Sobre", style="font-weight:normal"), 
              style="width:66.666%; margin:-24px 0px 32px -20px;font-weight:bold; color:white; background-color:#aaa; padding:14px 32px; border-bottom:4px solid #ccc;"), 
          ),
          width = 12,
          p("Este é um trabalho acadêmico desenvolvido na disciplina de Visualização de Dados, 
            dentro do curso de Pós Graduação em Big Data da Universidade do Vale do Itajaí (Univali). 
            Com a abordagem de PBL - Problem Based Learning - nos foi proposto 
            escolher um tema para aplicar os conhecimentos adquiridos nas aulas."
            ),
          
          p("O tema escolhido foi: 'Boletim epidemiológico da COVID-19 do Município de Itajaí',
            onde a proposta seria apresentar um boletim com gráficos interativos, de fácil interpretação
            das informações para toda a população, desse modo registrando um histórico da evolução da doença no município. 
            E assim, complementando o boletim oficial, que é publicado diariamente em forma textual online.
          "),
          hr(),
          
          
          h2("Alunos"),
          p(strong("Wagner Correia"), 
          " é professor na área de desenvolvimento de software, 
          graduado em Sistemas de Informação e pós-graduando em Big Data, 
          além da docência é sócio de uma agência web atuando no desenvolvimento de sistemas web e mobile. 
          Gosta de coisas criativas e de pensar fora da caixa."),

          p(strong("Marcio Fossa"), 
            " é Bacharel em Ciências da Computação e pós-graduando em Big Data pela Univali. 
            Iniciou a carreira como desenvolvedor e migrou para a área de Banco de Dados. 
            Atualmente integra a equipe de Conhecimento, Sistemas e Integrações da Digitro Tecnologia S/A."),
          
          p(strong("Eric S. Lucinger Ruiz"), 
            " é Técnólogo em Análise e Desenvolvimento de Sistemas e pós-graduando em Big Data pela Univali.
            Funcionário público municipal desde 2012 na prefeitura de Itajaí, tendo atuado na construção e implantação de diversos sistemas.
            Mais recentemente com atuação incisiva em projetos de Business Intelligence (BI) e Data Science."),
          
          h2("Professor"),
          p(strong("Rodrigo Sant'Ana"), 
            " é Graduado em Oceanografia, 
            Mestre em Ciência e Tecnologia Ambiental, 
            Doutorando em Ciência e Tecnologia Ambiental,
            professor e pesquisador no Laboratório de Estudos Marinhos Aplicados da Escola do Mar, 
            Ciência e Tecnologia na Universidade do Vale do Itajaí (Univali)."),
          p("Rodrigo também é autor dos modelos matemáticos utilizados nas análises preditivas aqui apresentadas."),
          hr(),
          
          
          h2("Fontes de dados"),
          p("IBGE: ", 
            a("https://cidades.ibge.gov.br/brasil/sc/itajai/panorama", 
              href="https://cidades.ibge.gov.br/brasil/sc/itajai/panorama",
              target="blank")),
          p("Município de Itajaí: ", 
            a("https://coronavirus.itajai.sc.gov.br/", 
              href="https://coronavirus.itajai.sc.gov.br/",
              target="blank")),
          p("Município de Itajaí: ", 
            a("https://api.itajai.sc.gov.br/covid19/boletins", 
              href="https://api.itajai.sc.gov.br/covid19/boletins",
              target="blank")),
          hr(),
          
          
          h2("Tecnologias"),
          p("Aplicação desenvolvida majoritariamente na linguagem R, com as bibliotecas:"),
          tags$ul(
            tags$li("Shiny - ", a("https://shiny.rstudio.com/", href="https://shiny.rstudio.com/")),
            tags$li("Shinydashboard - ", a("https://rstudio.github.io/shinydashboard/", href="https://rstudio.github.io/shinydashboard/")),
            tags$li("Plotly - ", a("https://plotly.com/r/", href="https://plotly.com/r/")),
          ),
          # p("Dados sobre tecnologias aplicadas (R, Shiny, etc.) e suas licenças"),
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
  observeEvent(input$openModalBoletimOficial, {
    showModal(
      modalDialog(
        title = "",
          span(
            h2(tags$i(class="fa fa-file-alt", style="color:#fff; margin-right:16px"), 
              span(" Boletim epidemiológico", style="font-weight:normal"), 
              style=paste0("width:75%; margin:-24px 0px 32px -20px;font-weight:bold; color:white; background-color:", COR_CONFIRMADOS,"; padding:14px 32px; border-bottom: 4px solid ", COR_CONFIRMADOS_1,";")
            ), 
          ),
        easyClose = TRUE,
        footer = NULL,
        p("Resumo do boletim epidemioliógico oficial, para maiores detalhes acesse: ",
          a("https://coronavirus.itajai.sc.gov.br", 
            href="https://coronavirus.itajai.sc.gov.br", 
            target="blank"
          )
        ),
        img(src="https://intranet2.itajai.sc.gov.br/public/corona-virus/imagens/output.png",
          width="360px"
        )
      )
    )
  })

  # modal dialog com informações sobre atualização dos dados
  observeEvent(input$openModalAtualizacao, {
    showModal(
      modalDialog(
        title = "",
          span(
            h2(tags$i(class="fa fa-info-circle", style="color:#517; margin-right:16px"), 
              span(" Atualização dos dados", style="font-weight:normal"), 
              style="width:75%; margin:-24px 0px 32px -20px;font-weight:bold; color:white; background-color:#aaa; padding:14px 32px; border-bottom:4px solid #ccc;"), 
          ),
        easyClose = TRUE,
        footer = NULL,
        p("Os dados aqui contidos, eventualmente podem não estar sincronizados com os boletins oficiais."),
        p("Para maiores detalhes acesse: ",
          a("https://coronavirus.itajai.sc.gov.br", 
            href="https://coronavirus.itajai.sc.gov.br", 
            target="blank"
          )
        )
      )
    )
  })
  
  # 
  # Casos confirmados por bairro
  # Fonte: GeoJSON API Itajaí
  # 
  # cria uma lista com os valores distribuidos
  # usado para construir o range do choropleth dinamicamente  
  bins <- round(
      seq(0, max(confirmados_bairro$confirmados), 
        by = max(confirmados_bairro$confirmados) / 10)
  )
  pal <- colorBin("YlOrRd", domain = confirmados_bairro$confirmados, bins = bins)
  
  output$map1 <- renderLeaflet({
    leaflet(confirmados_bairro) %>%
    setView(lat = -26.95, lng = -48.7, zoom = 11) %>%
    # tiles provider
    addProviderTiles(providers$CartoDB.Positron, 
      options = providerTileOptions(minZoom = 10, maxZoom = 14),
      group = "Claro"
    ) %>%
    addProviderTiles(providers$CartoDB.DarkMatter, 
      options = providerTileOptions(minZoom = 10, maxZoom = 14),
      group = "Escuro"
    ) %>%
    # addProviderTiles(providers$OpenStreetMap.HOT, 
    #   options = providerTileOptions(minZoom = 10, maxZoom = 14),
    #   group = "Colorido"
    # ) %>%
    # addTiles() %>%
    addPolygons(
      fillColor = ~pal(confirmados),
      weight = 1,
      opacity = 1,
      color = "#444",
      stroke = TRUE,
      dashArray = 1, 
      fillOpacity = 1,
      # smoothFactor = 0.3, 
      label = ~paste0(bairro, ": ", confirmados ),
      highlight = highlightOptions(
        weight = 5,
        color = "#446",
        dashArray = "",
        fillOpacity = 1,
        bringToFront = TRUE)
    ) %>%
    # Layers control
    addLayersControl(
      # title = "Mapa base",
      baseGroups = c("Claro", "Escuro"),
      # overlayGroups = c("Quakes", "Outline"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>%
    addLegend("bottomright", pal = pal, 
      values = ~confirmados_bairro$confirmados, 
      opacity = 1.0,
      title = "Casos confirmados",
      # labFormat = labelFormat(transform = function(x) round(10^x))
    ) %>%  
    addScaleBar( 
      position = "bottomleft", 
      options = scaleBarOptions(maxWidth = 100, 
        metric = TRUE, imperial = FALSE, 
        updateWhenIdle = TRUE
      )
    ) %>%
    htmlwidgets::onRender("
      function() {
        $('form.leaflet-control-layers-list').prepend('<label style=\"text-align:center\">Mapas base</label>');
      }
    ") 
    # addMeasure()
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
        line = list(color = COR_OBITOS, width = 2)
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
        line = list(color = COR_CONFIRMADOS)
    )
    pEvolucao <- pEvolucao %>% layout(xaxis=list(title='Período'),yaxis=list(title='Casos Confirmados'))
    # pEvolucao
  })
  

  # ################################################
  # # Evolução dos Casos confirmados por Idade
  # # Grafico - Barras - Casos confirmados por Idade
  # ################################################
  # output$confirmados_idade <- renderPlotly({
  #   pIdade <- plot_ly(count(confirmados,idade), x = ~idade, y = ~n, type = 'bar',
  #     marker = list(color = COR_CONFIRMADOS)
  #   )
  #   pIdade <- pIdade %>% layout(xaxis=list(title='Idade'),yaxis=list(title='Casos Confirmados'))
  #   # pIdade
  # })
  

  ################################################
  # Evolução dos Casos confirmados por bairro
  # Grafico - Barras - Casos confirmados por bairro
  ################################################
  output$confirmados_bairros <- renderPlotly({
    pBairro <- plot_ly(count(confirmados,bairro),x = ~bairro, y = ~n,type='bar',
      marker = list(color = COR_CONFIRMADOS)
    )
    pBairro <- pBairro %>% layout(xaxis=list(title='Bairros'),yaxis=list(title='Casos Confirmados'))
    # pBairro
  })


  ################################################
  # Confirmados por genero
  # Grafico - pizza - Confirmados por genero
  ################################################
  output$confirmados_genero <- renderPlotly({
    plot_ly(count(confirmados, sexo), labels = ~sexo, values = ~n, 
            type = 'pie',hole = 0.5,
      marker = list(colors = c(COR_CONFIRMADOS_1, COR_CONFIRMADOS))
    )
  })
  
  
  ################################################
  # Confirmados por Faixa Etária
  # Grafico - barras - Confirmados por Faixa Etária
  ################################################
  output$confirmados_faixa_etaria <- renderPlotly({
    pConfFaixaEtaria <- plot_ly(count(confirmados,faixa_etaria),x = ~faixa_etaria, y = ~n,marker = list(color = COR_CONFIRMADOS) ,type='bar')
    pConfFaixaEtaria <- pConfFaixaEtaria %>% layout(xaxis=list(title='Faixa Etária'),yaxis=list(title='Casos Confirmados'))
    pConfFaixaEtaria
    
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
    plot_ly(count(mortes, sexo), labels = ~sexo, values = ~n, 
            type = 'pie', hole = 0.5,
      marker = list(colors = c(COR_OBITOS_1, COR_OBITOS), width = 2)
    )
  })    
  
  ################################################
  # Obitos por Faixa Etária
  # Grafico - barras - Obitos por Faixa Etária
  ################################################
  output$obitos_faixa_etaria <- renderPlotly({
    pObitosFaixaEtaria <- plot_ly(count(mortes,faixa_etaria),x = ~faixa_etaria, y = ~n,marker = list(color = COR_OBITOS) ,type='bar')
    pObitosFaixaEtaria <- pObitosFaixaEtaria %>% layout(xaxis=list(title='Faixa Etária'),yaxis=list(title='Óbitos'))
    pObitosFaixaEtaria
   })
  
  # ################################################
  # # Evolução dos öbitos por Idade
  # # Grafico - Barras - öbitos por Idade
  # ################################################
  # output$obitos_idade <- renderPlotly({
  #   pObitosIdade <- plot_ly(count(mortes,idade), x = ~idade, y = ~n, type = 'bar',
  #     marker = list(color = COR_OBITOS, width = 2)
  #   )
  #   pObitosIdade <- pObitosIdade %>% layout(xaxis=list(title='Idade'),yaxis=list(title='Óbitos'))
  #   # pObitosIdade
  # })
  
  
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
        marker = list(color = COR_ATIVOS)
    )
    pAtivos <- pAtivos %>% layout(xaxis=list(title='Período'),yaxis=list(title='Casos Ativos'))
    # pAtivos
  })   
  

  ################################################
  # Predicao proximos 10 dias moelo exponencial
  # Grafico - Linhas - Predicao proximos 10 dias
  ################################################
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

  ################################################
  # Predicao Expectativa de dias para contágio de X % da população.
  # Grafico - Linhas - Predicao Contágio da População
  ################################################
  output$predicao_rebanho <- renderPlotly({
      # Linha Lwr, intervalo de conviança 95% inferior
      pLogistico <- plot_ly(log.mat.pred, x = ~ id.date, y = ~(upr), name = 'I.C.Sup 95%', 
                            type = 'scatter', mode='lines',
                            line = list(color = 'rgb(212,212,212)',dash = 'dash',width = 3))
      
      # Linha Pred.m, valor predito
      pLogistico <- pLogistico %>% add_trace(x = ~ id.date, y = ~(pred.m), name = 'Predito',
                                             line = list(color = 'rgb(152,152,152)', width = 4))
      
      
      # Linha Upr, intervalo de confiança 95% superior
      pLogistico <- pLogistico %>% add_trace(x = ~ id.date, y = ~(lwr), name = 'I.C.Inf 95%',
                                             line = list(color = 'rgb(192,192,192'))
      
      pLogistico <- pLogistico %>% add_trace(x = ~ log.par$id.date, y = ~log.par$casos,
                                             name = paste0(str_pad(as.character(log.par$percentual),3,side='left',pad=' '),' %',log.par$texto,as.character(log.par$id.date),' Dias'),
                                             line = list(color = 'transparent'),
                                             mode='markers',marker=list(color=log.par$cores), 
                                             size=15
      )
      
      # Linha 
      pLogistico <- pLogistico %>% layout(
        #title = 'Expectativa de dias para contágio de X % da população',
        #plot_bgcolor='#CCCCCC',
        xaxis=list(title='Quantidade de Dias'),
        yaxis=list(title='Casos',range=c(0,250000),hoverformat='.0f'),
        hovermode = 'compare'
        #legend=list(x=1,y=0.5)
      )
      pLogistico
})

  source("predicao_confirmados.R")
  # output$predicao10dias <- renderPlotly(pPredicao)
  
  # DT::renderDT({
  #   datatable(iris) %>% formatStyle(
  #     'Sepal.Width',
  #     backgroundColor = styleInterval(3.4, c('gray', 'yellow'))
  #   )
  # }) 

  ################################################
  # Tabela de Predicao Próximos 10 dias
  ################################################
  output$tabelaPredicao = DT::renderDataTable(
      tblPredicao,
      option = list(language = list(search = 'Pesquisar:',
                                    EmptyTable= 'Nenhum registro encontrado',
                                    info = 'Mostrando de _START_ até _END_ de _TOTAL_ registros',
                                    infoEmpty ='Mostrando 0 até 0 de 0 registros',
                                    infoFiltered = '(Filtrados de _MAX_ registros)',
                                    loadingRecords = 'Carregando...',
                                    processing = 'Processando...',
                                    zeroRecords = 'Nenhum registro encontrado',
                                    lengthMenu = '_MENU_ resultados por página',
                                    paginate = list(previous = 'Anterior',
                                                    `next` = 'Próximo',
                                                    first = 'Primeiro',
                                                    last = 'Último'
                                    ) #,
                                    # select = list(rows=list(
                                    #        `_` = 'Selecionado %d linhas',
                                    #        0 = 'Nenhuma linha selecionada',
                                    #        1 = 'Selecionado 1 linha'
                                    #  ))
                                    
      )),
      colnames=c('Data Boletim','Confirmados Acum.','Preditos','Erro %','I.C.Inf 95%','I.C.Sup 95%'))

  ################################################
  # Tabela de Predicao Expectativa de dias para contágio de X % da população.
  ################################################
  
  output$tabelaLogPredicao = DT::renderDataTable(
    tblLogPredicao,
    option = list(language = list(search = 'Pesquisar:',
                                  EmptyTable= 'Nenhum registro encontrado',
                                  info = 'Mostrando de _START_ até _END_ de _TOTAL_ registros',
                                  infoEmpty ='Mostrando 0 até 0 de 0 registros',
                                  infoFiltered = '(Filtrados de _MAX_ registros)',
                                  loadingRecords = 'Carregando...',
                                  processing = 'Processando...',
                                  zeroRecords = 'Nenhum registro encontrado',
                                  lengthMenu = '_MENU_ resultados por página',
                                  paginate = list(previous = 'Anterior',
                                                  `next` = 'Próximo',
                                                  first = 'Primeiro',
                                                  last = 'Último'
                                  ) #,
                                  # select = list(rows=list(
                                  #        `_` = 'Selecionado %d linhas',
                                  #        0 = 'Nenhuma linha selecionada',
                                  #        1 = 'Selecionado 1 linha'
                                  #  ))
                                  
    )),
    colnames=c('Data Boletim','Confirmados Acum.','% Populção','Preditos','Erro %','I.C.Inf 95%','I.C.Sup 95%')
  )
  
}

shinyApp(ui = ui, server = server)
