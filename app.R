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

source("configs.R")
# source("helpers.R")
source("prepara_dados.R")
# source("mapas/bairros_itajai.R")

ui = dashboardPage(
  skin = "yellow",  
  dashboardHeader(
    title = "Itajaí | COVID-19",
    tags$li(actionLink("openModal", label = "", icon = icon("file-alt")),
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
          valueBox(width = 3, 10 * 2, "Confirmados", 
              color = "orange", icon = icon("exclamation-triangle")),
          valueBox(width = 3, 10 * 2, "Óbitos", 
              color = "maroon", icon = icon("exclamation")),
          valueBox(width = 3, 10 * 2, "Curados", 
              color = "green", icon = icon("thumbs-up")),
          valueBox(width = 3, 10 * 2, "Ativos", 
              color = "purple", icon = icon("search")),
          
          
          box(
            title = "Evolução dos casos",
            width = 12,
            status = 'primary', solidHeader = TRUE,
            h3("Gráfico de linha"),
            p("Série temporal")
          ),

          box(
            title = "Novos casos por período",
            width = 12,
            status = 'primary', solidHeader = TRUE,            
            h3("Gráfico de barras"),
            p("Série temporal")
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
            tabPanel("Por gênero", "Gráfico de barras/pizza dos casos confirmados por gênero"),
            tabPanel("Faixa etária", "Gráfico de barras dos casos confirmados por faixa etária"),
            tabPanel("Idade", "Gráfico de barras dos casos confirmados por idade"),
            tabPanel("Por bairro", "Gráfico dos casos confirmados por bairro")
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
          h3("Gráfico de linha"),
          p("Série temporal")
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
                "Gráfico de barras/pizza dos casos de óbitos por gênero"),
            tabPanel("Faixa etária", "Gráfico de barras dos casos de óbitos por faixa etária"),
            tabPanel("Idade", "Gráfico de barras dos casos de óbitos por idade")
          )
        )
      ), # Painel 4


      # Painel 5
      tabItem(tabName = "predicoes",
        box(
          title = "Predições",
          width = 12,
          status = 'primary', solidHeader = TRUE,            
          h3("Predição de contágio"),
          p("Gráfico")
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
  # modal dialog
  observeEvent(input$openModal, {
    showModal(
      modalDialog(
        title = "Boletim epidemiológico",
        easyClose = TRUE,
        footer = NULL,
        img(src="https://intranet2.itajai.sc.gov.br/public/corona-virus/imagens/output.png",
          width="400px"
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

}

shinyApp(ui = ui, server = server)
