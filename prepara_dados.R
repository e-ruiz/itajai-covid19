#
# Script que demonstra como ler os dados sobre COVID19
#        diretamente da API do município de Itajaí-SC
#
#
# @link: https://api.itajai.sc.gov.br/covid19
# @author: Eric S. Lucinger Ruiz <ruiz.eric@itajai.sc.gov.br>[https://github.com/e-ruiz]
# @version: 20-jun-2020
#


#
# Se necessário instala dependências do repo oficial
#
# if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(jsonlite)) install.packages("jsonlite", repos = "http://cran.us.r-project.org")
# if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
# if(!require(hrbrthemes)) install.packages("hrbrthemes", repos = "http://cran.us.r-project.org")



# parametros de configuração, tokens de acesso, secrets, etc..
source("configs.R")


# Load some data
# countries = readRDS("dados/countries.rds")
# bairros = readOGR("dados/itajai_bairros2.shp")

#
# busca os dados na API de Itajaí
#
json_confirmados = paste("https://api.itajai.sc.gov.br/covid19/confirmados?token=", sep = "", api_itajai_token)
json_mortes = paste("https://api.itajai.sc.gov.br/covid19/mortes?token=", sep = "", api_itajai_token)
json_boletins = paste("https://api.itajai.sc.gov.br/covid19/boletins?token=", sep = "", api_itajai_token)

# importa dados do JSON
confirmados = fromJSON(json_confirmados)
mortes = fromJSON(json_mortes)
boletins = fromJSON(json_boletins)

# ordena boletins por data decrescente
# boletins <- boletins[order(data),]

# barplot(height = boletins$confirmados_acumulados, 
#         names = ymd(boletins$data),
#         # las = 1,
#         # horiz = T,
#         main = "Casos Confirmados",
#         xlab = "Data",
# )


# # Most basic bubble plot
# ggplot(boletins, aes(x=ymd(data), y=confirmados_acumulados)) +
#   geom_line( color="steelblue") + 
#   geom_point() +
#   xlab("Período") +
#   theme_ipsum() +
#   theme(axis.text.x=element_text(angle=60, hjust=1)) +
#   scale_x_date(limit=c(as.Date("2020-05-01"),as.Date("2020-06-25"))) +
#   ylim(0,1200)
