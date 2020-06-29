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
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
# if(!require(hrbrthemes)) install.packages("hrbrthemes", repos = "http://cran.us.r-project.org")



# parametros de configuração, tokens de acesso, secrets, etc..
source("configs.R")


# Load some data
# countries = readRDS("dados/countries.rds")
# bairros = readOGR("dados/itajai_bairros2.shp")

#
# Prepara os links para busca dos dados na API de Itajaí
#
json_confirmados = paste("https://api.itajai.sc.gov.br/covid19/confirmados?token=", sep = "", api_itajai_token)
json_mortes = paste("https://api.itajai.sc.gov.br/covid19/mortes?token=", sep = "", api_itajai_token)
json_boletins = paste("https://api.itajai.sc.gov.br/covid19/boletins?token=", sep = "", api_itajai_token)
geojson_confirmados_bairro = paste("https://api.itajai.sc.gov.br/covid19/confirmados_bairros?token=", sep = "", api_itajai_token)

# importa dados do JSON
confirmados = fromJSON(json_confirmados)
confirmados_bairro = rgdal::readOGR(geojson_confirmados_bairro)
mortes = fromJSON(json_mortes)
boletins = fromJSON(json_boletins)

# Inclui a coluna faixa etaria nos confirmados
# e classifica os dados por faixa etaria
confirmados$faixa_etaria <- 'Não Informado'
for (n in 1:nrow(confirmados))
{
  if (confirmados$idade[n] >= 80)
  {confirmados$faixa_etaria[n] <- "80 anos e mais"}
  if (confirmados$idade[n] <= 79)
  {confirmados$faixa_etaria[n] <- "75 a 79 anos"}
  if (confirmados$idade[n] <= 74)
  {confirmados$faixa_etaria[n] <- "70 a 74 anos"}
  if (confirmados$idade[n] <= 69)
  {confirmados$faixa_etaria[n] <- "65 a 69 anos"}
  if (confirmados$idade[n] <= 64)
  {confirmados$faixa_etaria[n] <- "60 a 64 anos"}
  if (confirmados$idade[n] <= 59)
  {confirmados$faixa_etaria[n] <- "55 a 59 anos"}
  if (confirmados$idade[n] <= 54)
  {confirmados$faixa_etaria[n] <- "50 a 54 anos"}
  if (confirmados$idade[n] <= 49)
  {confirmados$faixa_etaria[n] <- "45 a 49 anos"}
  if (confirmados$idade[n] <= 44)
  {confirmados$faixa_etaria[n] <- "40 a 44 anos"}
  if (confirmados$idade[n] <= 39)
  {confirmados$faixa_etaria[n] <- "34 a 39 anos"}
  if (confirmados$idade[n] <= 34)
  {confirmados$faixa_etaria[n] <- "30 a 34 anos"}
  if (confirmados$idade[n] <= 29)
  {confirmados$faixa_etaria[n] <- "25 a 49 anos"}
  if (confirmados$idade[n] <= 24)
  {confirmados$faixa_etaria[n] <- "20 a 24 anos"}
  if (confirmados$idade[n] <= 19)
  {confirmados$faixa_etaria[n] <- "15 a 19 anos"}
  if (confirmados$idade[n] <= 14)
  {confirmados$faixa_etaria[n] <- "10 a 14 anos"}
  if (confirmados$idade[n] <= 9)
  {confirmados$faixa_etaria[n] <- "5 a 9 anos"}
  if (confirmados$idade[n] <= 4)
  {confirmados$faixa_etaria[n] <- "0 a 4 anos"}
}
#
# Inclui a coluna faixa etaria nas mortes
# e classifica os dados por faixa etaria
mortes$faixa_etaria <- 'Não Informado'
for (n in 1:nrow(mortes))
{
  if (mortes$idade[n] >= 80)
  {mortes$faixa_etaria[n] <- "80 anos e mais"}
  if (mortes$idade[n] <= 79)
  {mortes$faixa_etaria[n] <- "75 a 79 anos"}
  if (mortes$idade[n] <= 74)
  {mortes$faixa_etaria[n] <- "70 a 74 anos"}
  if (mortes$idade[n] <= 69)
  {mortes$faixa_etaria[n] <- "65 a 69 anos"}
  if (mortes$idade[n] <= 64)
  {mortes$faixa_etaria[n] <- "60 a 64 anos"}
  if (mortes$idade[n] <= 59)
  {mortes$faixa_etaria[n] <- "55 a 59 anos"}
  if (mortes$idade[n] <= 54)
  {mortes$faixa_etaria[n] <- "50 a 54 anos"}
  if (mortes$idade[n] <= 49)
  {mortes$faixa_etaria[n] <- "45 a 49 anos"}
  if (mortes$idade[n] <= 44)
  {mortes$faixa_etaria[n] <- "40 a 44 anos"}
  if (mortes$idade[n] <= 39)
  {mortes$faixa_etaria[n] <- "34 a 39 anos"}
  if (mortes$idade[n] <= 34)
  {mortes$faixa_etaria[n] <- "30 a 34 anos"}
  if (mortes$idade[n] <= 29)
  {mortes$faixa_etaria[n] <- "25 a 49 anos"}
  if (mortes$idade[n] <= 24)
  {mortes$faixa_etaria[n] <- "20 a 24 anos"}
  if (mortes$idade[n] <= 19)
  {mortes$faixa_etaria[n] <- "15 a 19 anos"}
  if (mortes$idade[n] <= 14)
  {mortes$faixa_etaria[n] <- "10 a 14 anos"}
  if (mortes$idade[n] <= 9)
  {mortes$faixa_etaria[n] <- "5 a 9 anos"}
  if (mortes$idade[n] <= 4)
  {mortes$faixa_etaria[n] <- "0 a 4 anos"}
}

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
