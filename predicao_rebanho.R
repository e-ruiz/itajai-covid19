######@> Modelo Logístico ajustado para avaliar o tempo para
######@> contaminação total da população...
# 
# analisar o motivo de não aceitar variável no lugar do número absoluto
#

# população estimada 2019 - IBGE
populacao <- 219536

logistico <-filter(boletins[order(boletins$data),], confirmados_acumulados > 0)
logistico$city <-'Itajaí'
logistico$data <-ymd(logistico$data)
logistico$id.date <- do.call(c,
                             tapply(logistico$data, logistico$city,
                                    function(x) as.numeric(as.factor(x))))
######@> Modelo linear para determinação dos parâmetros iniciais...
log.mod0 <- lm(log(confirmados_acumulados) ~ id.date, data = logistico)
######@> Modelo exponencial...
#####@> Parametros iniciais...
log.par.a <- coef(log.mod0)[1] # (intercepto do modelo linear)
log.par.b <- coef(log.mod0)[2] # (coeficiente angular do modelo linear)


# mod2 <- nls(confirmed ~ cidade_analisada$estimated_population_2019[1] / (1 + exp(-(a + b*id.date))),
log.mod2 <- nls(confirmados_acumulados ~ populacao / (1 + exp(-(a + b*id.date))),
                data = logistico,
                start = list(a = coef(log.mod0)[1], b = coef(log.mod0)[2]),
                control = nls.control(maxiter = 200))

#####@> Matriz de predição...
log.mat.pred <- data.frame(date = seq(logistico$data[1],
                                      logistico$data[nrow(logistico)] + 299,
                                      1),
                           id.date = seq(1, nrow(logistico) + 300, 1))

#####@> Predizendo a média...
log.mat.pred$pred.m <- with(log.mat.pred,
                            head(populacao, 1) /
                              (1 + exp(-(coef(log.mod2)[1] +
                                           coef(log.mod2)[2]*id.date))))

#####@> Intervalo de confiança...
#confint2(mod2)
log.mat.pred$lwr <- with(log.mat.pred,
                         head(populacao, 1) /
                           (1 + exp(-(confint2(log.mod2)[1] +
                                        confint2(log.mod2)[2]*id.date))))
log.mat.pred$upr <- with(log.mat.pred,
                         head(populacao, 1) /
                           (1 + exp(-(confint2(log.mod2)[3] +
                                        confint2(log.mod2)[4]*id.date))))

#####@> Percentuais...
log.mat.pred$P <- with(log.mat.pred, round((pred.m / head(populacao, 1)) * 100, 2))


####@> Construindo uma matriz com parâmetros temporais de contágio...
### definir parametros conforme [mat.pred] definido acima

log.par <- data.frame(id.date = c(which(log.mat.pred$date == as.Date(max(boletins$data)))
                                  , (which(round(log.mat.pred$P,0) == 50))[1]
                                  , (which(round(log.mat.pred$P,0) == 70))[1]
                                  , (which(round(log.mat.pred$P,0) == 100))[1]
),
texto = c(" Atual ", " ", " "," "),
zero = rep(0, 4),
casos = c( round(log.mat.pred$pred.m[which(log.mat.pred$date == as.Date(max(boletins$data)))],0)
           ,round(log.mat.pred$pred.m[(which(round(log.mat.pred$P,0) == 50))[1]],0)
           ,round(log.mat.pred$pred.m[(which(round(log.mat.pred$P,0) == 70))[1]],0)
           ,round(log.mat.pred$pred.m[(which(round(log.mat.pred$P,0) == 100))[1]],0)
),
percentual = c( round(log.mat.pred$P[which(log.mat.pred$date == as.Date(max(boletins$data)))],0)
                ,round(log.mat.pred$P[(which(round(log.mat.pred$P,0) == 50))[1]],0)
                ,round(log.mat.pred$P[(which(round(log.mat.pred$P,0) == 70))[1]],0)
                ,round(log.mat.pred$P[(which(round(log.mat.pred$P,0) == 100))[1]],0)
                
),
cores = c('orange','#999900','green','#0033CC')
# qtd_dias = c(
#   as.numeric(log.mat.pred$date[(which(log.mat.pred$date == as.Date(max(boletins$data))))]-Sys.Date())  
# ,as.numeric(log.mat.pred$date[(which(round(log.mat.pred$P,0) == 50))[1]]-Sys.Date())  
# ,as.numeric(log.mat.pred$date[(which(round(log.mat.pred$P,0) == 70))[1]]-Sys.Date())  
# )
)


#Cria a tabela de predição
tblLogMat <- data.frame("Data" = log.mat.pred$date,
                     "Preditos" = log.mat.pred$pred.m,
                     "Erro.Percentual" = 0,
                     "Mínimo" = log.mat.pred$lwr,
                     "Máximo" = log.mat.pred$upr,
                     "Perc_Populacao" = log.mat.pred$P
                     )

tblLogOriginal <- data.frame("Data" = logistico$data,
                          "Confirmados" = logistico$confirmados_acumulados)

tblLogPredicao <- merge(x = tblLogOriginal, y = tblLogMat, by = 'Data',all.y = TRUE)
tblLogPredicao$Erro.Percentual = round(abs((tblLogPredicao$Confirmados - tblLogPredicao$Preditos) / tblLogPredicao$Confirmados),2)

