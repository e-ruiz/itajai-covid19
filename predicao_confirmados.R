
#
# Script com análise preditiva de novos casos confirmados de COVID19
#        diretamente da API do município de Itajaí-SC
#
# Modelo matemático desenvolvido pelo professor Dr. Rodrigo Sant'Ana <rsantana@univali.br>
#
# @link: https://api.itajai.sc.gov.br/covid19
# @author: Márcio Fossa <marcio.fossa@hotmail.com>
# @version: 27-jun-2020
#


#
# Se necessário instala dependências do repo oficial
#
if(!require(nlstools)) install.packages("nlstools", repos = "http://cran.us.r-project.org")


########################################################################
######@> Modelos para predição do número de casos confirmados...
######@>---------------------------------------=== Município de Itajai
df <-filter(boletins[order(boletins$data),], confirmados_acumulados > 0)
df$city <-'Itajaí'
df$data <-ymd(df$data)
df$id.date <- do.call(c,
                        tapply(df$data, df$city,
                                function(x) as.numeric(as.factor(x))))
######@> Modelo linear para determinação dos parâmetros iniciais...
mod0 <- lm(log(confirmados_acumulados) ~ id.date, data = df)
######@> Modelo exponencial...
#####@> Parametros iniciais...
par.a <- coef(mod0)[1] # (intercepto do modelo linear)
par.b <- coef(mod0)[2] # (coeficiente angular do modelo linear)
#####@> Ajustando um modelo exponencial...
mod1 <- nls(confirmados_acumulados ~ a*exp(b*id.date), data = df,
            start = list(a = par.a, b = par.b),
            control = nls.control(maxiter = 200))
#####@> Intervalo de confiança de 95% para os parâmetros estimados...
#####@> Predição para o observado...
df$Pred.m <- round(coef(mod1)["a"] *
                        exp(coef(mod1)["b"] * df$id.date))

####@> Intervalo de confiança...
df$Lwr <- round(confint2(mod1)[1] *
                    exp(confint2(mod1)[2] * df$id.date))
df$Upr <- round(confint2(mod1)[3] *
                    exp(confint2(mod1)[4] * df$id.date))
#####@> Predizendo os próximos 10 dias...
mat.pred <- data.frame(date = seq(df$data[1],
                                    df$data[nrow(df)] + 9,
                                    1),
                        id.date = seq(1, nrow(df) + 10, 1))

####@> Predição para os próximos 10 dias...
mat.pred$Pred.m <- round(coef(mod1)["a"] *
                            exp(coef(mod1)["b"] * mat.pred$id.date))

####@> Intervalo de confiança...
mat.pred$Lwr <- round(confint2(mod1)[1] *
                        exp(confint2(mod1)[2] * mat.pred$id.date))
mat.pred$Upr <- round(confint2(mod1)[3] *
                        exp(confint2(mod1)[4] * mat.pred$id.date))

#######################################################