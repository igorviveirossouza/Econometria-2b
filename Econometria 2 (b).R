# Set UP ------------------------------------------------------------------
rm(list=ls())

# Utilidades
if(!require(stringr)){install.packages("stringr")&require(stringr);require(stringr)}
if(!require(sjPlot)){install.packages("sjPlot")&require(sjPlot);require(sjPlot)}
if(!require(tidyverse)){install.packages("tidyverse")&require(tidyverse);require(tidyverse)}
if(!require(AER)){install.packages("AER")&require(AER);require(AER)}
if(!require(ICglm)){install.packages("ICglm")&require(ICglm);require(ICglm)}
if(!require(lubridate)){install.packages("lubridate")&require(lubridate);require(lubridate)}

# Pacotes de Séries Temporais
if(!require(urca)){install.packages("urca")&require(urca);require(urca)}
if(!require(dynlm)){install.packages("dynlm")&require(dynlm);require(dynlm)}
if(!require(forecast)){install.packages("forecast")&require(forecast);require(forecast)}
if(!require(tseries)){install.packages("tseries")&require(tseries);require(tseries)}
if(!require(fma)){install.packages("fma")&require(fma);require(fma)}
if(!require(TSstudio)){install.packages("TSstudio")&require(TSstudio);require(TSstudio)}

# Pacote para Séries Temporais multivariadas:
if(!require(MTS)){install.packages("MTS")&require(MTS);require(MTS)}
if(!require(vars)){install.packages("vars")&require(vars);require(vars)}
if(!require(aTSA)){install.packages("aTSA")&require(aTSA);require(aTSA)}

# Pacote para puxar dados do BACEN e no FRED, respectivamente:
if(!require(BETS)){install.packages("BETS")&require(BETS);require(BETS)}
if(!require(fredr)){install.packages("fredr")&require(fredr);require(fredr)}

# Pacote para puxar informações do IBGE
if(!require(ecoseries)){install.packages("ecoseries")&require(ecoseries);require(ecoseries)}
if(!require(caschrono)){install.packages("caschrono")&require(caschrono);require(caschrono)}

# Gráficos
if(!require(ggplot2)){install.packages("ggplot2")&require(ggplot2);require(ggplot2)}
if(!require(ggpubr)){install.packages("ggpubr")&require(ggpubr);require(ggpubr)}
if(!require(patchwork)){install.packages("patchwork")&require(patchwork);require(patchwork)}
if(!require(highcharter)){install.packages("highcharter")&require(highcharter);require(highcharter)}

# Pacotes com exmeplo de conjuntos de dados
if(!require(fpp2)){install.packages("fpp2")&require(fpp2);require(fpp2)}

#Séries financeiras:
if(!require(quantmod)){install.packages("quantmod")&require(quantmod);require(quantmod)}

par.ori <- par(no.readonly = T)


source("./global.R")

# Regressão em séries temporais -------------------------------------------

autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% change") + xlab("Year")

uschange %>%
  as.data.frame() %>%
  ggplot(aes(x=Income, y=Consumption)) +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
#> `geom_smooth()` using formula = 'y ~ x'


## ---- Exemplo de ruído branco -------------------------------------------------
n <- 200
y <- as.ts(rnorm(n))
plot(y);abline(0,0,col=2)        


# FAC teórica de um RB ----------------------------------------------------
par(mar=c(4,4,2,1),mfrow=c(2,2))
plot(ARMAacf(ar = 0.5, lag.max = 10),t="h",lwd=3,ylab="",xlab="",main=
       expression(Y[t]==0.5*Y[t-1] + epsilon[t])); par(new=T);
plot(ARMAacf(ar = 0.5, lag.max = 10),t="b",pch=19,col=2,axes=F,ylab="",xlab="")
mtext(expression(rho),side=2,las=1,line=3)

plot(ARMAacf(ar = 1, lag.max = 10),t="h",lwd=3,ylab="",xlab="",main=
       expression(Y[t]== Y[t-1] + epsilon[t])); par(new=T);
plot(ARMAacf(ar = 1, lag.max = 10),t="b",pch=19,col=2,axes=F,ylab="",xlab="")
mtext(expression(rho),side=2,las=1,line=3)

plot(ARMAacf(ar = -.8, lag.max = 10),t="h",lwd=3,ylab="",xlab="",main=
       expression(Y[t]== -0.8*Y[t-1] + epsilon[t])); par(new=T);
plot(ARMAacf(ar = -.8, lag.max = 10),t="b",pch=19,col=2,axes=F,ylab="",xlab="")
mtext(expression(rho),side=2,las=1,line=3);abline(0,0)

plot(ARMAacf(ma = 0.5, lag.max = 10),t="h",lwd=3,ylab="",xlab="",main=
       expression(Y[t]== 0.5*epsilon[t-1] + epsilon[t])); par(new=T);
plot(ARMAacf(ma = 0.5, lag.max = 10),t="b",pch=19,col=2,axes=F,ylab="",xlab="")
mtext(expression(rho),side=2,las=1,line=3)



# Gráficos de ACF amostrais ---------------------------------------------------------

## Autoregressivos ---------------------------------------------------------


n <- 45
AR1.a <- arima.sim(list(order = c(1,0,0), ar = 0.5), n )
AR1.b <- arima.sim(list(order = c(1,0,0), ar = 0.99), n )
AR1.c <- arima.sim(list(order = c(1,0,0), ar = -0.8), n)
AR1.d <- arima.sim(list(order = c(2,0,0), ar = c(.2,.6)), n)

par(mfrow=c(3,2))

acf(AR1.a,main="",ylim=c(-1,1),lag.max=20)
pacf(AR1.a,main="",ylim=c(-1,1),lag.max=20)
acf(AR1.b,main="",ylim=c(-1,1),lag.max=20)
pacf(AR1.b,main="",ylim=c(-1,1),lag.max=20)
acf(AR1.c,main="",ylim=c(-1,1),lag.max=20)
pacf(AR1.c,main="",ylim=c(-1,1),lag.max=20)

acf(AR1.d,main="",ylim=c(-1,1),lag.max=20)


## Médias Móveis -----------------------------------------------------------

n <- 600
MA1.a <- arima.sim(list(order = c(0,0,1), ma = 0.9), n )
MA1.b <- arima.sim(list(order = c(0,0,1), ma = 0.8), n )
MA1.c <- arima.sim(list(order = c(0,0,1), ma = -0.8), n)
MA2.a <- arima.sim(list(order = c(0,0,2), ma = c(.2,.6)), n)


par(mfrow=c(3,2))

acf(MA1.a,main="",ylim=c(-1,1),lag.max=20)
pacf(MA1.a,main="",ylim=c(-1,1),lag.max=20)
acf(MA1.b,main="",ylim=c(-1,1),lag.max=20)
pacf(MA1.b,main="",ylim=c(-1,1),lag.max=20)
acf(MA2.a,main="",ylim=c(-1,1),lag.max=20)
pacf(MA2.a,main="",ylim=c(-1,1),lag.max=20)


# FAC teórica e amostral do RB --------------------------------------------
par(mfrow=c(1,2))  
plot(ARMAacf(c(0, 0), lag.max = 10),t="h",lwd=3,ylab="",xlab="",main=
       expression( paste(Y[t]==epsilon[t]%~%"","(0,",sigma[epsilon]^2, ")",""%->%
                           "RB"))); par(new=T)
plot(ARMAacf(c(0, 0), lag.max = 10),t="p",pch=19,col=2,axes=F,ylab="",xlab="")
mtext(expression(rho),side=2,las=1,line=3)
set.seed(350); n = 250; acf(rnorm(n),main="FAC amostral de um RB",lag.max=10)  


# Testes diagnósticos nos resíduos ----------------------------------------
##  US macro data  ---------------------------------------------------------

data("USMacroG")
head(USMacroG)
class(USMacroG)

autoplot(USMacroG)
autoplot(USMacroG[,"interest"])
p1 <- autoplot(USMacroG[,"consumption"],colour="blue")
p2 <- autoplot(USMacroG[,"dpi"])

p1 + p2

par(mfrow=c(1,2))
acf(USMacroG[,"dpi"]); acf(USMacroG[,"consumption"])


consump1 <- dynlm(consumption ~ dpi + L(dpi),data = USMacroG)
consump2 <- dynlm(consumption ~ dpi + L(consumption), data = USMacroG)

tab_model(consump1,consump2,  title = "Modelo para o consumo", show.ci = F,
          dv.labels = c("modelo 1",'modelo 2'), digits = 5)

# Graficamente:
NEWDATA <- merge(as.zoo(USMacroG[,"consumption"]), fitted(consump1),
                 fitted(consump2), 0, residuals(consump1), residuals(consump2))

plot(NEWDATA, screens = rep(1:2,c(3, 3)), col = rep(c(1, 'red', 'green'), 2), 
     xlab = "Time", ylab = c("Fitted values", "Residuals"), 
     lwd=c(4,2,3,1,1,1), main = "")

par(mfrow=c(1,2))

atp1 <- residuals(consump1) |> autoplot(main="modelo 1", colour = "red",lwd=.8)
atp2 <- residuals(consump2) |> autoplot(main="modelo 2", colour = "blue",lwd=.8)
atp1 + atp2

acf(residuals(consump1),col='darkred'); acf(residuals(consump2),col='darkblue')

# Teste de seleção de modelos não aninhados: Encompassing Test

encomptest(consump1,consump2)

# Estimando o modelo abrangente (encompassing model)

cons_lmE <- dynlm(consumption ~ dpi + L(dpi) + L(consumption), data = USMacroG)
tab_model(consump1,consump2,cons_lmE, title = "Modelo para o consumo",
          show.ci = F, dv.labels = c("modelo 1",'modelo 2','modelo 3'), digits = 5)

NEWDATA2 <- merge(as.zoo(USMacroG[,"consumption"]), fitted(consump1),
                  fitted(consump2),fitted(cons_lmE), 0, residuals(consump1), 
                  residuals(consump2),residuals(cons_lmE))

plot(NEWDATA2, screens = rep(1:2, c(4, 4)), col = rep(c(1, 'darkred', 
                                                        'darkblue','orange'), 2), xlab = "Time", 
     ylab = c("Fitted values", "Residuals"), lwd=c(4,2,3,1,1,1), main = "")

### Autocorrelação -------------------------------------------------------------  

par(mfrow=c(1,3))
acf(residuals(consump1),col='darkred',lwd=3) 
acf(residuals(consump2),col='darkblue',lwd=3)
acf(residuals(cons_lmE),col='darkorange',lwd=3)

# Testes de autocorrelação

bgtest(consump1,order=1); bgtest(consump2,order=1); bgtest(cons_lmE,order=1)
bgtest(consump1,order=2); bgtest(consump2,order=2); bgtest(cons_lmE,order=2)

# Durbin- Watson não deveria ser usado nos modelos 2 e lmE
dwtest(consump1);  dwtest(consump2);  dwtest(cons_lmE)

Box.test(residuals(consump1), type = "Ljung-Box",lag = 1)
Box.test(residuals(consump2), type = "Ljung-Box",lag = 1)
Box.test(residuals(cons_lmE), type = "Ljung-Box",lag = 1)

Box.test(residuals(consump1), type = "Ljung-Box",lag = 2)
Box.test(residuals(consump2), type = "Ljung-Box",lag = 2)
Box.test(residuals(cons_lmE), type = "Ljung-Box",lag = 2)


### Heterocedasticidade --------------------------------------------------------
par(mfrow=c(1,2))
plot(resid(consump2)) ; plot(resid(cons_lmE))
plot(resid(consump2)^2) ; plot(resid(cons_lmE)^2)

# Testar a presença de heterocedasticidade:
bptest(consump2) # Bresuch-Pagan Teste
gqtest(consump2) # Goldfeld-Quandt teste

bptest(cons_lmE) # Bresuch-Pagan Teste
gqtest(cons_lmE) # Goldfeld-Quandt teste


rbind(SE = sqrt(diag(vcov(cons_lmE))),
      QS = sqrt(diag(kernHAC(cons_lmE))),
      NW = sqrt(diag(NeweyWest(cons_lmE))))

coeftest(cons_lmE,vcov = NeweyWest(cons_lmE))

## Inflação vs câmbio ----------------------------------------------------------

SEARCH <- BETSsearch(periodicity = "M", view = FALSE)

IPCA_atual   <- BETSget(code=c(433),  from = "2000-01-01")
Cambio_atual <- BETSget(code=c(3695), from = "2000-01-01")


INFLA_BR <- cbind(IPCA_atual,Cambio_atual[,2]) %>%
  `colnames<-`(c('ipca','cambio'))

plot(INFLA_BR)

#Comparando ACF's:
par(mfrow=c(1,2));acf(INFLA_BR[,1]); acf(INFLA_BR[,2])

# Criando a série de variação cambial:
INFLA_BR <- (cbind(INFLA_BR, c(NA,diff(log(INFLA_BR[,2])))))%>%
  `colnames<-`(c('ipca','cambio','d.cambio'))

plot(INFLA_BR)

acf( na.remove(INFLA_BR) )

# Inflação vs câmbio: sem relação? resultado incoerente?

dynlm(ipca~d.cambio,data=INFLA_BR)%>%summary()

# Hipótese: Variações cambiais antecipam a variação de IPCA: inflação reage ao
# dólar defasadamente, vejamos graficamente:

ylim = c(1.05*min(na.remove(INFLA_BR[,'ipca']),na.remove(INFLA_BR[,'d.cambio'])),
         1.05*max(na.remove(INFLA_BR[,'ipca']),na.remove(INFLA_BR[,'d.cambio'])))

par(mfrow=c(1,1)); plot(INFLA_BR[,'ipca'],ylim=ylim,lwd=2)
par(new=T); plot(INFLA_BR[,'d.cambio'],ylim=ylim, col='red',lwd=2)

# Médias móveis padronizadas:

MA_IPCA <- na.remove(ma(INFLA_BR[,'ipca'], order=12,centre = F))
MA_IPCA <- (MA_IPCA-mean(MA_IPCA))/sd(MA_IPCA)
MA_USDL <- na.remove(ma(INFLA_BR[,'d.cambio'], order=12,centre = F))
MA_USDL <- (MA_USDL-mean(MA_USDL))/sd(MA_USDL)

ylim = c(1.05*min(MA_IPCA,MA_USDL),1.05*max(MA_IPCA,MA_USDL))

plot(MA_IPCA,ylim=ylim);par(new=T);plot(MA_USDL,ylim=ylim,col='red')

(mod01 <- dynlm(ipca~d.cambio+L(d.cambio)+ L(d.cambio,2) + L(d.cambio,3) + 
                  L(d.cambio,4) + L(d.cambio,5) + L(d.cambio,6), 
                data=INFLA_BR))%>%summary()

NEWDATA <- merge(as.zoo(INFLA_BR[,"ipca"]),fitted(mod01), 0,residuals(mod01))

plot(NEWDATA, screens = rep(1:2,c(2, 2)), col = c('darkred', 'darkblue',
                                                  'black','darkorange'),xlab = "Time", lwd=c(2,2,1,1), main = "",
     ylab = c("Fitted values", "Residuals"))

acf(residuals(mod01),col='darkred')

# Modelo atualizado com o comando update
(mod02 <- update(mod01, ~ .  + L(ipca)))%>%summary()

NEWDATA <- merge(as.zoo(INFLA_BR[,"ipca"]),fitted(mod02), 0,residuals(mod02))

plot(NEWDATA, screens = rep(1:2,c(2, 2)), col = c('darkred', 'darkblue',
                                                  'black','darkorange'),xlab = "Time", lwd=c(2,2,1,1), main = "",
     ylab = c("Fitted values", "Residuals"))

acf(residuals(mod02),col='darkred')

bgtest(mod02)
Box.test(residuals(mod02), type = "Ljung-Box")

# Testar a presença de heterocedasticidade:
plot(resid(mod02))
plot(resid(mod02)^2)

bptest(mod02) # Bresuch-Pagan Teste
gqtest(mod02) # Goldfeld-Quandt teste


cbind(SE = sqrt(diag(vcov(mod02))),
      #    QS = sqrt(diag(kernHAC(mod02))),
      NW = sqrt(diag(NeweyWest(mod02))))

coeftest(mod02,NeweyWest(mod02))

# Teste Reset de especificação de modelos:
resettest(mod02)

# Update de Lags do IPCA:
(mod03 <- update(mod02, ~ .  + L(ipca,2)))%>%summary()
reset(mod03)
(mod04 <- update(mod03, ~ .  + L(ipca,3)))%>%summary()
reset(mod04)
(mod05 <- update(mod04, ~ .  + L(ipca,4)))%>%summary()
reset(mod05)
(mod06 <- update(mod05, ~ .  + L(ipca,5)))%>%summary()
reset(mod06)
(mod07 <- update(mod06, ~ .  + L(ipca,6)))%>%summary()
reset(mod07)

(mod08 <- update(mod04, ~ .  + season(ipca)))%>%summary()
reset(mod08)


mod00 <- dynlm(ipca~L(ipca,1), data=INFLA_BR)
summary(mod00)
summary(ipca01)
reset(mod00)


  #BOX-JENKINS para IPCA:
plot(IPCA_atual)
par(mfrow=c(1,2))

acf(IPCA_atual);pacf(IPCA_atual)

## Paridade de poder de compra ---------------------------------------------


CPI_USA      <- BETSget(code=c(3794), from = "2000-01-01")
IPCA_index   <- NULL; IPCA_index[1] <- 100
for(i in 2:length(IPCA_atual)){
  IPCA_index[i] <- IPCA_index[i-1]*(1+IPCA_atual[i]/100)
}
par(mfrow=c(2,1))
(IPCA_index <- ts(IPCA_index,start = c(2000,01),frequency = 12))%>%plot()  
plot(CPI_USA,t='l')
plot(diff(IPCA_index))

Cambio_atual 

# Para acessar os dados do FRED você deve gerar sua própria
# API no site 
api.fred <- 'Cole sua chave aqui'

fredr_set_key(api.fred)

fredr(
  series_id = "CPALTT01USM657N",
  observation_start = as.Date("2000-01-01") 
  # frequency = "q",
  # aggregation_method = "avg",
  # units = "log"
) -> CPI_USA_FRED

# Explicação da série:
#     Units: Growth rate previous period, Not Seasonally Adjusted  
CPI_USA <- ts(CPI_USA_FRED$value,start = c(2000,01),frequency = 12)
plot(CPI_USA)
CPI_index   <- NULL; CPI_index[1] <- 100
for(i in 2:length(CPI_USA)){
  CPI_index[i] <- CPI_index[i-1]*(1+CPI_USA[i]/100)
}
CPI_index <- ts(CPI_index,start = c(2000,01),frequency = 12)

par(mfrow=c(2,1));plot(IPCA_index);plot(CPI_index,col=2)
PPP <- CPI_index/IPCA_index
Cambio_atual <- ts(Cambio_atual$value,frequency = 12,start = c(2000,01))
par(mfrow=c(2,1));plot(PPP);plot(Cambio_atual)

(ruido <- log(Cambio_atual) - log(PPP))%>%plot()
acf(ruido,col=2,lwd=2);pacf(ruido,col=2,lwd=2)

par(mfrow=c(2,1));plot(ruido);plot(Cambio_atual)


par(mfrow=c(2,2))
acf(ruido,col=2,lwd=2);pacf(ruido,col=2,lwd=2)
acf(Cambio_atual,col=2,lwd=3);pacf(Cambio_atual,col=2,lwd=3)

# Simulação de modelos aditivos e multiplicativos -------------------------
n     <- 251
betaT <- .5
T_t   <- betaT*seq(1:n)
Mu_t  <- 10
S_t   <- abs(cos(seq(0.2855993,pi,length.out=12)))*Mu_t
S_t   <- (rep(S_t,ceiling(n/length(S_t))))[1:n]
set.seed(432)
z_t   <- rnorm(n)
par(mfrow=c(1,2))
plot((Yt_add <- Mu_t + T_t + S_t + 4*z_t),t="l",xlab="tempo", ylab="",
     main = expression(Y[t] == mu[t] + 'T'[t] + S[t] + epsilon[t])
)
plot((Yt_mult <- exp(Yt_add/3e1)),t="l",xlab="tempo", ylab="",
     main = expression(Y[t] == mu[t] %*% 'T'[t] %*% S[t] %*% epsilon[t])
)



# Série New York Births ---------------------------------------------------

births <- ts(
  scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
  , frequency = 12, start = c(1946, 1)
)
plot(births)

ggseasonplot(births)  

birthsComp <- decompose(births)
plot(birthsComp)

plot(birthsComp$random)
acf(na.remove(birthsComp$random))

ggseasonplot(birthsComp$random)  

# ALternativa via regressão:

seasonaldummy(births)
DECOMPOSE <- dynlm(births~trend(births)+season(births))

# Não é a mesma coisa que usar o filtro 'decompose' pois o mesmo usa
# uma média móvel centrada para realizar o cálculo da trend:
cbind(DECOMPOSE$residuals,birthsComp$random)

plot(DECOMPOSE$residuals)
acf(na.remove(DECOMPOSE$residuals))




#  Modelando o LOG da série NY Births ----------------------------------------

l_births <- log(births)
par(mfrow=c(2,1));plot(l_births,col=2);plot(births)

l_birthsComp <- decompose(l_births)
plot(l_birthsComp)

plot(l_birthsComp$random)
acf(na.remove(l_birthsComp$random))

par(mfrow=c(2,1))
acf(na.remove(l_birthsComp$random),col=2,lwd=2)
acf(na.remove(birthsComp$random),lwd=2)


# Série Airpassanger ----------------------------------------------------------

data(airpass)
autoplot(airpass)    
ggseasonplot(airpass)  
ggseasonplot(airpass,polar=T)  
airpassCompAdd <- decompose(airpass)
airpassCompMul <- decompose(airpass,type = "multiplicative")

plot(airpassCompAdd)
plot(airpassCompMul)

par(mfrow=c(1,2))
acf(na.remove(airpassCompAdd$random),col=2);acf(na.remove(airpassCompMul$random))

# Olhando para o log (equivale a modelar a série como multiplicativa)   

autoplot(log(airpass),col=2) + autoplot((airpass))
ggseasonplot(log(airpass))
l_airpassCompAdd <- decompose(log(airpass))
plot(l_airpassCompAdd)
# Análise do resíduo de log(airpass)  
par(mfrow=c(1,2))
acf(na.remove(l_airpassCompAdd$random),col=2);acf(na.remove(airpassCompMul$random))

plot(l_airpassCompAdd$random);plot(airpassCompMul$random)

sd(na.remove(l_airpassCompAdd$random));sd(na.remove(airpassCompMul$random))
# Olhando para a sazonalidade:
plot(l_airpassCompAdd$seasonal);plot(airpassCompMul$seasonal)

G1 <- ggseasonplot(l_airpassCompAdd$seasonal) + theme(legend.position = "none")
G2 <- ggseasonplot(l_airpassCompAdd$seasonal,polar=T)  + theme(legend.position = "none")

G1 + G2

# Decomposição do random


decompose2 <- decompose(airpassCompMul$random)

plot(decompose2)

acf( na.remove(decompose2$random) )

# Previsões:

autoplot(forecast::naive(log(airpass), h = 20)) + autoplot(forecast::snaive(log(airpass), h = 20))

# A decomposição feita pelo comando decompose não funciona no 
#  preditor do forecast:
forecast(l_airpassCompAdd)

# Poderíamos usar regressão contra o termo de tendência e a sazonalidade com o
# comando a seguir:  
plot(tslm(log(airpass) ~ trend + season)$residuals)
plot(l_airpassCompAdd$random)  

# Mas notem que o resíduo obtido não é o mesmo. Porquê? Por que o comando 
#  decompose faz seu detrend usando uma média móvel centrada.
# Podemos recriar a estrutura usando o comando de média móvel  
Media_Movel <- ma(log(airpass), order=12,centre = T)
par(mfrow=c(1,1)) ; plot(Media_Movel)

# Temos agora a série detrended 
(airpass_detrend <- log(airpass)-Media_Movel)%>%plot()

# E o resíduo desazonalisado:  
(l_airpassCompAdd_random <-
    tslm(airpass_detrend ~ season + Media_Movel)$resid)%>%plot()

# Será preciso ajustar a base de dados para remover os 'missings':

BASE_FORECAST <- cbind(airpass,Media_Movel)%>%na.remove()

Modelo.decomposicao <- tslm(log(BASE_FORECAST[,'airpass']) ~ 
                              + BASE_FORECAST[,'Media_Movel'],data=BASE_FORECAST)


autoplot(forecast::naive(log(airpass), h = 20)) + 
  autoplot(forecast::snaive(log(airpass), h = 20))

forecast(Modelo.decomposicao)

cbind(l_airpassCompAdd$random,residuo)    

Modelo.decomposicao$resid%>%
  plot(,lwd=2)
par(new=T)
plot(l_airpassCompAdd$random,col=2,lty=2)


# Modelos ARIMA -----------------------------------------------------------

autoplot(IPCA_atual)
par(mfrow=c(2,1));acf(IPCA_atual);pacf(IPCA_atual)

ipca01 <- arima(IPCA_atual,order=c(1,0,0))

summary(ipca01)
reset(ipca01)

# Minha série é um RB? 
Box.test(IPCA_atual, lag = 1, type = "Ljung")


ipca01
tsdiag(ipca01)
par(mfrow=c(1,2))
acf(ipca01$residuals)
pacf(ipca01$residuals)

#Ljung-Box: Testa se a Autocorrelação de é igual a zero, H0: Gamma_k = 0.

#Outros modelos
ipca02 <- arima(IPCA_atual,order=c(0,0,4))
ipca03 <- arima(IPCA_atual,order=c(1,0,1))

ipca02
tsdiag(ipca02)
plot(ipca02$residuals)
acf(ipca02$residuals)

ipca03
tsdiag(ipca03)
plot(ipca03$residuals)
acf(ipca03$residuals)
# Critérios de informação para seleção de modelos:  
stats::AIC(ipca01,ipca02,ipca03)
stats::BIC(ipca01,ipca02,ipca03)
ICglm::HQIC(ipca01); ICglm::HQIC(ipca02);ICglm::HQIC(ipca03)

# Previsão com modelo escolhido 
plot(forecast(ipca01,h=10))


## Modelando o CPI USA   --------------------------------------------------- 
fredr(
  series_id = "CPIAUCSL",
  observation_start = as.Date("1947-01-01") 
  # frequency = "q",
  # aggregation_method = "avg",
  # units = "log"
) -> CPI_USA_SA_FRED

# A série acima é: 
#  Consumer Price Index for All Urban Consumers: 
#  All Items in U.S. City Average (CPIAUCSL)	- Seasonally Adjusted


CPI_USA_index <- ts(CPI_USA_SA_FRED$value,frequency = 12,start = c(1947,1))
plot(CPI_USA_index)
plot(diff(log(CPI_USA_index) ))

# A série claramente apresenta um prolema de mensuração antes da década de 80.
# Vamos cortar nossa análise nesse período:

(CPI_USA_inflation <- CPI_USA_index%>%diff()%>%window(start=c(1980,1)))%>%
  plot()
# Notem um comportamento heterocedástico da série. Vamos tomar o log e 
# verficar se há mélhoras:

(CPI_USA_inflation_log <- CPI_USA_index%>%log()%>%diff()%>%
    window(start=c(1980,1)))%>% plot()

# periodo <- c("1980-01-01/1999-05-01")
# chart.TimeSeries(CPI_USA_inflation,period.areas = periodo,
#                  period.color = 'lightgrey')
# chart.TimeSeries(CPI_USA_inflation_log)

# FAC's e FACP's
par(mfrow=c(2,2))
acf(CPI_USA_inflation);pacf(CPI_USA_inflation)
acf(CPI_USA_inflation_log);pacf(CPI_USA_inflation_log)


# ARMA, mas qual? Vamos começar com o processo mais simples: ARMA(1,1)
cpi_usa_ARMA11 <- CPI_USA_inflation_log%>%arima(order = c(1,0,1))
cpi_usa_ARMA12 <- CPI_USA_inflation_log%>%arima(order = c(1,0,2))
cpi_usa_ARMA21 <- CPI_USA_inflation_log%>%arima(order = c(2,0,1))
cpi_usa_ARMA22 <- CPI_USA_inflation_log%>%arima(order = c(2,0,2))

# Analisando a estabilidade dos modelos:
# IMPORTANTE: Notem que aqui o gráfico coloca o INVERSO das raízes do 
# polinômio. Logo se as raízes do polinômio têm de estar fora do círculo 
# unitário, as raízes inversas têm de estar dentro!!!!
plot(cpi_usa_ARMA11)
plot(cpi_usa_ARMA12)
plot(cpi_usa_ARMA21)
plot(cpi_usa_ARMA22)

# Todos os modelos são estáveis?

# Análise dos resíduos:
par(mfrow=c(2,2))
plot(cpi_usa_ARMA11$residuals)
plot(cpi_usa_ARMA12$residuals)
plot(cpi_usa_ARMA21$residuals)
plot(cpi_usa_ARMA22$residuals)

par(mfrow=c(2,2))
acf(cpi_usa_ARMA11$residuals)
acf(cpi_usa_ARMA12$residuals)
acf(cpi_usa_ARMA21$residuals)
acf(cpi_usa_ARMA22$residuals)

pacf(cpi_usa_ARMA11$residuals)
pacf(cpi_usa_ARMA12$residuals)
pacf(cpi_usa_ARMA21$residuals)
pacf(cpi_usa_ARMA22$residuals)



tab_model(
  cpi_usa_ARMA11,
  cpi_usa_ARMA12,
  cpi_usa_ARMA21,
  cpi_usa_ARMA22,
  show.ci = F,
  show.aic = T,
  show.r2 = T
)



stats::AIC(cpi_usa_ARMA11,
           cpi_usa_ARMA12,
           cpi_usa_ARMA21,
           cpi_usa_ARMA22)

stats::BIC(cpi_usa_ARMA11,
           cpi_usa_ARMA12,
           cpi_usa_ARMA21,
           cpi_usa_ARMA22)  

# E um componente MA(3)?

cpi_usa_ARMA13 <- CPI_USA_inflation_log%>%arima(order = c(1,0,3))
cpi_usa_ARMA23 <- CPI_USA_inflation_log%>%arima(order = c(2,0,3))
plot(cpi_usa_ARMA13)
plot(cpi_usa_ARMA23)
tab_model(cpi_usa_ARMA13,
          cpi_usa_ARMA23,
          show.ci = F,
          show.aic = T)


#ARMA(p,q) ou AR(3)?
cpi_usa_ARMA30 <- CPI_USA_inflation_log%>%arima(order = c(3,0,0))  
cpi_usa_ARMA32 <- CPI_USA_inflation_log%>%arima(order = c(3,0,2))  

plot(cpi_usa_ARMA30)
plot(cpi_usa_ARMA32)
tab_model(cpi_usa_ARMA30,
          cpi_usa_ARMA32,
          show.ci = F,
          show.aic = T)

stats::AIC(cpi_usa_ARMA11,
           cpi_usa_ARMA12,
           cpi_usa_ARMA21,
           cpi_usa_ARMA22,
           cpi_usa_ARMA23,
           cpi_usa_ARMA30)

stats::BIC(cpi_usa_ARMA11,
           cpi_usa_ARMA12,
           cpi_usa_ARMA21,
           cpi_usa_ARMA22
           cpi_usa_ARMA23,
           cpi_usa_ARMA30)

require()  
# Qual modelo adequado?  


## Modelando o ITUB3 -------------------------------------------------------


getSymbols("ITUB3.SA")
ITUB3.SA <- ITUB3.SA |> janitor::clean_names()
ITUB     <- ITUB3.SA$itub3_sa_close

# Série nos níveis
acf(ITUB)
hitub <- highchart(type = "stock") %>% 
  hc_title(text = "ITUB-3") %>% 
  hc_add_series(ITUB, id = "ts", color = 'black') %>%
  hc_exporting(enabled = TRUE)

hditub <- highchart(type = "stock") %>% 
  hc_title(text = "ITUB-3") %>% 
  hc_add_series(diff(ITUB), id = "ts", color = 'blue') %>%
  hc_exporting(enabled = TRUE)


hitub
hditub

par(mfrow=c(1,2))
diff(ITUB) |> acf(na.action = na.pass)
diff(ITUB) |> pacf(na.action = na.pass)

auto.arima(ITUB)

residuals <- residuals(auto.arima(ITUB))


MA1 <- arima(ITUB,order=c(0,1,1))

summary(MA1)

residuo <- ts(MA1 %>% residuals(), start = "2007-01-02")

end(residuo)

library(FinTS)

hdres <- highchart(type = "stock") %>% 
  hc_title(text = "ITUB-3") %>% 
  hc_add_series(residuo, id = "ts", color = 'red') %>%
  hc_exporting(enabled = TRUE)


hdres

residuo %>% ArchTest()

# Se tomarmos os logs:

ITUB.log     <- log(ITUB3.SA$itub3_sa_close)

# Série nos níveis
acf(ITUB.log)
hitub.log <- highchart(type = "stock") %>% 
  hc_title(text = "ITUB-3") %>% 
  hc_add_series(ITUB.log, id = "ts", color = 'black') %>%
  hc_exporting(enabled = TRUE)

hditub.log <- highchart(type = "stock") %>% 
  hc_title(text = "ITUB-3") %>% 
  hc_add_series(diff(ITUB.log), id = "ts", color = 'blue') %>%
  hc_exporting(enabled = TRUE)


hitub.log
hditub.log

par(mfrow=c(1,2))
diff(ITUB.log) |> acf(na.action = na.pass)
diff(ITUB.log) |> pacf(na.action = na.pass)

auto.arima(ITUB.log) # Sério?

residuals <- residuals(auto.arima(ITUB.log))


### Comparando previsões -------------------------------------------------------

ITUB_train <- window(ITUB, end = today()-365)
ITUB_teste <- window(ITUB, start = end(ITUB_train)+1)

MA3   <-  arima(ITUB_train,order = c(0,1,3))
MA5   <-  arima(ITUB_train,order = c(0,1,5))
AR5   <-  arima(ITUB_train,order = c(5,1,0))

MA3_forecast <- forecast(MA3, h = length(ITUB_teste))
MA5_forecast <- forecast(MA5, h = length(ITUB_teste))
AR5_forecast <- forecast(AR5, h = length(ITUB_teste))

TSstudio::plot_forecast(MA3_forecast)
TSstudio::plot_forecast(MA5_forecast)
TSstudio::plot_forecast(AR5_forecast)

test_forecast(actual = ITUB, # data keseluruhan
              forecast.obj = MA3_forecast, # hasil object forecast
              train = ITUB_train, # data train
              test = ITUB_teste) 

ITUB |> length()

ITUB_train |> length() + ITUB_teste |> length()

# Defina uma variável dependente constante para o teste.
constante <- rep(1, length(residuals))

# Realize o teste de Breusch-Pagan
hetero_test <- lmtest::bptest(residuals ~ constante)

# Exiba os resultados do teste
print(hetero_test)


# Raízes unitárias --------------------------------------------------------


# Dados de PIB (Nominal)
PIB_anual <- BETSget(1207,from = "1962-12-31")
PIB_anual <- PIB_anual[,2]%>%ts(start="1962")

PIB_trim_desaz  <- BETSget(22109,from = "1995-01-01") # Pib Real

par(mfrow=c(1,2)); plot(PIB_anual); plot(PIB_trim_desaz)

# Vamos deflacionar o PIB pelo IGP-DI
IGP_DI_mes    <- BETSget(code=c(190),  from = "1962-01-01")

# Correção do IGP-DI: 
IGP_DI_mes[which(IGP_DI_mes$date=="1994-07-01"),2] <- 24.71
IGP_DI_mes|>filter(date=="1994-07-01")
# Transforma em TS:
IGP_DI_mes    <- IGP_DI_mes[,2]%>%ts(start=c(1962,1),frequency=12)

# Vamos construir a série em número índice:
IGP_DI_INDEX_mes <- NULL; IGP_DI_INDEX_mes[1] <- 100
for(i in 1:length(IGP_DI_mes)){
  IGP_DI_INDEX_mes[(i+1)] <- IGP_DI_INDEX_mes[i]*(1+IGP_DI_mes[i]/100)
}

IGP_DI_INDEX_mes <- IGP_DI_INDEX_mes%>%ts(frequency = 12,start = c(1961,12))
g1 <- autoplot(IGP_DI_INDEX_mes, main = "Índice")
g2 <- autoplot(IGP_DI_mes, main = "Variação % a.m")
g1 + g2

# Vamos anualizar a série usando dummies de sazonalidade:

SEASON <- seasonaldummy(IGP_DI_INDEX_mes)%>%as.data.frame()
SEASON$Dec <- (1 - SEASON%>%apply(1,sum))
SEASON$IGP_DI_INDEX_mes <- IGP_DI_INDEX_mes

IGP_DI_ANUAL <- SEASON%>%filter(Dec==1)%>%
  dplyr::select(IGP_DI_INDEX_mes)%>%
  `colnames<-`('IGP_DI_ANUAL')%>%
  ts(frequency = 1,start = c(1961))


IGP_DI_TRIM  <- SEASON%>%filter(Mar==1|
                                  Jun==1|
                                  Sep==1|
                                  Dec==1
)%>%
  dplyr::select(IGP_DI_INDEX_mes)%>%
  `colnames<-`('IGP_DI_TRIM')%>%
  ts(frequency = 4,start = start(IGP_DI_INDEX_mes))

IGP_DI_ANUAL <- IGP_DI_ANUAL%>%window(start=start(PIB_anual),end=end(PIB_anual))  
PIB_ANUAL_REAL_IGP <- (PIB_anual*IGP_DI_ANUAL[length(IGP_DI_ANUAL)]/IGP_DI_ANUAL)%>%
  ts(frequency = 1,start = c(1962,1))


graf01 <- IGP_DI_ANUAL|>autoplot() + ylab("IGP-DI")
graf02 <- (IGP_DI_ANUAL[length(IGP_DI_ANUAL)]/IGP_DI_ANUAL)|>autoplot() 
+ ylab("Deflator")
graf03 <- PIB_anual|>window()|>autoplot(color="red") + ylab("PIB Nominal, R$")
graf04 <- PIB_ANUAL_REAL_IGP|>autoplot(color="blue")  + ylab("PIB Real, R$")

graf01 + graf03 + graf02 + graf04


fUnitRoots::urdfTest(PIB_ANUAL_REAL_IGP) -> ur01


urca:: ur.df(PIB_ANUAL_REAL_IGP,type=c("drift")) %>% summary()

ur01 %>% summary()


PIB_ANUAL_REAL_IGP %>% test_unit_root()

# Regressão espúria -------------------------------------------------------


# Download  Stockholm stock exchange index data since 2010
getSymbols("^OMX", src = "yahoo", from = "2010-01-01")

# Convert daily data to quarterly data
OMX_quarterly <- to.quarterly(OMX, OHLC = FALSE, drop = TRUE)
OMX_quarterly|>class()
autoplot(OMX_quarterly$OMX.Close)

data <- merge(OMX_quarterly[ , 4, drop = FALSE], PIB_trim_desaz, join = "inner")

par(mfrow=c(1,1))
par(mar = c(5, 4, 4, 6))
plot.ts(data$OMX.Close, main = "OMX vs. GDP", ylab = "OMX.Close", xlab = "",
        col=2)
par(new = TRUE)
plot.ts(data$PIB_trim_desaz, ylab = "GDP", xlab = "", axes = FALSE)
axis(side = 4, at = pretty(range(data$PIB_trim_desaz)))
mtext("GDP", side = 4, line = 3)
legend("topleft", legend = c("OMX.Close", "GDP"), lty = 1, col = c(1, 2))

# Regress OMX on GDP
reg <- lm(OMX.Close ~ PIB_trim_desaz, data = data)

# Print regression summary
summary(reg)
reg$residuals|>plot(type="l")
acf(reg$residuals)
# Test for unit roots in the residuals

# Averiguar RU por um teste ADF é um procedimento comum, mas que pode levar a 
# erros, principalmente para pequenas amostras:
adf.test(reg$residuals)

# O correto é usaro procedimento de Engle-Granger que corrige os valores críticos
aTSA::coint.test(data$OMX.Close %>% as.vector(), # forçando os objetos a possuírem a estrura correta
                 data$PIB_trim_desaz %>% as.matrix()
                 )

# Usando cointegração para verificar a possibilidade de Long-short --------

getSymbols(c("ITUB4.SA","ITUB3.SA","ITSA3.SA","ITSA4.SA"))

ITUB4.SA <- ITUB4.SA|>janitor::clean_names()
ITUB3.SA <- ITUB3.SA|>janitor::clean_names()
ITSA4.SA <- ITSA4.SA|>janitor::clean_names()
ITSA3.SA <- ITSA3.SA|>janitor::clean_names()

getSymbols(c("BBDC4.SA","BBDC3.SA"))

BBDC4.SA <- BBDC4.SA|>janitor::clean_names()
BBDC3.SA <- BBDC3.SA|>janitor::clean_names()

getSymbols(c("PETR4.SA","PETR3.SA"))
PETR4.SA <- PETR4.SA|>janitor::clean_names()
PETR3.SA <- PETR3.SA|>janitor::clean_names()



## Pega os preços de fechamento --------------------------------------------


### Bradesco ----------------------------------------------------------------

BBDC <- cbind(BBDC4.SA$bbdc4_sa_close,BBDC3.SA$bbdc3_sa_close)
plot(BBDC)

ITAU <- cbind(ITUB4.SA$itub4_sa_close,
              ITUB3.SA$itub3_sa_close,
              ITSA3.SA$itsa3_sa_close,
              ITSA4.SA$itsa4_sa_close
              )
PETR <- cbind(PETR4.SA$petr4_sa_close ,PETR3.SA$petr3_sa_close)

plot(BBDC)

plot(cbind( log(BBDC[,1]), log(ITAU[,4]) ))

plot(log(ITAU))

cor(BBDC$bbdc4_sa_close,BBDC$bbdc3_sa_close)
cor(ITAU$itub4_sa_close,ITAU$itub3_sa_close)

plot( log(BBDC) )
plot( log(ITUB) )
plot( log(PETR) )



aTSA::coint.test( log(BBDC[,1]) %>% as.vector(), # forçando os objetos a possuírem a estrura correta
                  log(BBDC[,2]) %>% as.vector()
)


### Itaú --------------------------------------------------------------------

plot(log(ITAU[,1:2]))

aTSA::coint.test( log(ITAU$itub4_sa_close) %>% as.vector(), # forçando os objetos a possuírem a estrura correta
                  cbind( log(ITAU$itub3_sa_close)) %>% as.matrix()
)

(itau01 <- lm(log(itub4_sa_close) ~ log(itub3_sa_close), data = ITAU)) %>% 
    summary()
(res <- itau01 %>% 
  residuals()) %>% 
  ts() %>%
  autoplot()

res %>% acf()

res %>% fracdiff::fdGPH(bandw.exp = .7)


X <- cbind(log(ITAU$itub4_sa_close),log(ITAU$itub3_sa_close))

   X %>% LongMemoryTS::FCI_SRFB18(d=1,m=floor(1+nrow(X)^.7),r=3,alpha = .1)

   aTSA::coint.test( log(ITAU$itsa4_sa_close) %>% as.vector(), # forçando os objetos a possuírem a estrura correta
                  cbind( log(ITAU$itsa3_sa_close)) %>% as.matrix()
)

(itau02 <-lm(log(ITAU$itsa4_sa_close) ~ log(ITAU$itsa3_sa_close))) %>% summary()

(res2 <- itau02 %>%
    residuals()) %>%
  autoplot()

res2 %>% acf()

aTSA::coint.test( log(ITAU$itub4_sa_close)  %>% as.vector(), # forçando os objetos a possuírem a estrura correta
                  cbind( log(ITAU$itub3_sa_close), 
                         log(ITAU$itsa3_sa_close), 
                         log(ITAU$itsa4_sa_close) ) %>% as.matrix()
)

(itau03 <-update( itau01, ~ .  + log(itsa3_sa_close)+ log(itsa4_sa_close))) %>% 
  summary()

date(ITAU)

(res3 <- itau03 %>%
    residuals() %>%
    ts()) %>%
  autoplot()

res3 %>% acf()

fracdiff::fdGPH(res3)

new_data = cbind(ITAU,BBDC) %>% window(start = "2011-01-03") 

  
(itau04 <-update( itau03, ~ . + log(bbdc3_sa_close) + log(bbdc4_sa_close), data = new_data))%>% 
  summary()

(res4 <- itau04 %>%
    residuals() %>%
    ts()) %>%
  autoplot()
res4 %>% acf()


### Petrobrás ---------------------------------------------------------------

corte <- "2020-01-01"
PETR_c  <- PETR %>% window(start = corte)

plot(log(PETR_c[,1:2]))
aTSA::coint.test( log(PETR_c[,1]) %>% as.vector(), # forçando os objetos a possuírem a estrura correta
                  log(PETR_c[,2]) %>% as.vector()
)
(petr <-lm(log(PETR_c$petr4_sa_close) ~ log(PETR_c$petr3_sa_close))) %>% summary()

(res <- petr %>%
    residuals()) %>%
  autoplot()

res %>% acf()

