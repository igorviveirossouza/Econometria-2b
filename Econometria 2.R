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

# Pacote para Séries Temporais multivariadas:
if(!require(MTS)){install.packages("MTS")&require(MTS);require(MTS)}
if(!require(vars)){install.packages("vars")&require(vars);require(vars)}

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

# Pacotes com exmeplo de conjuntos de dados
if(!require(fpp2)){install.packages("fpp2")&require(fpp2);require(fpp2)}

#Séries financeiras:
if(!require(quantmod)){install.packages("quantmod")&require(quantmod);require(quantmod)}

par.ori <- par(no.readonly = T)


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
  acf(residuals(consump1),col='darkred'); acf(residuals(consump2),col='darkblue')
  
  # Teste de seleção de modelos não aninhados: Encompassing Test
  
  encomptest(consump1,consump2)
  
  # Estimando o modelo abrangente (encompassing model)
  
  cons_lmE <- dynlm(consumption ~ dpi + L(dpi) + L(consumption), data = USMacroG)
  tab_model(consump1,consump2,cons_lmE, title = "Modelo para o consumo",
            show.ci = F, dv.labels = c("modelo 1",'modelo 2'), digits = 5)
  
  NEWDATA2 <- merge(as.zoo(USMacroG[,"consumption"]), fitted(consump1),
                   fitted(consump2),fitted(cons_lmE), 0, residuals(consump1), 
                   residuals(consump2),residuals(cons_lmE))
  
  plot(NEWDATA2, screens = rep(1:2, c(4, 4)), col = rep(c(1, 'darkred', 
      'darkblue','orange'), 2), xlab = "Time", 
      ylab = c("Fitted values", "Residuals"), lwd=c(4,2,3,1,1,1), main = "")
  
  par(mfrow=c(1,3))
  acf(residuals(consump1),col='darkred',lwd=3) 
  acf(residuals(consump2),col='darkblue',lwd=3)
  acf(residuals(cons_lmE),col='darkorange',lwd=3)
  
  # Testes de autocorrelação
  
  bgtest(consump1,order=1); bgtest(consump2,order=1); bgtest(cons_lmE,order=1)
  
  dwtest(consump1);  dwtest(consump2);  dwtest(cons_lmE)
  
  Box.test(residuals(consump1), type = "Ljung-Box")
  Box.test(residuals(consump2), type = "Ljung-Box")
  Box.test(residuals(cons_lmE), type = "Ljung-Box")
  
  
  
  
  rbind(SE = sqrt(diag(vcov(consump2))),
        QS = sqrt(diag(kernHAC(consump2))),
        NW = sqrt(diag(NeweyWest(consump2))))

  coeftest(consump2,vcov=NeweyWest(consump2))
## Inflação vs câmbio ------------------------------------------------------
  SEARCH <- BETSsearch(periodicity = "M", view = FALSE)
    
  IPCA_atual   <- BETSget(code=c(433),  from = "2000-01-01")
  Cambio_atual <- BETSget(code=c(3695), from = "2000-01-01")
  
  
  INFLA_BR <- cbind(IPCA_atual,Cambio_atual[,2])%>%`colnames<-`(c('ipca','cambio'))
  
  plot(INFLA_BR)
  
  #Comparando ACF's:
  par(mfrow=c(1,2));acf(INFLA_BR[,1]); acf(INFLA_BR[,2])
  
  # Criando a série de variação cambial:
  INFLA_BR <- (cbind(INFLA_BR, c(NA,diff(INFLA_BR[,2]))))%>%
    `colnames<-`(c('ipca','cambio','d.cambio'))
  
  plot(INFLA_BR)
  
  acf( na.remove(INFLA_BR) )
  
  # Inflação vs câmbio: sem relação? resultado incoerente?
  dynlm(ipca~diff(cambio),data=INFLA_BR)%>%summary()
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
  
  plot(MA_IPCA,ylim=ylim);par(new=T);plot(MA_USDL,ylim=ylim,col=2)
  
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
  (mod04 <- update(mod03, ~ .  + L(ipca,3)))%>%summary()
  reset(mod04)
  
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
  API.key <- '76489ce521ce14d034d9fc261e24dc30'
  fredr_set_key(API.key)
  
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
  PPP <- IPCA_index/CPI_index
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


# Regressão espúria -------------------------------------------------------


# Download  stockholm stock exchange index data since 2010
getSymbols("^OMX", src = "yahoo", from = "2010-01-01")

# Convert daily data to quarterly data
OMX_quarterly <- to.quarterly(OMX, OHLC = FALSE, drop = TRUE)
OMX_quarterly|>class()
plot(OMX_quarterly)

data <- merge(OMX_quarterly[ , 4, drop = FALSE], PIB_trim_desaz, join = "inner")

par(mfrow=c(1,1))
par(mar = c(5, 4, 4, 6))
plot.ts(data$OMX.Close, main = "OMX vs. GDP", ylab = "OMX.Close", xlab = "")
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
# Test for unit roots in the residuals
adf.test(reg$residuals)
