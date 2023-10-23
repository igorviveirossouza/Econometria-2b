
# Set up ----
if(!require(sjPlot)){install.packages("sjPlot")&require(sjPlot);require(sjPlot)}
if(!require(stargazer)){install.packages("stargazer")&require(stargazer);require(stargazer)}
require(plm)
require(tidyverse)

CrimesMG <- read.csv2("C:/Users/igorv/Google Drive/UFMG/Curso - ECN/Econometria 2/dados/CrimesMG.csv")

View(CrimesMG)

# Regressão ----

CrimesMG$Efetivo_tx <- Efetivo*100000/Pop


a <- 2015
b <- 2017
attach(CrimesMG)

tab_model(
  dv.labels = "Crimes violentos contra o patrimonio - Taxa",  
  M1 <- lm(CVPatr_tx ~ Efetivo_tx,subset = (a<=Ano & Ano<=b)),
  robust = T,
  show.ci = F,
  digits = 5
)
  

PAINEL <- (pdata.frame(CrimesMG|>filter(a<=Ano & Ano<=b)
                       ,index=c("cod_mun","Ano")))

plot(CVPatr_tx~Efetivo_tx,data=PAINEL)
abline(lm(CVPatr_tx~Efetivo_tx),col=2)
lines(lowess(CVPatr_tx,Efetivo_tx),col=3)


plot(diff(CVPatr_tx)~diff(Efetivo_tx),data=PAINEL)


PAINEL$diff <- diff(PAINEL$CVPatr_tx)

(M2 <- plm(CVPatr_tx ~ Efetivo_tx,data=PAINEL,model='fd'))|>tab_model()


