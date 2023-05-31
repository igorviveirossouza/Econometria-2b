rm(list=ls())
require(quantmod)
require(forecast)
require(fracdiff)
require(tidyverse)
require(aTSA)
require(sjPlot)
require(dynlm)

source("UR_test_function.R")

getSymbols(c("ITUB4.SA","ITUB3.SA"))

ITUB4.SA <- ITUB4.SA|>janitor::clean_names()
ITUB3.SA <- ITUB3.SA|>janitor::clean_names()


getSymbols(c("BBDC4.SA","BBDC3.SA"),from="2011-01-02")


BBDC4.SA <- BBDC4.SA|>janitor::clean_names()
BBDC3.SA <- BBDC3.SA|>janitor::clean_names()


# Pega os preços de fechamento --------------------------------------------

BBDC <- cbind(BBDC4.SA$bbdc4_sa_close,BBDC3.SA$bbdc3_sa_close)

ITUB <- cbind(ITUB4.SA$itub4_sa_close,ITUB3.SA$itub3_sa_close)



# ITUB --------------------------------------------------------------------

test_unit_root(log(ITUB[,1]))
test_unit_root(log(ITUB[,2]))

autoplot(log(ITUB))

x <- log(ITUB[,2])|>as.vector()
y <- log(ITUB[,1])|>as.vector()

coint.test(y,x)


tab_model(
  (itub1 <-  lm(y~x)),
  (itub2 <-  dynlm(y~x+trend(y))),
  show.ci=F,
  digits = 6
)

ur.df(itub1$residuals,type="trend")|>summary()

par(mfrow=c(2,1))
plot(itub$residuals|>as.ts())
abline(0,0,col=2)
plot(itub2$residuals|>as.ts(),col='darkblue')
abline(0,0,col='darkgray')



ITUB <- (ITUB|>as.data.frame())|>
  mutate(Data = ITUB|>as.data.frame()|>row.names())

writexl::write_xlsx(ITUB,"./ITUB.xlsx")


# Bradesco ----------------------------------------------------------------

autoplot((BBDC))


C1 <- lm((ITUB[,1])~(ITUB[,2]))

C1$residuals|>autoplot()
C1$residuals|>acf()
C1$residuals|>fdGPH(bandw.exp = .9)


BBDC |>autoplot()

test_unit_root((BBDC[,1]))
test_unit_root((BBDC[,2]))


test_unit_root(log(BBDC[,1]))
test_unit_root(log(BBDC[,2]))

C2 <- lm(log(BBDC[,1])~log(BBDC[,2]))

C2$residuals|>autoplot()
C2$residuals|>acf()

C2$residuals|>test_unit_root()
C2$residuals|>ur.df("none")|>summary()

x <- log(BBDC[,2])|>as.vector()
y <- log(BBDC[,1])|>as.vector()



