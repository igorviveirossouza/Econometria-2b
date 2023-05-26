#DADOS EM PAINEL


# Set up ----
if(!require(sjPlot)){install.packages("sjPlot")&require(sjPlot);require(sjPlot)}
if(!require(plm)){install.packages("plm")&require(plm);require(plm)}
if(!require(wooldridge)){install.packages("wooldridge")&require(wooldridge);require(wooldridge)}
if(!require(stargazer)){install.packages("stargazer")&require(stargazer);require(stargazer)}
if(!require(gujarati)){install.packages("gujarati")&require(gujarati);require(gujarati)}
if(!require(tidyverse)){install.packages("tidyverse")&require(tidyverse);require(tidyverse)}

install.packages('devtools')
devtools::install_github('https://github.com/brunoruas2/gujarati')
require(gujarati)
require(AER)


# Demanda por gasolina ----------------------------------------------------

GAS <- read.table("C:\\Users\\igorv\\Google Drive\\UFMG\\Curso - ECN\\Econometria 1\\Scripts e Dados\\Datasets\\Batalgi\\Gasoline.txt",header=T)

#GAS <- read.table("U:\\Econometria 2\\GASOLINE.DAT",header=T)

names(GAS) <- c("CO","YR","LN.Gas.Car","LN.Y.N","LN.Pmg.Pgdp","LN.Car.N") # TROCANDO OS NOMES DAS VAR
data("OECDGas")

class(GAS)

View(GAS[which(GAS$YR==1978),])


GAS_PANEL <- pdata.frame(GAS, index = c("CO", "YR")) 

class(GAS_PANEL)
names(GAS_PANEL) <- c("CO","YR","LN.Gas.Car","LN.Y.N","LN.Pmg.Pgdp","LN.Car.N") # TROCANDO OS NOMES DAS VAR

# Pooled Cross-Section:

GAS.POOL <- lm(LN.Gas.Car ~LN.Y.N + LN.Pmg.Pgdp + LN.Car.N , data = GAS) 
tab_model(GAS.POOL,
          dv.labels = c("Pooled"),
          show.ci = F)

GAS.POOL2 <- plm(LN.Gas.Car ~LN.Y.N + LN.Pmg.Pgdp+ LN.Car.N , data = GAS_PANEL,  model = "pooling") 
tab_model(GAS.POOL,
          GAS.POOL2,
          dv.labels = c("Pooled","Pooled (plm)"),
          show.ci = F)

#PRIMEIRA DIFERENÇA

GAS.PD <- plm(LN.Gas.Car ~LN.Y.N + LN.Pmg.Pgdp+ LN.Car.N , data = GAS_PANEL,  model = "fd") 


tab_model(GAS.POOL2,
          GAS.PD,
          dv.labels = c("Pooled","FD"),
          show.ci = F)


#EFEITOS FIXOS

GAS.EF <- plm(LN.Gas.Car ~LN.Y.N + LN.Pmg.Pgdp+ LN.Car.N , data = GAS_PANEL,  model = "within") 

summary(GAS.EF)



#EFEITOS ALEATÓRIOS
GAS.EA <- plm(LN.Gas.Car ~LN.Y.N + LN.Pmg.Pgdp+ LN.Car.N , data = GAS_PANEL,  model = "random") 
summary(GAS.EA)
#POOLED

GAS.PO <- plm(LN.Gas.Car ~LN.Y.N + LN.Pmg.Pgdp+ LN.Car.N , data = GAS_PANEL,  model = "pooling") 
summary(GAS.PO)



tab_model(GAS.POOL2,
          GAS.EA,
          GAS.PD,
          GAS.EF,
          dv.labels = c("Pooled","EA","FD","EF"),
          show.ci = F)

#MQO COM DUMMIES vs EFEITOS FIXOS

GAS.DUM <- plm(LN.Gas.Car ~LN.Y.N + LN.Pmg.Pgdp+ LN.Car.N + as.factor(CO) , data = GAS_PANEL,  model = "pooling") 


summary(GAS.DUM)

# O comando as.factor(X) diz ao R para interpretar a variável X como categórica.
  #comparação



tab_model(GAS.DUM,
          GAS.EF,
          dv.labels = c("LSDV","EF"),
          show.ci = F)

  summary(GAS.EF);summary(GAS.DUM)



  # Podemos admitir que o erro é composto pelos dois efeitos fixos: o de tempo e o cross-sectional. 
  # Basta estimar o EF e cololcar Dummies de tempo
  # Erro composto u_it + alpha_t. Idéia por trás: choques comuns a todos os agentes econômicos. Choque macro.
  
  


  GAS.DUMT <- plm(LN.Gas.Car ~LN.Y.N + LN.Pmg.Pgdp+ LN.Car.N + as.factor(YR) , data = GAS_PANEL,  model = "within") 
  summary(GAS.DUMT)
  
  tab_model(GAS.DUMT,
            GAS.EF,
            dv.labels = c("WITH+DUMT","EF"),
            show.ci = F)

  
  GAS.WITHIN.TW <- plm(LN.Gas.Car ~LN.Y.N + LN.Pmg.Pgdp+ LN.Car.N , data = GAS_PANEL,  model = "within", effect = "twoways") 
  
  tab_model(GAS.DUMT,
            GAS.WITHIN.TW,
            dv.labels = c("WITH+DUMT","EF_toways"),
            show.ci = F)
  
  
  summary(GAS.EF);summary(GAS.DUMT)
  
  
  
#Deve-se testar, a presença de efeitos não observados vs pooled, diretamente à partir do EF:
  pFtest(GAS.EF,GAS.PO)


  
# TESTE DE HAUSSMAN
  
phtest(GAS.EF,GAS.EA)  
      # P-value baixo -> Região de rejeição da Nula e evidência à favor do EF

# Obviamente  temos indicação da presença de efeitos específicos e correlacionados com X_it
# Se tivéssemos optado por um EA estaríamos assumindo que COV(X_it,Alpha_i) = 0.
# Neste caso, poderíamos testar se Sigma_alpha = 0. O teste de um pooled vs EA no qual aceitar H_0 implicaria em aceitar o Pooled



tab_model(GAS.EF,
          GAS.WITHIN.TW,
          dv.labels = c("EF","EF_toways"),
          show.ci = F)


pFtest(GAS.WITHIN.TW,GAS.EF)


# Dados dos ovos ----------------------------------------------------------
data('Table1_1')


# Preparando dos dados:
Table1_1$Y1 <- as.numeric(as.character(Table1_1$Y1))
Table1_1$Y2 <- as.numeric(as.character(Table1_1$Y2))
Table1_1$X1 <- as.numeric(as.character(Table1_1$X1))
Table1_1$X2 <- as.numeric(as.character(Table1_1$X2))

OVOS <- rbind((Table1_1%>%select(,STATE,Y1,X1)%>%
                   `colnames<-`(c("STATE","EGGS","PRICE"))%>%mutate(,YEAR=1991)),
                (Table1_1%>%select(,STATE,Y2,X2)%>%
                   `colnames<-`(c("STATE","EGGS","PRICE"))%>%mutate(,YEAR=1992)))%>%
  pdata.frame(,index = c("STATE","YEAR"))


OVOSef <- plm((EGGS) ~ (PRICE), data = OVOS,  model = "within",effect = 'twoways') 
OVOSea <- plm((EGGS) ~ (PRICE), data = OVOS,  model = "random") 
OVOSpo <- plm((EGGS) ~ (PRICE), data = OVOS,  model = "pooling")


tab_model(OVOSpo,OVOSef,OVOSea,
          dv.labels = c('pooled',"efeito_fixo","efeito_aleatorio"),
          digits = 4,
          show.ci = F
)

