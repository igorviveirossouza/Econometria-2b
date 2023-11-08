
# Set up ----
if(!require(sjPlot)){install.packages("sjPlot")&require(sjPlot);require(sjPlot)}
if(!require(stargazer)){install.packages("stargazer")&require(stargazer);require(stargazer)}
require(plm)
require(tidyverse)
library(readxl)
library(BETS)
library(sandwich)


CrimesMG <- read.csv2("G:\Meu Drive\UFMG\Curso - ECN\Econometria 2\dados\CrimesMG.csv")

View(CrimesMG)

# Regress?o ----

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



# Novos dados -------------------------------------------------------------

cvcp <- read.csv2("G:/Meu Drive/Pesquisa/dados-imrs/crimes-violentos-contra-pessoa.csv"
                  ,header=FALSE, comment.char="#")
comarca <- read.table("G:/Meu Drive/Pesquisa/dados-imrs/comarca.csv"
                      ,sep=";", quote="\"")
existencia.de.presidio <- read.table("G:/Meu Drive/Pesquisa/dados-imrs/existencia-de-presidio.csv"
                                     , sep=";", quote="\"")
guarda.municipal   <- read.table("G:/Meu Drive/Pesquisa/dados-imrs/existencia-guarda-municipal.csv"
                                          , sep=";", quote="\"")
gasto.pc.seguranca <- read.csv2("G:/Meu Drive/Pesquisa/dados-imrs/gasto-pc-seguranca.csv"
                                , header=FALSE, comment.char="#")
habitantes.por.pc  <- read.csv2("G:/Meu Drive/Pesquisa/dados-imrs/habitantes-por-pc.csv"
                                , header=FALSE, comment.char="#")
habitantes.por.pm  <- read.csv2("G:/Meu Drive/Pesquisa/dados-imrs/habitantes-por-pm.csv"
                               , header=FALSE, comment.char="#")
tx.matriculados.ef.15a.ou.mais <- read.csv2("G:/Meu Drive/Pesquisa/dados-imrs/tx-matriculados-ef-15a-ou-mais.csv"
                                            , header=FALSE, comment.char="#")
tx.matriculados.em.18a.ou.mais <- read.csv2("G:/Meu Drive/Pesquisa/dados-imrs/tx-matriculados-em-18a-ou-mais.csv"
                                            , header=FALSE, comment.char="#")

cvcp <- (cvcp[-1,] %>% `colnames<-`( cvcp[1,] )) %>%
  pivot_longer(cols=c(3:ncol(cvcp)), names_to = "ano", values_to = "cvcp" ) %>%
  janitor:: clean_names()

comarca <- (comarca[-1,] %>% `colnames<-`( comarca[1,] )) %>%
  pivot_longer(cols=c(3:ncol(comarca)), names_to = "ano", values_to = "possui_comarca" ) %>%
  janitor:: clean_names()

existencia.de.presidio <- (existencia.de.presidio[-1,] %>% `colnames<-`( existencia.de.presidio[1,] )) %>%
  pivot_longer(cols=c(3:ncol(existencia.de.presidio)), names_to = "ano", values_to = "possui_presidio" ) %>%
  janitor:: clean_names()

guarda.municipal <- (guarda.municipal[-1,] %>% `colnames<-`( guarda.municipal[1,] )) %>%
  pivot_longer(cols=c(3:ncol(guarda.municipal)), names_to = "ano", values_to = "guarda_munic" ) %>%
  janitor:: clean_names()

habitantes.por.pc <- (habitantes.por.pc[-1,] %>% `colnames<-`( habitantes.por.pc[1,] )) %>%
  pivot_longer(cols=c(3:ncol(habitantes.por.pc)), names_to = "ano", values_to = "habitantes.por.pc" ) %>%
  janitor:: clean_names()

habitantes.por.pm <- (habitantes.por.pm[-1,] %>% `colnames<-`( habitantes.por.pm[1,] )) %>%
  pivot_longer(cols=c(3:ncol(habitantes.por.pm)), names_to = "ano", values_to = "habitantes.por.pm" ) %>%
  janitor:: clean_names()

tx.matriculados.ef.15a.ou.mais <- (tx.matriculados.ef.15a.ou.mais[-1,] %>% `colnames<-`( tx.matriculados.ef.15a.ou.mais[1,] )) %>%
  pivot_longer(cols=c(3:ncol(tx.matriculados.ef.15a.ou.mais)), names_to = "ano", values_to = "tx.mat.ef.15oumais" ) %>%
  janitor:: clean_names()

tx.matriculados.em.18a.ou.mais <- (tx.matriculados.em.18a.ou.mais[-1,] %>% `colnames<-`( tx.matriculados.em.18a.ou.mais[1,] )) %>%
  pivot_longer(cols=c(3:ncol(tx.matriculados.em.18a.ou.mais)), names_to = "ano", values_to = "tx.mat.em.18oumais" ) %>%
  janitor:: clean_names()

pib_2002_2009 <- readxl::read_excel(
  "G:/Meu Drive/Pesquisa/dados-imrs/PIB dos Municípios - base de dados 2002-2009.xls") %>%
  select( c("Ano","SIGLA_UF","COD_MUN","PIB_pc_preços_correntes")) 

pib_2010_2020 <- readxl::read_excel(
  "G:/Meu Drive/Pesquisa/dados-imrs/PIB dos Municípios - base de dados 2010-2020.xls") %>%
  select( c("Ano","SIGLA_UF","COD_MUN","PIB_pc_preços_correntes"))

pib_2002_2009 <- pib_2002_2009 %>%
  mutate(COD_MUN = COD_MUN %>% as.character()) %>%
  filter(pib_2002_2009$COD_MUN %in% (cvcp$ibge7%>%unique()) )

pib_2010_2020 <- pib_2010_2020 %>%
  mutate(COD_MUN = COD_MUN %>% as.character()) %>%
  filter(pib_2010_2020$COD_MUN %in% (cvcp$ibge7%>%unique()) )

PIB <- rbind(pib_2002_2009,pib_2010_2020) %>%
  dplyr::rename(ibge7 = COD_MUN)

## deflaciona a series -----------------------------------------------------

Deflator <- BETSget(code=c(1211),  from = "2000-01-01")

Deflator$deflator <- 100
for(i in 1:(nrow(Deflator)-1) ){
  Deflator$deflator[(i+1)] <- Deflator$deflator[i]*(1+Deflator$value[i+1]/100)
}
Deflator$deflator <- Deflator$deflator[nrow(Deflator)]/Deflator$deflator

Deflator$Ano <- lubridate::year(Deflator$date)

PIB <- left_join(PIB,Deflator,by="Ano")

PIB$pib_pcreal <- PIB$PIB_pc_preços_correntes*PIB$deflator

PIB <- select(PIB ,c("Ano","ibge7","pib_pcreal")) %>%
  mutate(Ano = as.character(Ano))

cvcp <- cvcp %>% 
  dplyr::left_join(comarca, by = c("ibge7","ano") ) %>% select(-ends_with(c(".y",".x"))) %>%
  dplyr::left_join(existencia.de.presidio, by = c("ibge7","ano") ) %>% select(-ends_with(c(".y",".x"))) %>%
  dplyr::left_join(guarda.municipal, by = c("ibge7","ano") ) %>% select(-ends_with(c(".y",".x"))) %>%
  dplyr::left_join(habitantes.por.pc, by = c("ibge7","ano") ) %>% select(-ends_with(c(".y",".x"))) %>%
  dplyr::left_join(habitantes.por.pm, by = c("ibge7","ano") ) %>% select(-ends_with(c(".y",".x"))) %>%
  dplyr::left_join(tx.matriculados.ef.15a.ou.mais, by = c("ibge7","ano") ) %>% select(-ends_with(c(".y",".x"))) %>%
  dplyr::left_join(tx.matriculados.em.18a.ou.mais, by = c("ibge7","ano") ) %>% select(-ends_with(".y")) %>%
  dplyr::rename(municipio = munic_pio.x) %>%
  relocate(municipio, .before = ano)

cvcp <- left_join(cvcp,PIB,by= c(c("ano" =  "Ano"),"ibge7"))

cvcp <- cvcp %>% 
  mutate( pc_percapta = 100000/habitantes_por_pc
         ,pm_percapta = 100000/habitantes_por_pm
         ,ln_tx_matef = log(tx_mat_ef_15oumais)
         ,ln_tx_matem = log(tx_mat_em_18oumais)
         ,ln_pm_percapta = log(pm_percapta)
         ,possui_presidio = as.factor(case_when(
            possui_presidio == "N\xe3o" ~ "Não"
           ,possui_presidio == "Sim" ~ "Sim"
           ,TRUE ~ NA))
         ,possui_comarca = as.factor(case_when(
            possui_comarca == "N\xe3o" ~ "Não"
           ,possui_comarca == "Sim" ~ "Sim"
           ,TRUE ~ NA))
  ) 


filtra.inf <- which(!is.infinite(
  rowSums(cvcp %>%select(where(is.numeric)),na.rm = T)))


PAINEL <- (pdata.frame(cvcp[filtra.inf,], index = c("ibge7","ano")))
writexl::write_xlsx(PAINEL,"./CrimesMG.xlsx")

## modelos -----------------------------------------------------------------

  tab_model(
     (M0 <- plm(cvcp ~ ln_pm_percapta ,data = PAINEL, model='pooling'))
    ,(M1 <- plm(cvcp ~ ln_pm_percapta ,data = PAINEL, model='within')) 
    ,(M2 <- plm(cvcp ~ ln_pm_percapta ,data = PAINEL, model='within',effect = "twoways"))
    ,dv.labels = c("pooling","within","twoways")
    ,show.ci = F
    ,robust = T)
  
  espec <- formula("cvcp ~ ln_pm_percapta + 
                   ln_tx_matef  +
                   ln_tx_matem"
           ) 
  
  tab_model(
     (M0 <- plm(espec, data = PAINEL, model='pooling'))
    ,(M1 <- plm(espec, data = PAINEL, model='within')) 
    ,(M2 <- plm(espec, data = PAINEL, model='within', effect = "twoways"))
    ,dv.labels = c("pooling","within","twoways")
    ,show.ci = F
    ,robust = T)
  
  
  espec <- update(espec, ~ . + possui_presidio + possui_comarca)
  
  tab_model(
     (M0 <- plm(espec, data = PAINEL, model='pooling'))
    ,(M1 <- plm(espec, data = PAINEL, model='within'))
    ,(M2 <- plm(espec, data = PAINEL, model='within', effect = "twoways"))
    ,(M3 <- plm(espec, data = PAINEL, model='random'))
    ,dv.labels = c("pooling","within","twoways","random")
    ,show.ci = F)
  
  pFtest(M1,M0)
  pFtest(M2,M1)
  phtest(M1,M3)
  
  
  espec <- update(espec, ~ . + log(pib_pcreal) )
  
  tab_model(
     (M0 <- plm(espec, data = PAINEL, model='pooling'))
    ,(M1 <- plm(espec, data = PAINEL, model='within')) 
    ,(M2 <- plm(espec, data = PAINEL, model='within', effect = "twoways"))
    ,(M3 <- plm(espec, data = PAINEL, model='random'))
    ,dv.labels = c("pooling","within","twoways","random")
    ,show.ci = F)
  
  pFtest(M1,M0)
  pFtest(M2,M1)
  phtest(M1,M3)
  
  
  
  # modelo tobit -----------------------------------------------------------------
  
  tobit.panel  <- censReg::censReg(espec, data = PAINEL)
  summary(tobit.panel)
  
  # Pseudo R²
  tobit.null   <- update(tobit.panel,~ .1)
  
  1 - as.vector(logLik(tobit.panel)/logLik(tobit.null))
  
  # Variância robusta: tem que usar exatamente os mesmos dados que o modelo:
  lmtest::coeftest(tobit.panel, vcov.=vcovCL(tobit.panel, cluster = (PAINEL$ibge7), type="HC0"))
  
  
  tab_model(tobit.panel,M1,M2,M3
           ,dv.labels = c("tobit","within","twoways","random")
           ,show.ci = F
           #,show.dev = T
           ,show.aic = T
           ,show.r2 = T
            )
  
  
  tobit.pool <- censReg::censReg(espec, data = PAINEL%>%as.data.frame())
  summary(tobit.pool)

espec <- update(espec, ~ . -1 + as.factor(ibge7))
tobit.pool <- censReg::censReg(espec, data = PAINEL%>%as.data.frame())


tobit.dummy %>% summary()

# filtra.ano = c(2002,2004,2006,2008,2010,2012,2014,2016,2018,2020)
# tab_model(
#   (M0 <- plm(espec, data = PAINEL %>% filter(ano %in% filtra.ano), model='pooling'))
#  ,(M1 <- plm(espec, data = PAINEL %>% filter(ano %in% filtra.ano), model='within')) 
#  ,(M2 <- plm(espec, data = PAINEL %>% filter(ano %in% filtra.ano), model='within', effect = "twoways"))
#  ,(M3 <- plm(espec, data = PAINEL %>% filter(ano %in% filtra.ano), model='random'))
#  ,dv.labels = c("pooling","within","twoways","random")
#  ,show.ci = F)
 