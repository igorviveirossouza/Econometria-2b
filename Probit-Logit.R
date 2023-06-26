rm(list=ls())
# Setup -------------------------------------------------------------------


# Modelos de escolha discreta:

# Conjunto de dados affairs: Probabilidade de um caso extraconjugal
# Refer?ncia: 
#  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole.

# install.packages("margins") # Calcula os efeitos marginais
# install.packages('ROCR') # Calcula a curva ROC
require(pROC)
require(wooldridge)
require(sjPlot)
require(margins)
require(AER)

View(affairs)
?affairs

plot(affairs$educ,affairs$affair)


#Estat?sitcas descritivas com o comando table: segunda vari?vel aparece nas colunas
table(affairs$affair,affairs$age)
table(affairs$affair,affairs$male)


plot(table(affairs$affair,affairs$male),col=c(0,2),ylab = "affair",
     xlab = "gender, 1 = male")

legend('bottom',legend = paste("N?o traiu", "J? traiu"), col = c(0,2),
       pch = 19, bty = "n")


plot(table(affairs$affair,affairs$age))
plot(affairs$affair~affairs$age, ylevels = 1:2)



# Estimar um modelo Probit e Logit: comando glm --------------------------------

# Modelo Baseline: sexo

Probit_01 <- glm(formula = affair ~ male, family = binomial(link = "probit"),
                 data = affairs)
Logit_01  <- glm(formula = affair ~ male, family = binomial(link = "logit"), 
                 data = affairs)

summary(Probit_01)
summary(Logit_01)

# Valores preditos:

fitted(Probit_01) ; fitted(Logit_01)

# Usando o pacote margins
marg_probit <- margins(Probit_01)
print(marg_probit)

marg_logit <- margins(Logit_01)
print(marg_logit)


# Pseudo R²: O que é um pseudo R²: medida de qualidade de ajuste para um GLM----

# Regressão somente contra o intercepto:
Probit_00 <- update(Probit_01, formula = . ~ 1) 
1 - as.vector(logLik(Probit_01)/logLik(Probit_00))

# Regressão somente contra o intercepto:
Logit_00 <- update(Logit_01, formula = . ~ 1) 
1 - as.vector(logLik(Logit_01)/logLik(Logit_00))

# Outra forma de calcular o pseudo R² é usando a deviance
# A deviance é a 'função desvio', é equivanele à SQE do modelo classico de 
# regressão ao passo que a null.deviance é  equivalente a SQT

Probit_01$deviance ; Probit_01$null.deviance 
Logit_01$deviance  ; Logit_01$null.deviance 

1 - Probit_01$deviance/Probit_01$null.deviance 
1 - Logit_01$deviance/Logit_01$null.deviance 

#A deviance é igual a -2logLiK
-2*logLik(Probit_01)

# Taxa de acerto do modelo: O modelo, by default, faz previsão para quem tem 
# prob. predita > 0,5 igual a 1 (o evento deve ocorrer) 

# Matriz de confusão:
table(true = affairs$affair, pred = round(fitted(Probit_01)))
table(true = affairs$affair, pred = round(fitted(Logit_01)))

# Notem que os modelos não predisseram nenhum caso

# Vamos incluir mais variáveis explicativas-------------------------------------

Probit_02 <- glm(formula = affair ~ male + age, 
                 family = binomial(link = "probit"), data = affairs)
Logit_02 <- glm(formula = affair ~ male + age, 
                family = binomial(link = "logit"), data = affairs)

summary(Probit_02)
summary(Logit_02)


Probit_03 <- glm(formula = affair ~ male + age + yrsmarr, 
                 family = binomial(link = "probit"), data = affairs)
Logit_03 <- glm(formula = affair ~ male + age  + yrsmarr, 
                family = binomial(link = "logit"), data = affairs)

summary(Probit_03)
summary(Logit_03)


Probit_04 <- glm(formula = affair ~ male + age  + yrsmarr + kids, 
                 family = binomial(link = "probit"), data = affairs)
Logit_04  <- glm(formula = affair ~ male + age   + yrsmarr + kids, 
                 family = binomial(link = "logit"), data = affairs)

summary(Probit_04)
summary(Logit_04)

Probit_05 <- glm(formula = affair ~ male*kids + age + yrsmarr, 
                 family = binomial(link = "probit"), data = affairs)
Logit_05  <- glm(formula = affair ~ male*kids + age + yrsmarr,
                 family = binomial(link = "logit"), data = affairs)

summary(Probit_05)
summary(Logit_05)


# Incluir a variável religião, categ?rica, como várias dummies

class(affairs$relig) # Variável está como numérica!

Probit_06 <- glm(formula = affair ~ male*kids + male + kids + age + yrsmarr + 
                   as.factor(relig), family = binomial(link = "probit"), data = affairs)
Logit_06  <- glm(formula = affair ~ male*kids + male + kids + age + yrsmarr + 
                   as.factor(relig), family = binomial(link = "logit"), data = affairs)

summary(Probit_06)
summary(Logit_06)

# Usando uma matriz robusta:
coeftest(Probit_06, vcov = sandwich)
coeftest(Logit_06, vcov = sandwich)

# Pseudo R²?
1 - Probit_06$deviance/Probit_06$null.deviance
1 - Logit_06$deviance/Logit_06$null.deviance 

# Taxa de acerto do modelo: 

table(true = affairs$affair, pred = round(fitted(Probit_06)))
table(true = affairs$affair, pred = round(fitted(Logit_06)))

summary(fitted(Probit_06))

# Efeitos Marginais:
Probit_06|>margins()
Logit_06|>margins()


# Curva ROC ---------------------------------------------------------------


# Curva ROCR: ENCONTRA O PONTO ÓTIMO DE CORTE PARA DECIDIR SE O EVENTO
# VAI OCORRER OU NÃO -> AMPLAMENTE USADA NA MEDICINA PARA AVALIAR TESTES

# "Alternatively, the receiver operating characteristic (ROC) curve can be
#   used: for every cutoff [0, 1], the associated true positive rate (TPR(c)"

#Cálculo do vetor de observado vs predito:
pred.pro <- pROC::roc(affairs$affair,fitted(Probit_06))
pred.log <- pROC::roc(affairs$affair,fitted(Logit_06))

par(pty = "s")
plot.roc(pred.pro,
     print.auc = TRUE,
     legacy.axes = TRUE
     )
par(new=T)
plot.roc(pred.log,
         print.auc = TRUE,
         legacy.axes = TRUE,
         print.auc.y = .4,
         add = TRUE,
         col=2
)

legend("bottomright", legend=c("Regressão Probit", "Regressão Logit"),
       col= c("black", "red"), lwd=2)

# Reparem como os ambos têm desempenho similar!


## Comparar os modelos Probit e Logit usando o teste de Wald ------------------

# Os resultados do teste de Wald nos fornecerão uma estatística de teste e um 
# valor-p que nos ajudarão a determinar se há diferenças significativas entre os
# modelos Probit e Logit. Se o valor-p for menor que um nível de significância 
# específico (por exemplo, 0,05), podemos rejeitar a hipótese nula de igualdade 
# entre os modelos e concluir que há diferenças significativas. 
# Caso contrário, não teremos evidências suficientes para rejeitar a hipótese 
# nula e podemos considerar os modelos como estatisticamente equivalentes.


wald_test <- anova(Logit_06,Probit_06, test = "Chisq")
# Imprimir os resultados do teste de Wald
print(wald_test)


# Uso do MQO --------------------------------------------------------------

MQO_06  <- lm(formula = affair ~ male*kids + male + kids + age + yrsmarr 
              + as.factor(relig), data = affairs)
# MQO produzindo predições fora do intervalo (0,1):
MQO_06|>predict()|>summary()


# Comparando os três modelos 
tab_model(Probit_06,Logit_06,MQO_06,
          dv.labels = c("Probit","Logit","MQO"),
          show.ci = F,
          transform = NULL
                    )

# Exibindo em odds-ratios-------------------------------------------------------
tab_model(Probit_06,Logit_06,
          show.ci = F)


# Modelos com idade ao quadrado e classificador de felicidade ------------------

Probit_07 <- glm(formula = affair ~ male*kids + male + kids + age +
                 I(age^2) + yrsmarr + as.factor(relig), 
                 family = binomial(link = "probit"), data = affairs)
Logit_07  <- glm(formula = affair ~ male*kids + male + kids + age +
                 I(age^2) + yrsmarr + as.factor(relig),
                 family = binomial(link = "logit"), data = affairs)

summary(Probit_07)
summary(Logit_07)

# Pseudo R² (McFadden)
1 - Probit_07$deviance/Probit_07$null.deviance
1 - Logit_07$deviance/Logit_07$null.deviance 

Probit_07a <- glm(formula = affair ~ male*kids + male + kids + age  + 
                    yrsmarr + as.factor(ratemarr) + as.factor(relig) + educ, family = binomial(link = "probit"), data = affairs)
Logit_07a  <- glm(formula = affair ~ male*kids + male + kids + age  + 
                    yrsmarr + as.factor(ratemarr) + as.factor(relig) + educ, family = binomial(link = "logit"), data = affairs)

summary(Probit_07a)
summary(Logit_07a)

# Pseudo R2
1 - Probit_07a$deviance/Probit_07a$null.deviance
1 - Logit_07a$deviance/Logit_07a$null.deviance 


# Modelo aninhado e Teste de Razão de Verossimilhan?a:

Probit_07b <- glm(formula = affair ~  age  + yrsmarr + as.factor(ratemarr) +
                    as.factor(relig) + educ, family = binomial(link = "probit"),
                  data = affairs)
summary(Probit_07b)

#  Modelo Irrestrito  - Modelo Restrito     
testEst <-  2*as.numeric(  logLik(Probit_07a) - logLik(Probit_07b))

p.val   <- pchisq(testEst, df = 3, lower.tail = FALSE)
print(p.val)

# O comando abaixo é equivalente:

lrtest(Probit_07b, Probit_07a)

# Curva ROC:

pred.pro <- pROC::roc(affairs$affair,fitted(Probit_07a))
pred.log <- pROC::roc(affairs$affair,fitted(Logit_07a))

par(pty = "s")
plot.roc(pred.pro,
         print.auc = TRUE,
         legacy.axes = TRUE
)
par(new=T)
plot.roc(pred.log,
         print.auc = TRUE,
         legacy.axes = TRUE,
         print.auc.y = .4,
         add = TRUE,
         col=2
)

legend("bottomright", legend=c("Regressão Probit", "Regressão Logit"),
       col= c("black", "red"), lwd=2)


# Performance: 
table(true = affairs$affair, pred = round(fitted(Probit_07a)))
table(true = affairs$affair, pred = round(fitted(Logit_07a)))


# Usando matrizes robustas de heterocedastidade em Modelos de escolha discreta

library(AER)
coeftest(Probit_07a, vcov = sandwich)
coeftest(Logit_07a, vcov = sandwich)
