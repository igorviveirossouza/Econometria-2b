
# Modelos de escolha discreta:

# Conjunto de dados affairs: Probabilidade de um caso extraconjugal
# Refer?ncia: 
#  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) The New S Language. Wadsworth & Brooks/Cole.

install.packages("margins") # Calcula os efeitos marginais

require(wooldridge)
require(sjPlot)

View(affairs)
?affairs


plot(affairs$educ,affairs$affair)


#Estat?sitcas descritivas com o comando table: segunda vari?vel aparece nas colunas
table(affairs$affair,affairs$age)
table(affairs$affair,affairs$male)


plot(table(affairs$affair,affairs$male),col=c(0,2),ylab = "affair",xlab = "gender, 1 = male")

legend('bottom',legend = paste("N?o traiu", "J? traiu"), col = c(0,2), pch = 19, bty = "n")


plot(table(affairs$affair,affairs$age))
plot(affairs$affair~affairs$age, ylevels = 1:2)



# Estimar um modelo Probit e Logit: comando glm

# Modelo Baseline: sexo

Probit_01 <- glm(formula = affair ~ male, family = binomial(link = "probit"), data = affairs)
Logit_01  <- glm(formula = affair ~ male, family = binomial(link = "logit"), data = affairs)


summary(Probit_01)
summary(Logit_01)


# Valores preditos:

fitted(Probit_01) ; fitted(Logit_01)

cbind(fitted(Probit_01), fitted(Logit_01))

summary(fitted(Probit_01)) ; summary(fitted(Logit_01))


# se us?ssemos um MQO: 
MQO <- lm(affair ~ male, data = affairs)

summary(MQO)
fitted.values(MQO)


# Porque deram iguais, apesar de serem modelos distintos? 

summary(affairs$affair)


Probit_hat_01 <- dnorm(predict(Probit_01, type = "link")) # Aqui uso a dist. Normal
Logit_hat_01  <- dlogis(predict(Logit_01, type = "link")) # Aqui uso a dist. Log?stica

summary(Probit_hat_01)
summary(Logit_hat_01)

# Efeitos Marginais na m?dia: 

mean(Probit_hat_01)*coef(Probit_01)
mean(Logit_hat_01)*coef(Logit_01)

# Usando o pacote margins
require(margins)
marg_probit <- margins(Probit_01)
print(marg_probit)

marg_logit <- margins(Logit_01)
print(marg_logit)


# Pseudo R?: O que ? um pseudo R??

Probit_00 <- update(Probit_01, formula = . ~ 1) # Regress?o somente contra o intercepto
1 - as.vector(logLik(Probit_01)/logLik(Probit_00))

Logit_00 <- update(Logit_01, formula = . ~ 1) # Regress?o somente contra o intercepto
1 - as.vector(logLik(Logit_01)/logLik(Logit_00))

# Outra forma de calcular o pseudo R? ? usando a deviance
# A deviance ? a 'fun??o desvio', a deviance ? equivanele ? SQE do modelo classico de regress?o
# ao passo que a null.deviance ? equivalente ? SQT

Probit_01$deviance ; Probit_01$null.deviance 
Logit_01$deviance  ; Logit_01$null.deviance 

1 - Probit_01$deviance/Probit_01$null.deviance 
1 - Logit_01$deviance/Logit_01$null.deviance 


#A fun??o desvio ? igual a -2logLiK
-2*logLik(Probit_01)

# Taxa de acerto do modelo: O modelo (By Default prev? que quem recebe prob. predita > 0,5 o evento deve ocorrer) 

table(true = affairs$affair, pred = round(fitted(Probit_01)))
table(true = affairs$affair, pred = round(fitted(Logit_01)))

#Notem que os modelos n?o predisseram nenhum caso

# Cruva ROCR: ENCONTRA O PONTO ?TIMO DE CORTE PARA DECIDIR SE O EVENTO
# vAI OCORRER OU N?O -> AMPLAMENTE USADA NA MEDICINA PARA AVALIAR TESTES

install.packages('ROCR')
library("ROCR")


# "Alternatively, the receiver operating characteristic (ROC) curve can be
#   used: for every cutoff [0, 1], the associated true positive rate (TPR(c)"

summary(fitted(Probit_01))

pred <- prediction(fitted(Probit_01),affairs$affair)
par(mfrow=c(1,2))
plot(performance(pred, "acc")) # O gr?fico de acur?cia respeita o valor m?ximo predito de prob.
plot(performance(pred, "tpr", "fpr")) # ROC
abline(0, 1, lty = 2,col=2)


summary(fitted(Logit_01))

pred2 <- prediction(fitted(Logit_01),affairs$affair)
par(mfrow=c(1,2))
plot(performance(pred2, "acc"))
plot(performance(pred2, "tpr", "fpr"))
abline(0, 1, lty = 2,col=2)




# Vamos incluir mais vari?veis explicativas

Probit_02 <- glm(formula = affair ~ male + age, family = binomial(link = "probit"), data = affairs)
Logit_02 <- glm(formula = affair ~ male + age, family = binomial(link = "logit"), data = affairs)

summary(Probit_02)
summary(Logit_02)


Probit_03 <- glm(formula = affair ~ male + age + yrsmarr, family = binomial(link = "probit"), data = affairs)
Logit_03 <- glm(formula = affair ~ male + age  + yrsmarr, family = binomial(link = "logit"), data = affairs)

summary(Probit_03)
summary(Logit_03)


Probit_04 <- glm(formula = affair ~ male + age  + yrsmarr + kids, family = binomial(link = "probit"), data = affairs)
Logit_04  <- glm(formula = affair ~ male + age   + yrsmarr + kids, family = binomial(link = "logit"), data = affairs)

summary(Probit_04)
summary(Logit_04)


Probit_05 <- glm(formula = affair ~ male*kids + age + yrsmarr, family = binomial(link = "probit"), data = affairs)
Logit_05  <- glm(formula = affair ~ male*kids + age + yrsmarr, family = binomial(link = "logit"), data = affairs)

summary(Probit_05)
summary(Logit_05)


# Incluir a vari?vel religi?o, categ?rica, como v?rias dummies

class(affairs$relig) # Var??vel est? como num?rica!

Probit_06 <- glm(formula = affair ~ male*kids + male + kids + age + yrsmarr + as.factor(relig), family = binomial(link = "probit"), data = affairs)
Logit_06  <- glm(formula = affair ~ male*kids + male + kids + age + yrsmarr + as.factor(relig), family = binomial(link = "logit"), data = affairs)

summary(Probit_06)
summary(Logit_06)

coeftest(Probit_06, vcov = sandwich)
coeftest(Logit_06, vcov = sandwich)

# Pseudo R?
1 - Probit_06$deviance/Probit_06$null.deviance
1 - Logit_06$deviance/Logit_06$null.deviance 

# Taxa de acerto do modelo: O modelo (By Default prev? que quem recebe prob. predita > 0,5 o evento deve ocorrer) 

table(true = affairs$affair, pred = round(fitted(Probit_06)))
table(true = affairs$affair, pred = round(fitted(Logit_06)))

summary(fitted(Probit_06))

pred <- prediction(fitted(Probit_06),affairs$affair)
par(mfrow=c(1,2))
plot(performance(pred, "acc"))
plot(performance(pred, "tpr", "fpr"))
abline(0, 1, lty = 2,col=2)


summary(fitted(Logit_06))

pred2 <- prediction(fitted(Logit_06),affairs$affair)
par(mfrow=c(1,2))
plot(performance(pred2, "acc"))
plot(performance(pred2, "tpr", "fpr"))
abline(0, 1, lty = 2,col=2)

probabilidades_preditas <- predict(Probit_06, type = "response")

# Converter as probabilidades preditas em classes (0 ou 1) usando um ponto de corte de 0.5
classes_preditas <- ifelse(probabilidades_preditas >= 0.5, 1, 0)

# Calcular a acurácia
acuracia <- sum(classes_preditas == affairs$affair) / length(affairs$affair)
print(paste("Acurácia:", acuracia))

# Calcular a curva ROC e a AUC-ROC
library(pROC)
curva_roc <- roc(affairs$affair, probabilidades_preditas)
auc_roc <- auc(curva_roc)
print(paste("AUC-ROC:", auc_roc))

plot(curva_roc)

# Efeitos Marginais:

Probit_hat_06 <- dnorm(predict(Probit_06, type = "link")) # Aqui uso a dist. Normal
Logit_hat_06  <- dlogis(predict(Logit_06, type = "link")) # Aqui uso a dist. Log?stica


Probit_06|>margins()
Logit_06|>margins()

mean(Probit_hat_06)*coef(Probit_06)
mean(Logit_hat_06)*coef(Logit_06)

## Comparar os modelos Probit e Logit usando o teste de Wald ----

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

MQO_06  <- lm(formula = affair ~ male*kids + male + kids + age + yrsmarr 
              + as.factor(relig), data = affairs)

MQO_06|>predict()|>summary()


tab_model(Probit_06,Logit_06,MQO_06,
          dv.labels = c("Probit","Logit","MQO"),
          show.ci = F,
          transform = NULL
                    )

# Aqui temos as odds:
tab_model(Probit_06,Logit_06,
          show.ci = F)


# Modelos com idade ao quadrado e classificador de felicidade ----

Probit_07 <- glm(formula = affair ~ male*kids + male + kids + age + I(age^2) + yrsmarr + as.factor(relig), family = binomial(link = "probit"), data = affairs)
Logit_07  <- glm(formula = affair ~ male*kids + male + kids + age + I(age^2) + yrsmarr + as.factor(relig), family = binomial(link = "logit"), data = affairs)

summary(Probit_07)
summary(Logit_07)

# Pseudo R?
1 - Probit_07$deviance/Probit_07$null.deviance
1 - Logit_07$deviance/Logit_07$null.deviance 



Probit_07a <- glm(formula = affair ~ male*kids + male + kids + age  + yrsmarr + as.factor(ratemarr) + as.factor(relig) + educ, family = binomial(link = "probit"), data = affairs)
Logit_07a  <- glm(formula = affair ~ male*kids + male + kids + age  + yrsmarr + as.factor(ratemarr) + as.factor(relig) + educ, family = binomial(link = "logit"), data = affairs)

summary(Probit_07)
summary(Logit_07)

# Pseudo R?
1 - Probit_07$deviance/Probit_07$null.deviance
1 - Logit_07$deviance/Logit_07$null.deviance 


# Modelo aninhado e Teste de Raz?o de Verossimilhan?a:

Probit_07b <- glm(formula = affair ~  age  + yrsmarr + as.factor(ratemarr) + as.factor(relig) + educ, family = binomial(link = "probit"), data = affairs)
summary(Probit_07b)

#  Modelo Irrestrito  - Modelo Restrito     
testEst <-  2*as.numeric(  logLik(Probit_07a) - logLik(Probit_07b))

p.val   <- pchisq(testEst, df = 3, lower.tail = FALSE)


# O comando abaixo ? equivalente:

lrtest(Probit_07b, Probit_07a)


# Performance: 
pred3 <- prediction(fitted(Logit_07),affairs$affair)
par(mfrow=c(1,2))
plot(performance(pred2, "acc"))
plot(performance(pred2, "tpr", "fpr"))
par(new=T)
plot(performance(pred3, "tpr", "fpr"),col=4)
abline(0, 1, lty = 2,col=2)

table(true = affairs$affair, pred = round(fitted(Probit_07)))



# Usando matrizes robustas de heterocedastidade em Modelos de escolha discreta

library(AER)
coeftest(Probit_07, vcov = sandwich)
coeftest(Logit_07, vcov = sandwich)