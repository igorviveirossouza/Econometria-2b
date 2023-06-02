library(dplyr)
{
  set.seed(12345)
  T = 100 # no of periods
  N = 40 # no of subjects

  dat = expand.grid( t = 1:T,i = 1:N)

  # Simulate a common AR(1) time trend
  time.trend = as.numeric(arima.sim(n=T,list(ar = c(0.4,0.5), ma=c(0.6,0.5))))*3+0.7*(1:T)

  dat = mutate(dat,
               group = ifelse(i > N/2,"treat","control"),
               treat = 1L*(group == "treat"),
               exp = 1L*(t > T/2),
               treat_exp = exp*treat,
               mu.t = time.trend[t],
               eps = rnorm(n(),0,15),
               y = mu.t + treat*40 + treat_exp*50 + eps
  )
}



sample_n(dat, 5)

##    t  i group treat exp treat_exp     mu.t        eps         y
## 1 50 31 treat     1   0         0 23.71765  0.2370906  63.95474
## 2  8 21 treat     1   0         0 18.62367 -0.2297932  58.39387
## 3 22 26 treat     1   0         0 19.90087  1.5255634  61.42644
## 4 62 40 treat     1   1         1 33.21133 -0.3764318 122.83490
## 5 37 22 treat     1   0         0 28.60870 -0.7653263  67.84337



# Gráfico  ----------------------------------------------------------------

show.plot = function(dat,label="", show.means=TRUE) {
  library(ggplot2)
  gdat = dat %>%
    group_by(group, t,exp,treat) %>%
    summarize(y = mean(y))

  gg = ggplot(gdat, aes(y=y,x=t, color= group)) +
    geom_line() +
    geom_vline(xintercept=T/2) +
    theme_bw() +
    annotate("text",x=T/4, y = 0.9*max(gdat$y), label=label)

  if (show.means) {
    y.pre.tr <<- mean(filter(gdat,treat==1, exp==0)$y) %>% round(1)
    y.exp.tr <<- mean(filter(gdat,treat==1, exp==1)$y) %>% round(1)
    y.pre.co <<- mean(filter(gdat,treat==0, exp==0)$y) %>% round(1)
    y.exp.co <<- mean(filter(gdat,treat==0, exp==1)$y) %>% round(1)
    gg = gg +
      annotate("label", x=T/4, y=y.pre.tr+15,label=y.pre.tr) +
      annotate("label", x=T/4, y=y.pre.co-15,label=y.pre.co) +
      annotate("label", x=T*0.75, y=y.exp.tr+15,label=y.exp.tr) +
      annotate("label", x=T*0.75, y=y.exp.co-15,label=y.exp.co)
  }
  gg
}
show.plot(dat)



# In our simulated example, the pre-trends are almost perfectly parallel which
# gives substantial confidence that the DID estimator is appropriate. In practice,
# the DID estimator is typically implemented by running a linear regression:

# Variável exp: exposure
library(broom)
tidy(lm(y ~ exp*treat, data=dat))


# Adding a confounder that leads to non-parallel trends -------------------

dat = dat %>% mutate(
  x = ifelse(treat,-t, t)+runif(n())*2,
  y = mu.t + treat*40 + treat_exp*50 + 0.8*x + eps
)
show.plot(dat, show.means=FALSE, label="Pre-trends not parallel")

tidy(lm(y ~ exp*treat, data=dat))

# We now wrongly estimate a negative treatment effect. One advantage of the
# regression approach is that we can add additional control variables.
# So let’s add x as control variable.


tidy(lm(y ~ exp*treat+x, data=dat))


# Reavaliando as tendências paralelas: usando os resíduos: -----------------------------------------

dat$y.org = dat$y
# Faça a regressão completa! Por quê?
# Por que temos que expurgar o efeito de X em Y e também
# as correlações com o período pós tratamento
m0 <- lm(y~x+ exp*treat,data=dat)
dat$y = dat$y - m0$coefficients[2]*dat$x
show.plot(dat,show.means = FALSE)




# Correct way -------------------------------------------------------------

# Correct approach (I hope) for trends plot adjusted for control variables

reg = lm(y~exp*treat+x, data=dat)

# Fictitious data set where control variables
# have a constant value for treatment and control
# group: here just set everywhere 0
dat0 = mutate(dat, x=0)

# Now predict y and add residuals of original regression
dat$y = predict(reg, dat0) + resid(reg)
show.plot(dat,show.means = FALSE, label="parallel pretrends")


# Using the ParallelTrendsPlot package
# I also wrote a small R package to facilitate the generation of the trend plots given control variables. Below is the example:

library(ParallelTrendsPlot)
dat$y=dat$y.org
pt.dat = parallel.trends.data(dat,cvars="x")
parallel.trends.plot(pt.dat) + theme_bw()


# Teste simples de tendência paralelas ------------------------------------
require(sjPlot)

lm(y.org ~ x+ t*treat*exp ,data=dat)|>tab_model(
  show.ci = F
)

# Sem controles o que acontece
lm(y.org ~  t*treat*exp ,data=dat)|>tab_model(
  show.ci = F
)

