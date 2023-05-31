


#DECOMPOSIÇÃO FATORIAL DE POLINÔMIOS DE ORDEM 2 NO PRODUTO DE POLINÔMIOS DE ORDEM 1
a <- 4
b <- 5
c <- -8



lambda1 <- a/c
lambda2 <- b/c
delta   <- lambda2^2 - 4*lambda1
delta

E1 <- (-lambda2 - sqrt(delta))/2
E2 <- (-lambda2 + sqrt(delta))/2
  
  h <- function(x){a*x^2 + b*x  + c}
  f <- function(x){(a/c)*x^2 + (b/c)*x  + c/c}
  
  plot(h,xlim=c(-10,10), ylim=c(-10,10),axes=F)
  axis(1,pos=c(0,0))
  axis(2,pos=c(0,0))
  par(new=T)
  plot(f,xlim=c(-10,10), ylim=c(-10,10),axes=F,col=2)
  E1;E2
  
  
  h(1/E1);h(1/E2)
  f(1/E1);f(1/E2)

  # TRIANGULO 
  
  phi1 <- seq(from = -3, to = 3, length = 51) 
  plot(phi1,1+phi1,lty="dashed",type="l",xlab="",ylab="",cex.axis=.8,ylim=c(-3,3),axes = F,main = "Triangulo de Argand - AR(2)")
  abline(a = -1, b = 0, lty="dashed")
  abline(a = 1, b = -1, lty="dashed")
  axis(1,pos=c(0,0))
  axis(2,pos=c(0,0))
  title(ylab=expression(phi[2]),xlab=expression(phi[1]),cex.lab=.8)
  polygon(x = seq(-2,2), y = 1-abs(seq(-2,2)), col= rgb(0,0,0,.1))
  lines(phi1,-phi1^2/4)
  text(0,-.75,expression(phi[2]<phi[1]^2/4),cex=.7, col =  2)
  text(1.2,.5,expression(phi[2]>1-phi[1]),cex=.7)
  text(-1.75,.5,expression(phi[2]>1+phi[1]),cex=.7)
  
  polygon(x = seq(-2,2,length=1000), y = -(seq(-2,2,length=1000)^2)/4, col= rgb(1,0,0,.2))
  