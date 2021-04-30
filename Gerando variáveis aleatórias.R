#gerando uma distribuição uniforme  [0,1]
uniforme <-function (mult,mod,seed,size) {
  U <-replicate(size, 0)
  x <- (seed * mult + 1) %% mod
  x[1]<-x / mod
  g <-mod
  for (i in 1:1000){
    x = (x * mult + 1)%% mod
    U[i] = x / mod
  }
  return(U)
}
distuniforme<-uniforme(16807,(2**31)-1,193456789,1000) #distribuição uniforme gerada
z <-replicate(1000, 0)
for (i in 1:12){
  z =z+uniforme(16807,(2**31)-1,193456789-i,1000)
}
distgaussiana <- z-6  #distribuição gaussiana gerada
l<-3 #(lambda) da distribuição exponencial
distexponencial<- -1/l*log(1-distuniforme, base = exp(1)) #distribuição exponencial gerada
#comparando as distribuiçãoes geradas e reais
p1 <- hist(runif(1000))                     
p2 <- hist(distuniforme)      
p3<-hist(rexp(1000, l))
p4 <- hist(distexponencial) 
p5 <-hist(rnorm(1000, mean = 0, sd = 1))
p6 <- hist(distgaussiana)  

plot( p1, col="red") 
plot( p2, col="blue", add=T)  
legend(0.7, 100, legend=c("Real ", "Gerada "),
       col=c("red", "blue"), lty=1:2, cex=0.8)
plot( p3, col="red")  # first histogram
plot( p4, col="blue", add=T)  # seconds
legend(1, 300, legend=c("Real ", "Gerada "),
       col=c("red", "blue"), lty=1:2, cex=0.8)
plot( p5, col="red") 
plot( p6, col="blue", add=T) 
legend(2, 200, legend=c("Real 1", "Gerada "),
       col=c("red", "blue"), lty=1:2, cex=0.8)
