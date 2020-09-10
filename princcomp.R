par(pin=c(5,5))
n<-500
rho<-.80
plot(-999,-999,xlim=c(-4,4),ylim=c(-4,4),bty="l",xlab=expression(Z[1]),ylab=expression(Z[2]))
for(i in 1:n){
  Z_1<-rnorm(1,0,1); Z_2<-rnorm(1,0,1)
  Z_3<-rho*Z_1+sqrt(1-rho^2)*Z_2
  points(Z_1,Z_3,col="blue")
}
abline(0,1,lwd=2)
abline(0,-1,lwd=2,lty=2)


