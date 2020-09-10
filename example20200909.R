# This is the Fuji et al example
f<-function(x){
  temp<-(c-(1+7*.9)*x)/(1-rho)
  return(dchisq(x,1)*pchisq(temp,7))
}

rho<-0.99  #### this provides a pvalue (returned by integrate) to be 0.1259
##### or alternatively set this rho to .9 and the pvalue is 0.0055
rho<-.9
T1<-(143-141)/sqrt(10^2/8+15^2/8)
T2<-(132-130)/sqrt(12^2/8+15^2/8)
T3<-(15.3-15.5)/sqrt(1.8^2/8+2^2/8)
T4<-(20.9-21.1)/sqrt(2.2^2/8+2^2/8)
c<-(qnorm(pt(T1,14)))^2+(qnorm(pt(T2,14)))^2+(qnorm(pt(T3,14)))^2+(qnorm(pt(T4,14)))^2
integrate(f,0,5)$value

pchisq(c/8,1)

pchisq(c,8)

