f<-function(x){
temp1<-(u-x)/(1-rho)
temp2<-x/(1+(k-1)*rho)
return(pchisq(temp1,k-1)*dchisq(temp2,1))
}
alpha<-0.05

##########################################################################
#####  Table 1
# error rate: variability concentrated in j directions
for(j in 1:3){
for(k in c(1,5,10,25,100)){
#j<-k
u<-qchisq(.05,k)     # least conservative critical value
temp3<-j*u/k
if(k>=j) print(paste(j,"direction, k=",k,"p=",pchisq(temp3,j))) 

}
}
#### by correlation rho
for(rho in c(0,.25,.5,.75)){
for(k in c(1,5,10,25,100)){
 #u<-k*qchisq(alpha,1) # most conservative critical value
u<-qchisq(.05,k)     # least conservative critical value
probby<-(1/(1+(k-1)*rho))*integrate(f,0,u)$value
print(paste(rho,k,probby))

}
}
##########################################################################


##########################################################################
#####  Table 2
# error rate: variability concentrated in j directions
for(j in 1:3){
for(k in c(1,5,10,25,100)){
 u<-k*qchisq(alpha,1) # most conservative critical value
temp3<-j*u/k
if(k>=j) print(paste(j,"direction, k=",k,"p=",pchisq(temp3,j))) 

}
}

#### by correlation rho
for(rho in c(0,.25,.5,.75)){
for(k in c(1,5,10,25,100)){
 u<-k*qchisq(alpha,1) # most conservative critical value
#u<-qchisq(.05,k)     # least conservative critical value
probby<-(1/(1+(k-1)*rho))*integrate(f,0,u)$value
print(paste(rho,k,probby))
}
}

##########################################################################


