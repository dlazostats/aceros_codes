# Non Linear Least Squares GN and LvM
#====================================
library(dplyr)
library(numDeriv)
library(minpack.lm)
library(pracma)
options(scipen=999)

# Example 1
#-----------
dta<-data.frame(year=seq(1815,1885,10),pop=c(8.3,11,14.7,19.7,26.7,35.2,44.4,55.9),
                t=1:8)
plot(dta$year,dta$pop)

## Gauss-Newton
#--------------
lr<-list()
tol<-0.0001
niter<-20
xini=matrix(c(6,0.3),ncol=1)
for(k in 1:niter){
  #residuals
  mdli=function(t) xini[1]*exp(xini[2]*t)
  res_fun<-function(t,y) mdli(t)-y
  res=res_fun(dta$t,dta$pop)
  resm=matrix(res,ncol=1)
  #jacobian
  grdl=lapply(1:8,function(t) grad(function(prm) {prm[1]*exp(prm[2]*t)},
                                   c(xini[1],xini[2])))
  jacb=do.call("rbind",grdl) %>% as.matrix()
  pk=-solve(t(jacb)%*%jacb)%*%t(jacb)%*%resm   # pk = (J'J)-1 J'r
  xnw=xini+pk
  lr[[k]]<-xini
  err<-sqrt(sum(((xnw-xini)^2)))
  print(cbind(xini,xnw))
  print(sum(res^2))
  xini<-xnw
  if(err<tol){ 
    break
  }
}
drgn<-do.call("cbind",lr)
drgn

## automatic function
t=dta$t
y <- dta$pop
gn<-nls(formula=y~x1*exp(x2*t),
        start=list(x1=6,x2=0.3),
        trace = T)
gn

# Levenberg-Marquardt
#---------------------
lm_mdl<-nlsLM(y~x1*exp(x2*t),
              start=list(x1=6,x2=0.3),
              algorithm = "LM",
              trace=T)
lm_mdl
