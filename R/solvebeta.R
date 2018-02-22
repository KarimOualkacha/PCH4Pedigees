solvebeta <-
function(lambda1,lambda2,SSw,SSb)
 { temp<-eigen(SSw,symmetric=T)
   Uw<-temp$vectors
   diagD<-temp$values+lambda2
   value<-1/sqrt(diagD)
   root<-Uw%*%diag(value)%*%t(Uw)
   SSb<-SSb+lambda1*diag(1,nrow=nrow(SSb),ncol=ncol(SSb))
   m<-root%*%SSb%*%root
   temp1<-eigen(m,symmetric=T)
   beta<-root%*%temp1$vectors
   d<-temp1$values
   return(list(beta=beta, rootw=root,d=d))
 }
