PCH.Mment <-
function(lambda, nnd.Mnt.e, nnd.Mnt){
p=dim(nnd.Mnt)[2]
Orth.Mment=matrix(0,p,p)
res1 = solvebeta(0.001,lambda,nnd.Mnt.e,nnd.Mnt)
PCH.Mment = res1$beta
Diag.var.genetic = res1$d
for(i in 1:p){ Orth.Mment[ ,i] = PCH.Mment[,i]/sqrt(t(PCH.Mment[,i])%*%PCH.Mment[,i]) }
PCH.Mment1 = Orth.Mment[ ,1]
h.Mment=vector(length = p)
for (i in 1:p){
h.Mment[i] = t(Orth.Mment[ ,i])%*%(nnd.Mnt)%*%Orth.Mment[ ,i]/(t(Orth.Mment[ ,i])%*%(nnd.Mnt + nnd.Mnt.e)%*%Orth.Mment[ ,i])
}
ord.h = order(h.Mment, decreasing=T)
Orth.Mment = Orth.Mment[,ord.h]
h.Mment = h.Mment[ord.h]
ans = list(Orth.Mment=Orth.Mment, PCH.Mment1=PCH.Mment1, h.Mment=h.Mment, beta=PCH.Mment, Diag.var.genetic=Diag.var.genetic)
ans
}
