ANOVA.estimators <-
function(ntype, Y, kin.miss){
# ntype.0 c'est un vecteur qui contient la taille de toutes les familles du jeu de donnees total avec des missing data
m=length(ntype)
N=sum(ntype)
p=dim(Y[[1]])[2]

sum.mean.Y.i = matrix(0,p,p)
sum.Y.ij = matrix(0,p,p)
mean.Y = c(rep(0,p))

tau.a = 0
tau.b = 0
tau.c = 0

for (i in 1:m){
W.i = 2*kin.miss[[i]]
if (is.matrix(W.i) == TRUE){
                           n.i = length(W.i[1,])
                           tau.a.i = sum(diag(W.i))
                           tau.b.i = sum(W.i)
                           tau.c.i = (tau.b.i/n.i)
                           } else {
                                  n.i = 1
                                  tau.a.i = W.i
                                  tau.b.i = sum(W.i)
                                  tau.c.i = (tau.b.i/n.i)
                                  }
tau.a = tau.a + tau.a.i
tau.b = tau.b + tau.b.i
tau.c = tau.c + tau.c.i

mean.Y.i=apply(Y[[i]], 2, sum)
mean.Y = mean.Y + mean.Y.i
mean.Y.i=mean.Y.i/n.i
sum.mean.Y.i= sum.mean.Y.i + n.i * mean.Y.i %*% t(mean.Y.i)
sum.Y.ij = sum.Y.ij + t(Y[[i]]) %*% Y[[i]]
}

mean.Y = mean.Y/N
SSW = sum.Y.ij - sum.mean.Y.i
SST = sum.Y.ij - N * mean.Y %*% t(mean.Y)
SSB = sum.mean.Y.i - N * mean.Y %*% t(mean.Y)

a = N - m
b = tau.a - tau.c
c = m - 1
d = tau.c - (tau.b / N)

Mment.g = (SSB/c - SSW/a)/((d/c)-(b/a))
Mment.e = (SSW/a) - (b/a) * Mment.g

nnd.Mnt = eigen(Mment.g)
for (k in 1:p){ if (nnd.Mnt$values[k] < 0) {nnd.Mnt$values[k]=1e-10} }
nnd.Mnt=nnd.Mnt$vectors%*%diag(nnd.Mnt$values)%*%t(nnd.Mnt$vectors)

nnd.Mnt.e = eigen(Mment.e)
for (k in 1:p){ if (nnd.Mnt.e$values[k] <0) { nnd.Mnt.e$values[k]=1e-10 } }
nnd.Mnt.e = nnd.Mnt.e$vectors%*%diag(nnd.Mnt.e$values)%*%t(nnd.Mnt.e$vectors)

ans = list(Mment.g=Mment.g, nnd.Mnt=nnd.Mnt, Mment.e=Mment.e, nnd.Mnt.e=nnd.Mnt.e, SSW=SSW, SSB=SSB)
ans
}
