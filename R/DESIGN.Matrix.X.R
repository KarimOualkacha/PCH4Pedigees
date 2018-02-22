DESIGN.Matrix.X <-
function(p, ntype, c1.COV, c1.traits, Ped){
# c1.cov indique la 1 ere colonne a partir de laquelle la matrice X.COV commence
# c1.trait indique la 1 ere colonne a partir de laquelle la matrice le 1 er trait commence dans Ped
# Marker.COV indique le numero du marqueur en etude (Ex: on veut etudier le marqueur 5 cela veut dire que Marker.COV = 5)

d = c1.traits - c1.COV
X.COV = matrix(0, sum(ntype)*p, d)
I.p = diag(1,p,p)
t.X.intercept = diag(1,p,p)

k2=0
ss=1
for (i in 1:sum(ntype)){
                       if (c1.COV != 0){
                                       d = c1.traits - c1.COV
                                       xx = as.matrix(Ped[i, c1.COV:(c1.traits-1)])
                                       X.COV[(1+(k2*p)):((k2+1)*p), ] =  matrix(rep(xx,p), byrow=TRUE, ncol = d)
                                       }
if (ss==2){t.X.intercept = cbind(t.X.intercept, I.p)}
ss = 2
k2 = k2 + 1
}

X.intercept= t(t.X.intercept)

if (c1.COV == 0) { X = X.intercept }
if (c1.COV != 0) { X = cbind(X.intercept, X.COV) }
return(X)
}
