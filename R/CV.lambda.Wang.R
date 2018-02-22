CV.lambda.Wang <-
function(lambda1, lambda2, ntype, N.part, u, Y, kin.miss){
# lambda c est le parametre de la regularisation
# m nbre total de familles
# u matrice qui divise aleatoirement les m familles en deux jeux de donnees pour la validation croisee (voir la fonction MAIN FUNCTION pour un exemple de u)
# N.part c est le nbre de fois que nous allons separer le jeu de donnees global en deux jeux de donnees (ex: N.part=40, i.e. 40 paires (training data, validation data) de partitions independantes)

m=length(Y)
x=c(1:m)
CV.herid = 0
for(k in 1:N.part){
u1=u[k, ]
u2=x[-u1]
training=Y[u1]
kin1 = kin.miss[u1]
ntype1 = ntype[u1]

validation=Y[u2]
kin2 = kin.miss[u2]
ntype2 = ntype[u2]

res1 = ANOVA.estimators(ntype1, training, kin1)
traininig.Mment.g = res1$nnd.Mnt
traininig.Mment.e = res1$nnd.Mnt.e

res2 = ANOVA.estimators(ntype2, validation, kin2)
Sigma.g=res2$nnd.Mnt
Sigma.e=res2$nnd.Mnt.e

beta=solvebeta(lambda1, lambda2, traininig.Mment.e, traininig.Mment.g)$beta
beta1=beta[,1]
CV.herid=CV.herid + ( t(beta1)%*%Sigma.g%*%beta1 )/( t(beta1)%*%(Sigma.e)%*%beta1 )
}
CV.herid=CV.herid/N.part
return(CV.herid)
}
