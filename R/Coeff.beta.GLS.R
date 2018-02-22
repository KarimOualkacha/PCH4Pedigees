Coeff.beta.GLS <-
function(ntype, X, Y.Cov, Sigma.g, Sigma.e, kin.miss){
m=length(ntype)
p=dim(Sigma.g)[2]

block.diag.W = bdiag(kin.miss)
block.diag.W = 2 * block.diag.W 
DVS.kin = eigen(block.diag.W)
U = DVS.kin$vectors
S = diag(DVS.kin$values)

res.A = solvebeta(0.001,0.001,Sigma.e,Sigma.g)
A = res.A$beta
Gamma = diag(res.A$d)

tUA = (t(U) %x% t(A))
Y.tilde = tUA %*% Y.Cov
X.tilde = tUA %*% X

D = (S %x% Gamma) + diag(1, (dim(U)[1]*dim(A)[1]), (dim(U)[1]*dim(A)[1]))
inv.D = diag(1/diag(D))

tXD = t(X.tilde) %*% inv.D

GLS.beta = solve(tXD %*% X.tilde + 10e-6*diag(1,dim(X.tilde)[2],dim(X.tilde)[2])) %*% tXD %*% Y.tilde
return(GLS.beta)
}
