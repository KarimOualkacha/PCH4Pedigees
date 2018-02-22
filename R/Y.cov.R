Y.cov <-
function(ntype, Y){
m=length(ntype)
mat.Y=t(Y[[1]])

for(i in 1:(m-1)){ mat.Y = cbind(mat.Y, t(Y[[i+1]])) }

ss=sum(ntype)
mat.Y.p = matrix(c(mat.Y),ncol=ss)
Y.Cov = c(mat.Y.p)
return(Y.Cov)
}
