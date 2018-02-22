PCH4GeneralPed <-
function(Ped, c1.COV, c1.traits){

ntype.0 = Family.sizes(Ped)
Data.without.MISS = Data.Without.miss(Ped)
ntype = Family.sizes(Data.without.MISS)
nbre.fam = length(ntype)
nbre.subj = sum(ntype)
Y = ANOVA.data(ntype, c1.traits, Data.without.MISS)
kin.miss = kin.data.miss(ntype.0, Ped)
p = dim(Y[[1]])[2] # nbre de traits

if (c1.COV == 0){
                res2 = ANOVA.estimators(ntype, Y, kin.miss)
                hat.Sigma.g = res2$nnd.Mnt
                hat.Sigma.e = res2$nnd.Mnt.e
                Y.COV = Y.cov(ntype, Y)
                X = DESIGN.Matrix.X(p, ntype, c1.COV, c1.traits, Data.without.MISS)
                hat.beta = Coeff.beta.GLS(ntype, X, Y.COV, hat.Sigma.g, hat.Sigma.e, kin.miss)
                hat.intercept = hat.beta
                Cov.beta = "Ther is no covariates in the model"
                }

if (c1.COV != 0){
                Y.COV = Y.cov(ntype, Y)
                X = DESIGN.Matrix.X(p, ntype, c1.COV, c1.traits, Data.without.MISS)
                hat.beta.OLS = solve(t(X) %*% X + 10e-6*diag(1,dim(X)[2],dim(X)[2])) %*% t(X) %*% Y.COV
                res2 = ANOVA.estimators.COV(ntype, hat.beta.OLS, X, Y, kin.miss)
                hat.Sigma.g = res2$nnd.Mnt
                hat.Sigma.e = res2$nnd.Mnt.e
                hat.beta = Coeff.beta.GLS(ntype, X, Y.COV, hat.Sigma.g, hat.Sigma.e, kin.miss)
                hat.intercept = hat.beta[1:p]
                Cov.beta = round(hat.beta[(p+1):(p + (c1.traits - c1.COV))], 4)
                }

# Calculation of the regularistion parameter of Wang et al. (2007, ridge penalized PCH):
N.part=40
m=length(Y)
lambda1=.001
u=matrix(0,N.part,trunc(m/2))
rand.seed.lambda = 1000
set.seed(rand.seed.lambda)
for(k in 1:N.part){
                  u[k, ]=sample(1:m, trunc(m/2))
                  }
CVmax <- optimize(CV.lambda.Wang, c(0, 20), tol = 0.0001, maximum=TRUE, lambda1=lambda1, ntype = ntype, N.part=N.part, u=u, Y=Y, kin.miss=kin.miss)
lambda = CVmax$maximum

h.traits = vector(length = p)
prop.gene = vector(length = p)

Corr.E = cov2cor(hat.Sigma.e)
Corr.P = cov2cor(hat.Sigma.g)
PCH = PCH.Mment(lambda, hat.Sigma.e, hat.Sigma.g)
Diag.var.genetic = PCH$Diag.var.genetic
Orth.Mment = PCH$Orth.Mment
h.PCH = PCH$h.Mment
beta = PCH$beta
for (i in 1:p){
              h.traits[i] = hat.Sigma.g[i,i]/(hat.Sigma.g[i,i] + hat.Sigma.e[i,i])
              prop.gene[i] = Diag.var.genetic[i] / (sum(Diag.var.genetic))
              }
# criterea for the maximum number of PCHs to be hold in the analysis
nbr.pch = 0
for (i in 1:p){
r = 0
for (j in 1:p){
if (h.PCH[i] > h.traits[j]){r = r + 1}
}
if (r == p){nbr.pch = nbr.pch + 1}
}

resultats = list(nbre.families = nbre.fam, nbre.subjects.per.family = ntype, Total.study.subjects = nbre.subj, Intercept = round(hat.intercept, 4), Coeff.Covariates = Cov.beta, 
Var.Polygenic = round(hat.Sigma.g, 6), Var.Env = round(hat.Sigma.e, 6), Corr.Polygenic = round(Corr.P, 4),
Corr.Env = round(Corr.E, 4), All.PCH = round(Orth.Mment, 4), Regularisation.parameter = round(lambda, 4), 
heritability.PCH = round(h.PCH, 4), Trait.heritabilities = round(h.traits, 4), Genetic.Variance.Proportion.PCH = round(prop.gene, 4), nbr.pch = nbr.pch)
resultats
}
