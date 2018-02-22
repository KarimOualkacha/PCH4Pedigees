\name{PCH4GeneralPed}
\alias{PCH4GeneralPed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ANOVA Estimators of Principal Components of Heritability
%%  ~~function to do ... ~~
}
\description{ Estimation of the principal components of heritability in family-based designs and an arbitrary number of phenotypes using an ANOVA approach.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PCH4GeneralPed(Ped, c1.COV, c1.traits)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ped}{Pedigree file with the first 5 columns should be in the following order: family IDs, subject IDs, father IDs, mother IDs and sex. If there are covariates 
  in the model, they should be introduced after the five standard columns in the pedigree file followed by the quantitative traits.
%%     ~~Describe \code{Ped} here~~
}
  \item{c1.COV}{Integer indicates the column from which the covariates begin in the Pedigree file. If there is no covariate in the pedigree file, this parameter should be set to zero.
%%     ~~Describe \code{c1.COV} here~~
}
  \item{c1.traits}{Integer indicates the column from which the traits begin in the Pedigree file.
%%     ~~Describe \code{c1.traits} here~~
}
}
\details{This function fits a linear mixed-effects model in the formulation described in Oualkacha et al. (2012) in the case of family-based designs and an arbitrary number of phenotypes. 
It controlls for the family structure using a random effect and allows covariate effects as fixed effects. PCH4GeneralPed function estimates the genetic and the environmental 
variance components and the principal components of heritability based on an ANOVA approach. Fixed effects are estimated using generalized linear squares estimators.

We use the ridge penalized principal components of heritability proposed by Wang et al. (2007) to handle large number of traits. 

To see funcionality of the PCH4GeneralPed function, you can run the two datasets examples: data(Ped.with.COVs) and data(Ped.without.COVs).

%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{nbre.families}{Integer number indicates the number of families in th pedigree file.}
\item{nbre.subjects.per.family}{Vector of integers. Its length equals to the total number of families used in the pedigree file. The ntype's entries are the total number of subjects within each family.}
\item{Total.study.subjects}{Total subjects without missing data used in the study.}
\item{Intercept}{Vector of the GLS estimate of the model intercept.}
\item{Coeff.Covariates}{Vector of the GLS estimates of the covariate effects.}
\item{Var.Polygenic}{Genetic variance-covariance matrix.}
\item{Var.Env}{Environmental variance-covariance matrix.}
\item{Corr.Polygenic}{Genetic correlation matrix.}
\item{Corr.Env}{Environmental correlation matrix.}
\item{All.PCH}{Ridge penalized principal components of heritability.}
\item{Regularisation.parameter}{Regularisation parameter.}
\item{heritability.PCH}{Vector of heritabilities of all the ridge penalized principal components of heritability.}
\item{Trait.heritabilities}{Vector of single trait heritabilities.}
\item{Genetic.Variance.Proportion.PCH}{Vector with entries represent the proportion of the total genetic variation explained by each principal component of heritability. The proportion of the total genetic variation explained 
by a gievn PCH, is defined as the ratio of the eigenvalue of the genetic variance-covariance matrix that associated with this PCH and the sum of all the eigenvalues of the genetic variance-covariance matrix.}
\item{nbr.pch}{Number of the significant principal components to be hold in the analysis.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{If you use PCH4GeneralPed package in your analysis, please cite the following work:

Oualkacha, K., Labbe, A., Ciampi, A., Roy, M.A. and Maziade, M., (2012). Principal components of
heritability for high dimension quantitative traits and general pedigrees. Journal of Statistical
Applications in Genetics and Molecular Biology, Volume 11. Issue 2, Article 4.

Wang Y, Fang Y, Jin M (2007). A ridge penalized principal-components approach
based on heritability for high-dimensional data. Hum Hered, 64, 182-191.

Ott J, Rabinowitz D (1999). A principal-components approach based on heritability
for combining phenotype information. Hum Hered, 49, 106-111.
%% ~put references to the literature/web site here ~
}
\author{Karim Oualkacha et al.
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data(Ped.with.COVs)
c1.COV = 6
c1.traits = 10
PCH4GeneralPed(Ped.with.COVs, c1.COV, c1.traits)

data(Ped.without.COVs)
c1.COV = 0
c1.traits = 6
PCH4GeneralPed(Ped.without.COVs, c1.COV, c1.traits)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
