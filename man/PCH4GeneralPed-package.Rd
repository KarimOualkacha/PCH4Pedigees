\name{PCH4GeneralPed-package}
\alias{PCH4GeneralPed-package}
\docType{package}
\title{PCH4GeneralPed: R package for the calculation of principal components of heritability for high dimension quantitative traits and general pedigrees.
}
\description{PCH4GeneralPed: R package for the calculation of Principal Components of Heritability for High Dimension Quantitative Traits and General Pedigrees.
}
\details{This R package fits a linear mixed-effects model in the formulation described in Oualkacha et al. (2012) in the case of family-based designs and an arbitrary number of phenotypes. 
It controlls the family structure using a random effect and allows covariate effects as fixed effects. PCH4GeneralPed package estimates the genetic and the environmental 
variance components and the principal components of heritability based on an ANOVA approach. Fixed effects are estimated using generalized linear squares estimators.

The PCH4GeneralPed package uses the ridge penalized principal components of heritability proposed by Wang et al. (2007) to handle large number of traits. 

Missing data are handled by eliminating the corresponding rows and columns from the analyzed data.

To see funcionality of the PCH4GeneralPed package, you can run the datasets examples: data(Ped.with.COVs) and data(Ped.without.COVs).

Note that except the main function "PCH4GeneralPed", all remaining functions are for personnel usage. 
Thus, users need to run only the main function "PCH4GeneralPed" to get principal components of heritability, see examples below.


\tabular{ll}{
Package: \tab PCH4GeneralPed\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-07-29\cr
License: \tab (>= 2)\cr
}
}
\author{Karim Oualkacha et al.

Maintainer: Karim Oualkacha <oualkacha.karim@uqam.ca>
}
\references{
If you use PCH4GeneralPed package in your analysis, please cite the following work:

Oualkacha, K., Labbe, A., Ciampi, A., Roy, M.A. and Maziade, M., (2012). Principal components of
heritability for high dimension quantitative traits and general pedigrees. Journal of Statistical
Applications in Genetics and Molecular Biology, Volume 11. Issue 2, Article 4.

Wang Y, Fang Y, Jin M (2007). A ridge penalized principal-components approach
based on heritability for high-dimensional data. Hum Hered, 64, 182-191.

Ott J, Rabinowitz D (1999). A principal-components approach based on heritability
for combining phenotype information. Hum Hered, 49, 106-111.
}
\keyword{ package }
\seealso{
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
