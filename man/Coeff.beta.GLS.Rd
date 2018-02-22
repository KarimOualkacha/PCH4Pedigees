\name{Coeff.beta.GLS}
\alias{Coeff.beta.GLS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generalized Least Squares estimators of the covaraite effects
%%  ~~function to do ... ~~
}
\description{This function calculate GLS estimates of the covariate coefficients.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Coeff.beta.GLS(ntype, X, Y.Cov, Sigma.g, Sigma.e, kin.miss)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ntype}{Vector of integers. Its length equals to the total number of families used in the pedigree file. The ntype's entries are the total number of subjects within each family.
%%     ~~Describe \code{ntype} here~~
}
  \item{X}{Model design matrix.
%%     ~~Describe \code{X} here~~
}
  \item{Y.Cov}{Vector in which each subject is represented by the p columns (p is the number of traits considered in the study).
%%     ~~Describe \code{Y.Cov} here~~
}
  \item{Sigma.g}{An ANOVA estimate of the genetic variance component.
%%     ~~Describe \code{Sigma.g} here~~
}
  \item{Sigma.e}{An ANOVA estimate of the environmental variance component.
%%     ~~Describe \code{Sigma.e} here~~
}
  \item{kin.miss}{List of matrices. Its length equals to the total number of families. The list entries represent the family kinship matrices.
%%     ~~Describe \code{kin.miss} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Vector of the GLS estimates of the covariate effects, included the intercept.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Karim Oualkacha
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
%%##---- Should be DIRECTLY executable !! ----
%%##-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
