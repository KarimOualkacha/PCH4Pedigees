\name{ANOVA.estimators.COV}
\alias{ANOVA.estimators.COV}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Variance components estimates in the presence of covariates 
%%  ~~function to do ... ~~
}
\description{This function estimates the genetic and the environmental variance components and the covariate effects using ANOVA and GLS approaches respectively.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ANOVA.estimators.COV(ntype, beta, X, Y, kin.miss)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ntype}{Vector of integers. Its length equals to the total number of families used in the pedigree file. The ntype's entries are the total number of subjects within each family. 
%%     ~~Describe \code{ntype} here~~
}
  \item{beta}{Ordinary least squares estimates of the covariate effects.
%%     ~~Describe \code{beta} here~~
}
  \item{X}{Model design matrix.
%%     ~~Describe \code{X} here~~
}
  \item{Y}{List of matrices obtained from the function ANOVA.data(). Its length equals to the number of families used in the study. 
Each matrix of this list represents one family of the pedigree file (i. e. each list entry has subjects of a same family as rows and their corresponding traits as columns).
%%     ~~Describe \code{Y} here~~
}
  \item{kin.miss}{kinship matrix as a list with length the total number of families. Eeach object of the list represents a family kinship matrix.
%%     ~~Describe \code{kin.miss} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{nnd.Mnt.e}{Environmental variance-covariance matrix}
\item{nnd.Mnt.g}{Genetic variance-covariance matrix}
\item{SSW}{Subject-specific variance-covariance matrix}
\item{SSB}{family-specific variance-covariance matrix}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Oualkacha, K., Labbe, A., Ciampi, A., Roy, M.A. and Maziade, M., (2012). Principal components of
heritability for high dimension quantitative traits and general pedigrees. Journal of Statistical
Applications in Genetics and Molecular Biology, Volume 11. Issue 2, Article 4.
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
