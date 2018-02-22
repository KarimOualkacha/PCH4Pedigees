\name{DESIGN.Matrix.X}
\alias{DESIGN.Matrix.X}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get the model design matrix
%%  ~~function to do ... ~~
}
\description{This function calculates the model design matrix from the pedigree file.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DESIGN.Matrix.X(p, ntype, c1.COV, c1.traits, Ped)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{The number of quantitative traits that are used in the pedigree file.
%%     ~~Describe \code{p} here~~
}
  \item{ntype}{Vector of integers. Its length equals to the total number of families used in the pedigree file. The ntype's entries are the total number of subjects within each family. 
%%     ~~Describe \code{ntype} here~~
}
  \item{c1.COV}{Integer number indicates the column from which the covariates begin in the Pedigree file. If there is no covariate in the pedigree file, this parameter should be set to zero.
%%     ~~Describe \code{c1.COV} here~~
}
  \item{c1.traits}{Integer number indicates the column from which the traits begin in the Pedigree file.
%%     ~~Describe \code{c1.traits} here~~
}
  \item{Ped}{Pedigree file with the first 5 columns should be in the following order: family IDs, subject IDs, father IDs, mother IDs and sex. If there are covariates 
  in the model, they should be introduced after the five standard columns in the pedigree file followed by the quantitative traits.
%%     ~~Describe \code{Ped} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{The model design matrix.
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
