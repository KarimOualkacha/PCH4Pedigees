\name{solvebeta}
\alias{solvebeta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generalized eigensystem problem
%%  ~~function to do ... ~~
}
\description{This function gives a solution of the generalized eigensystem problem, see Mardia et al. (1979).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
solvebeta(lambda1, lambda2, SSw, SSb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lambda1}{Real number.
%%     ~~Describe \code{lambda1} here~~
}
  \item{lambda2}{Real number. 
%%     ~~Describe \code{lambda2} here~~
}
  \item{SSw}{Matrix semi-definite positive. 
%%     ~~Describe \code{SSw} here~~
}
  \item{SSb}{Matrix definite positive.
%%     ~~Describe \code{SSb} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{beta}{Eigenvector of the generalized eigensystem problem}
\item{d}{Eigenvalues of the generalized eigensystem problem}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Mardia KV, Kent JT, Bibby JM: Multivariate
Analysis. London, Academic Press, 1979.
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
