\name{PCH.Mment}
\alias{PCH.Mment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Principal components of heritability
%%  ~~function to do ... ~~
}
\description{This function calculates the ridge penalized principal components of heritability.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PCH.Mment(lambda, nnd.Mnt.e, nnd.Mnt)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lambda}{Regularization parameter (real number).
%%     ~~Describe \code{lambda} here~~
}
  \item{nnd.Mnt.e}{The environmental variance component.
%%     ~~Describe \code{nnd.Mnt.e} here~~
}
  \item{nnd.Mnt}{The genetic variance component.
%%     ~~Describe \code{nnd.Mnt} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{Orth.Mment}{Ridge penalized principal components of heritability}
\item{PCH.Mment1}{First ridge penalized principal components of heritability}
\item{h.Mment}{Vector of heritabilities of all the ridge penalized principal components}
\item{beta}{Eigenvector of the generalized eigensystem problem}
\item{Diag.var.genetic}{Eigenvalues of the generalized eigensystem problem, see the function solvebeta(.)}
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

\seealso{solvebeta
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
%%---- Should be DIRECTLY executable !! ----
%%-- ==>  Define data, use random,
%%##--	or do  help(data=index)  for the standard data sets.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
