\name{Y.cov}
\alias{Y.cov}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get all traits as one vector
%%  ~~function to do ... ~~
}
\description{This function transforms the traits from the pedigree file to a single vector
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Y.cov(ntype, Y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ntype}{Vector of integers. Its length equals to the total number of families used in the pedigree file. The ntype's entries are the total number of subjects within each family. 
%%     ~~Describe \code{ntype} here~~
}
  \item{Y}{List of matrices obtained from the function ANOVA.data(). Its length equals to the number of families used in the study. 
Each matrix of this list represents one family of the pedigree file (i. e. each list entry has subjects of a same family as rows and their corresponding traits as columns).
%%     ~~Describe \code{Y} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{vector in which each subject is represented by p columns, where p is the number of traits considered in the pedigree file.
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
