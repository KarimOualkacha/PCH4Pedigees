\name{CV.lambda.Wang}
\alias{CV.lambda.Wang}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cross validation techniques for regularisation parameter 
%%  ~~function to do ... ~~
}
\description{This function use cross validation techniques to calculate the regularisation parameter, Wang et al. (2007).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CV.lambda.Wang(lambda1, lambda2, ntype, N.part, u, Y, kin.miss)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lambda1}{Real number.
%%     ~~Describe \code{lambda1} here~~
}
  \item{lambda2}{Real number.
%%     ~~Describe \code{lambda2} here~~
}
  \item{ntype}{Vector of integers. Its length equals to the total number of families used in the pedigree file. The ntype's entries are the total number of subjects within each family. 
%%     ~~Describe \code{ntype} here~~
}
  \item{N.part}{Integer indicates the number of times that the data will be splited to training data and validation data.
%%     ~~Describe \code{N.part} here~~
}
  \item{u}{Matrix of integers used to separate randomly the families to two groups of families: training and validation data.  
%%     ~~Describe \code{u} here~~
}
  \item{Y}{List of matrices obtained from the function ANOVA.data(). Its length equals to the number of families used in the study. 
Each matrix of this list represents one family of the pedigree file (i. e. each list entry has subjects of a same family as rows and their corresponding traits as columns).
%%     ~~Describe \code{Y} here~~
}
  \item{kin.miss}{List of matrices. Its length equals to the total number of families. The list entries represent the family kinship matrices.
%%     ~~Describe \code{kin.miss} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{Cross validation heritability, see Wang et al. (2007).
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{Wang Y, Fang Y, Jin M (2007). A ridge penalized principal-components approach
based on heritability for high-dimensional data. Hum Hered, 64, 182-191.
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
