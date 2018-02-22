\name{Data.Without.miss}
\alias{Data.Without.miss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get Pedigree data without missing values
%%  ~~function to do ... ~~
}
\description{This function keeps in the pedigree file only subjects without missing data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Data.Without.miss(Ped)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Ped}{Pedigree file with the first 5 columns should be in the following order: family IDs, subject IDs, father IDs, mother IDs and sex. If there are covariates 
  in the model, they should be introduced after the five standard columns in the pedigree file followed by the quantitative traits.
%%     ~~Describe \code{Ped} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{ A list of matrices. Its length equals to the number of families used in the study. 
Each matrix of this list represents one family of the pedigree file (i. e. each list entry has subjects of a same family, without missing data, as rows and their corresponding traits as columns).

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
