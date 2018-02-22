\name{ANOVA.data}
\alias{ANOVA.data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Transform pedigree data to a list format
%%  ~~function to do ... ~~
}
\description{This function transforms the pedigree file to a format which will be used by PCH4GeneralPed(.) function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ANOVA.data(ntype, c1.traits, Ped)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ntype}{Vector of integers. Its length equals to the total number of families used in the pedigree file. The ntype's entries are the total number of subjects within each family. 
  It can be obtained using Family.sizes(.) function.
%%     ~~Describe \code{ntype} here~~
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
\value{A list of matrices. Its length equals to the number of families used in the study. Each matrix of this list represents one family of the pedigree file 
(i. e. each list entry has subjects of a same family as rows and their corresponding traits as columns). 
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
