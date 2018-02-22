\name{Ped.without.COVs}
\alias{Ped.without.COVs}
\docType{data}
\title{Pedigree file without covariates
%%   ~~ data name/kind ... ~~
}
\description{Pedigree file with five quantitative traits and without covariates.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(Ped.without.COVs)}
\format{
  A data frame with 385 observations on the following 10 variables.
  \describe{
    \item{\code{V1}}{Family IDs}
    \item{\code{V2}}{Subject IDs}
    \item{\code{V3}}{Father IDs}
    \item{\code{V4}}{Mother IDs}
    \item{\code{V5}}{Sex}
    \item{\code{V6}}{Trait 1}
    \item{\code{V7}}{Trait 2}
    \item{\code{V8}}{Trait 3}
    \item{\code{V9}}{Trait 4}
    \item{\code{V10}}{Trait 5}
  }
}
\details{This data contain 385 subjects (100 families) for which 5 correlated traits are simulated using a mixed model with a global mean alpha = (2,1,3,1,1) and without covariates.
%%\begin{eqnarray}
%%\label{model_Ott} Y_{ij} = \mu + G_{ij} + E_{ij},
%%\end{eqnarray}
%%where $\mu = (2,1,3,1,1)^T \in \Re^5$ is the general mean, with $i$ and $j$ denote respectively the $i-$th family and the $j-$th subject. 
%%The vector $G_{ij} \in \Re^5$ is the residual polygenic component which is normal with mean zero and variance-covariance
%%$\Sigma_{RG}$ and $E_{ij}$ represents the environmental component which is normal with mean zero and variance-covariance $\Sigma_e$. In the simulations we take: 
%%$$
%%\Sigma_e=\left(
%%\begin{array}{ccccc}
%%3.0 & 0.5 &  0  &  0  &  0  \\
%%0.5 & 3.0 &  0  &  0  &  0  \\
%% 0  &  0  &  1  & 0.5 & 0.5 \\
%% 0  &  0  & 0.5 &  1  & 0.5 \\
%% 0  &  0  & 0.5 & 0.5 &  1
%%\end{array} \right ) \quad \Sigma_{RG}=\left(
%%\begin{array}{ccccc}
%%0.5 & 0.3 & 0.3 & 0.3 & 0.3 \\
%%0.3 & 0.5 & 0.3 & 0.3 & 0.3 \\
%%0.3 & 0.3 & 0.5 & 0.3 & 0.3 \\
%%0.3 & 0.3 & 0.3 & 0.5 & 0.3 \\
%%0.3 & 0.3 & 0.3 & 0.3 & 0.5
%%\end{array} \right ).
%%$$
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(Ped.without.COVs)
%%## maybe str(Ped.without.COVs) ; plot(Ped.without.COVs) ...
c1.COV = 0
c1.traits = 6
rand.seed.lambda = sample(1:1000,1)
PCH4GeneralPed(Ped.without.COVs, c1.COV, c1.traits, rand.seed.lambda)
}
\keyword{datasets}
