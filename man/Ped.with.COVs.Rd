\name{Ped.with.COVs}
\alias{Ped.with.COVs}
\docType{data}
\title{Pedigree file with covariates
%%   ~~ data name/kind ... ~~
}
\description{Pedigree file with four covariates and five quantitative traits.
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(Ped.with.COVs)}
\format{
  A data frame with 385 observations on the following 14 variables.
  \describe{
    \item{\code{V1}}{Family IDs}
    \item{\code{V2}}{Subject IDs}
    \item{\code{V3}}{Father IDs}
    \item{\code{V4}}{Mother IDs}
    \item{\code{V5}}{Sex}
    \item{\code{V6}}{First covariate}
    \item{\code{V7}}{Second covariate}
    \item{\code{V8}}{Third covariate}
    \item{\code{V9}}{Forth covariate}
    \item{\code{V10}}{Trait 1}
    \item{\code{V11}}{Trait 2}
    \item{\code{V12}}{Trait 3}
    \item{\code{V13}}{Trait 4}
    \item{\code{V14}}{Trait 5}
  }
}
\details{This data contain 385 subjects (100 families) for which 5 correlated traits are simulated using a mixed model with a global mean alpha = (2,1,3,1,1)
and 4 covariates with fixed effects beta = (1, 0.5, 2, 3).
%%: Y = mu + X beta + G + E
%%\begin{eqnarray}
%%\label{model_Ott} Y_{ij} = \mu + X^{T}_{ij} \beta + G_{ij} + E_{ij},
%%\end{eqnarray}
%%where $\mu = (2,1,3,1,1)$ is the general mean, $\beta = (\beta_1, \beta_2, \beta_3, \beta_4)^T = (1, 0.5, 2, 3)^T$ is a vector of fixed effects for the covariates, 
%%with $i$ and $j$ denote respectively the $i-$th family and the $j-$th subject. The vector $G_{ij} \in \Re^5$ is the residual polygenic
%%component which is normal with mean zero and variance-covariance
%%$\Sigma_{RG}$ and $E_{ij}$ represents the environmental component
%%which is normal with mean zero and variance-covariance $\Sigma_e$. In the simulations we take: 
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
data(Ped.with.COVs)
%%## maybe str(Ped.with.COVs) ; plot(Ped.with.COVs) ...
c1.COV = 6
c1.traits = 10
rand.seed.lambda = sample(1:1000,1)
PCH4GeneralPed(Ped.with.COVs, c1.COV, c1.traits, rand.seed.lambda)
}
\keyword{datasets}
