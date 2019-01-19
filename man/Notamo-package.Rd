\name{Notamo-package}
\alias{Notamo-package}
\alias{Notamo}
\docType{package}
\title{
\packageTitle{Notamo}
}
\description{
\packageDescription{Notamo}
}
\details{
Notamo has two main functions to sample from either univariate or multivariate distributions. In the univariate case, Notamo estimates inverse cumulative distribution functions (CDFs) that comply with prespecified standardized moments from a set of prespecified inverse CDFs. The resulting quantile mixture allows to generate samples from different distributions that all have the same e.g. skewness and kurtosis.

In the multivariate case, Notamo simulates non-normal variables X_i as the sum of two sets of variables, L_i and E_i. The set of L_i consists of correlated variables and the E_i are independent. Notamo can generate two different types of multivariate distributions: 1) non-normal L_i and normal E_i or 2) normal L_i and non-normal E_i. In the first case, Notamo utilizes the Norta algorithm to generate multivariate samples with a prespecified correlation matrix. In the second case, the correlated L_i are normal and are generated with Cholesky matrix decomposition. For both case, the marginal distribution can be fixed to prespecified skewness, kurtosis, or other standardized central moments, but the distribution is different on a multivariate level. Importantly, the resulting distribution also complies to a prespecified correlation matrix.
}
\author{
\packageAuthor{Notamo}

Maintainer: \packageMaintainer{Notamo}
}
\references{

}

\seealso{
\code{\link{gen_univar_NOTAMO}}, \code{\link{gen_multivar_NOTAMO}}
}
\examples{

}
