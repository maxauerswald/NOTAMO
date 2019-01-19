\name{gen_multivar_NOTAMO}
\alias{gen_multivar_NOTAMO}
\title{
Generate multivariate Notamo sample
}
\description{
Generates a sample from a multivariate distribution with prespecified moments and correlation matrix. Requires the output of \code{\link{estimate_sum}}.
}
\usage{
gen_multivar_NOTAMO(estimate_sum_out, N)
}

\arguments{
  \item{estimate_sum_out}{
output of \code{\link{estimate_sum}}
}
  \item{N}{
sample size
}
}
\details{
This function samples from multivariate distributions prepared by the \code{\link{estimate_sum}} function. If \eqn{L_i}{Li} is non-normal, the NORTA algorithm is applied to ensure that the resulting variable comply with the prespecified correlation matrix. If \eqn{L_i}{Li} is normal, Cholesky matrix decomposition is used. The multivariate distribution of X_i is different, depending on whether L_i or E_i is normal, despite the same correlation matrix and standardized central moments.
}
\value{
}
\references{
}
\author{
Max Auerswald and Morten Moshagen
}
\note{
}

\seealso{
\code{\link{estimate_sum}}, \code{\link{gen_univar_NOTAMO}}
}
\examples{
}

