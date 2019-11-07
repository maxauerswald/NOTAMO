\name{estimate_quantile_mix}
\alias{estimate_quantile_mix}
\title{
Estimate quantile mixture
}
\description{
Estimates parameters of a quantile mixture distribution given a set of inverse cumulative distribution functions (CDFs) and  prespecified central moments. The output is the inverse CDF of a distribution with the prespecified moments. The function \code{\link{gen_univar_NOTAMO}} can be used to sample from this distribution.
}
\usage{
estimate_quantile_mix(icdf_list, targetmom, acc=7, startval=NULL, alg="NLOPT_GN_CRS2_LM", maxeval=500)
}

\arguments{
  \item{icdf_list}{
A list of lists that contains inverse CDFs and optional parameters. The weighted sum of these inverse CDFs is used to find one CDF that complies with the prespecified moments. See \code{\link{Details}} and \code{\link{Examples}} for additional information.
}
  \item{targetmom}{
A matrix that contains the order and value of desired moments. The first column contains the order, the second column contains the value.
}
\item{acc}{
An integer used to set up a vector of p-values to approximate the resulting distributions.
}
  \item{startval}{
Starting values for the weights in the quantile mixture distribution.
}
  \item{alg}{
Algorithm used in the non-linear optimization procedure. Default is a controlled random search with local mutation. See package \code{\link[nloptr]{nloptr}} for details.
}
  \item{maxeval}{
Maximum number of iterations used in the root solving or non-linear optimization procedure.
}
}
\details{
The function estimates parameters \eqn{a \ge 0} of a quantile mixture distribution of the form \deqn{a_1 F_1^{-1} + a_2 F_2^{-1} + .... + a_n F_n^{-1}}{a1 * iCDF1 + a2 * iCDF2 + ... + an * iCDFn} where \eqn{F^{-1}}{iCDF} denotes inverse cumulative distribution functions and \eqn{sum(a) = 1}. The result is the solution for the parameters \eqn{a} that are associated with a distribution having the desired standardized central moments.

The \code{icdf_list} input is a list of lists. Every list contains one \code{function} that is an inverse CDF and optional additional parameters that are passed on to the respective inverse CDF (see \code{Example} 2).

The function uses a non-linear root-finding algorithm if the number of prespecified moments is equal to the number provided inverse CDFs plus one (see \code{\link[rootSolve]{multiroot}}). If the number of prespecified moments is smaller than that, a non-linear optimization function is used instead (see \code{\link[nloptr]{nloptr}}). In general, the non-linear root finding is recommended because it is both faster, more accurate, and thus recommended.
}
\value{

\item{solution}{Parameters to be used for the quantile mixture distribution that yield a distribution with the desired central moments.}
\item{exp_moms}{The moments expected when generating data from the resulting quantile mixture distribution.}
\item{ssd}{The sum of the squared difference between desired and expected moments.}
\item{presp_moms}{Prespecified moments (for comparison to expected moments).}
\item{presp_funs}{Prespecified inverse CDFs}
}
\references{
}
\author{
Max Auerswald and Morten Moshagen
}
\note{
The algorithm is not guaranteed to converge.
}

\seealso{
\code{\link{gen_univar_NOTAMO}}
}
\examples{
### Example 1: Root-finding algorithm (number of CDFs = number of prespecified moments + 1):

# Define set of three inverse CDFs:
icdf_list <- list(
  list(qexp),
  list(qnormcube <- function(p) {
    return(qnorm(p)^3)
  }),
  list(qnorm)
)

# Define target moments:
moms <- matrix(0,nrow=2,ncol=2)
moms[1,] <- c(3,1)   #desired skewness is 1
moms[2,] <- c(4,10)  #desired kurtosis is 10

# Estimate parameters:
res1 <- estimate_quantile_mix(icdf_list,moms)

# Generate univariate sample from distribution with given moments and N=100000:
y1 <- gen_univar_NOTAMO(res1,100000)

# Calculate kurtosis of random variable
kurtosis(y1)

### Example 2: Non-linear optimization (number of CDFs > number of prespecified moments + 1):

# Define set of four inverse CDFs:
icdf_list2 <- list(
  list(qexp),
  list(qnormcube <- function(p) {
    return(qnorm(p)^3)
  }),
  list(qexp,rate=2),
  list(qnorm)
)

# Estimate parameters:
res2 <- estimate_quantile_mix(icdf_list2,moms)

# Generate univariate sample from distribution with given moments and N=100000:
y2 <- gen_univar_NOTAMO(res2,100000)

# The resulting distribution is different from the first example, despite having the same skewness and kurtosis:
kurtosis(y2)
}

