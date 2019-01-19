\name{estimate_sum}
\alias{estimate_sum}
\title{
Estimate sum variables
}
\description{
Estimates non-normal variables as the sum of normal and non-normal variables. The function \code{\link{gen_multivar_NOTAMO}} can be used to sample from the result of this function.
}
\usage{
estimate_sum(icdf_list, targetcor, targetmom, Lnn=TRUE, Lmaxcor=0.9, acc=7, verbose=TRUE)
}

\arguments{
  \item{icdf_list}{
A nested list. Each element contains a list of inverse CDFs and additional parameters. See \code{\link{Details}}, \code{\link{Examples}}, and \code{\link{estimate_quantile_mix}} for additional information.
}
  \item{targetcor}{
The desired correlation matrix
} \item{targetmom}{
A list that contains matrices with the order and value of desired moments for each non-normal random variable. In each matrix, the first column contains the order of the moment, the second column contains the value.
} \item{Lnn}{
Logical. If TRUE, the correlated variables are non-normal and the independent variables are normal. If FALSE, vice versa. See \code{\link{Details}}.
} \item{Lmaxcor} {
The maximum allowed correlation for the \eqn{L_i}{Li} variables.
} \item{acc}{
An integer used to set up a vector of p-values to approximate the resulting distributions.
} \item{verbose}{
Logical. If TRUE, various additional outputs are printed during the estimation.
}
}
\details{
The goal of this function is to prepare sampling from a non-normal random variable \eqn{X_i}{Xi} as the weighted sum of two random variable, correlated \eqn{L_i}{Li} and independent \eqn{E_i}{Ei}. Specifically, \eqn{X_i}{Xi} is defined as \deqn{X_i = \sqrt{c}L_i + \sqrt{1-c}E_i}{Xi = c^(1/2)*Li + (1-c)^(1/2)*Ei}
The function consists of four steps:
\enumerate{
\item The weight \eqn{c} is calculated so that the target correlation among \eqn{X_i}{Xi} can be reproduced if \eqn{L_i}{Li} and \eqn{E_i}{Ei} have unit variance, depending on the maximum allowed correlation among \eqn{L_i}{Li} (\code{Lmaxcor}).
\item The target moments (e.g. skewness, kurtosis) are adjusted by anticipating the change that occurs by adding a standard normal variable.
\item The function \code{\link{estimate_quantile_mix}} is applied to every \eqn{X_i}{Xi}, based on the adjusted target moments from the previous step.
\item Finally, the function ensures that the non-normal distributions of \eqn{L_i}{Li} or \eqn{E_i}{Ei} have unit variance.
}
The output of \code{estimate_sum} contains suitable distributions for \eqn{L_i}{Li} and \eqn{E_i}{Ei}, from which the function \code{\link{gen_multivar_NOTAMO}} samples.
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
\code{\link{gen_multivar_NOTAMO}}, \code{\link{estimate_quantile_mix}}
}
\examples{
# Define list of inverse CDFs

icdf_list <- list()
icdf_list[[1]] <- list(
  list(qlnorm),
  list(qnorm)
)
icdf_list[[2]] <- list(
  list(qnormcube <- function(p) {
    return(qnorm(p)^3)
  }),
  list(qnorm)
)
icdf_list[[3]] <- list(
  list(qlnorm),
  list(qnormcube <- function(p) {
    return(qnorm(p)^3)
  }),
  list(qnorm)
)

# Define target correlation and moments
targetcor <- matrix(0.3,3,3)
diag(targetcor) <- 1
targetmom <- list(matrix(c(3,1),nrow=1),    #1st var: skewness=1
                  matrix(c(4,10),nrow=1),   #2nd var: kurtosis=10
                  matrix(c(3,1,             #3rd var: skewness=1,
                           4,10),nrow=2,byrow=TRUE))# kurtosis=10

# Estimate sum of distributions

res <- estimate_sum(icdf_list, targetcor, targetmom, Lnn=TRUE)

# Sample from distribution

y <- gen_multivar_NOTAMO(res, N=100000)

# Check results:

cor(y)
skewness(y[,1])
kurtosis(y[,2])
skewness(y[,3])
kurtosis(y[,3])

}

