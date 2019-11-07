\name{calc_std_mom}
\alias{calc_std_mom}
\title{
Calculate standardized central moments
}
\description{
Calculates standardized sample central moments of a sample.
}
\usage{
calc_std_mom(data, moment)
}

\arguments{
  \item{data}{
A vector containing the univariate data
}
  \item{moment}{
An integer, order of moment to be calculated
}
}
\details{
Calculates the sample central moment. If the order is 3, the output is the sample skewness, if the order is 4, the sample kurtosis, and otherwise the standardized central moment.
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
\code{\link[moments]{moment}}
}
\examples{
calc_std_mom(rnorm(1000),4)
}

