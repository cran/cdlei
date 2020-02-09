\name{Fk}
\alias{Fk}

\title{
Curve Probability function
}
\description{
A simple discrete-time function accounting for the probability that HIV will be cured by time t. Assume the curve function begins at age 0.
}
\usage{
Fk(age, k)
}

\arguments{
  \item{age}{age of person}
  \item{k}{cure probability parameter}
}

\value{
 \item{Fk}{curve probability function}
}
\references{
1. Adamic, P. (2008). Cause-deleted life expectancy improvement in the presence of
left and right censoring. Belgian Actuarial Bulletin, 8: 17-21. \cr
2. Brown, R.L. (1997). Introduction to the Mathematics of Demography, 3rd ed, Winsted, Connecticut: Actex. \cr
}

\author{
Peter Adamic, Alicja Wolny-Dominiak}

\examples{
data(lifeData)
Fk(lifeData$age, 0.02)
}