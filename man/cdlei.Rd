\name{cdlei}
\alias{cdlei}

\title{
The life expectancy improvement with a cure distribution for a cause of death.
}
\description{In may circumstances, to increase in life expectancy when a certain cause of death is eliminated is sought, but this is usually done by taking the cause out of consideration fully, which is unrealistic. Here, we incorporate a probability distribution for the cure of the cause over time, to more accurately predict the increase in life expectancy at each age.}

\usage{
cdlei(age, qtau, qhiv, k, d)
}

\arguments{
  \item{age}{age}
  \item{qtau}{vector of probabilities of death by all causes at each age}
  \item{qhiv}{vector of probabilities of death by HIV at each age}
  \item{k}{cure probability parameter}
  \item{d}{index}
}

\value{
\item{cdlei}{cause-deleted life expectancy}
\item{qx}{probability of deatch at age x}
\item{px}{probability of survival at age x}
\item{tpx}{probability an x year old survives to age x+t}
\item{sumtpx}{sum of tpx}
\item{Fk}{probability of curve}
\item{pxx}{probability of survival at age x, using cure probability}
\item{tpxx}{probability of sirviving t years after age x, using cure probability}
\item{sumtpxx}{cumulative sum of tpx}
\item{df}{data frame}
}

\references{
1. Adamic, P. (2015). Life Expectancy Improvement with a Curve Distribution for a cause of death, Australian Journal of Actuarial Practice, 3, 59-70. \cr
2. Adamic, P. (2008). Cause-deleted life expectancy improvement in the presence of
left and right censoring. Belgian Actuarial Bulletin, 8: 17-21. \cr
3. Brown, R.L. (1997). Introduction to the Mathematics of Demography, 3rd ed, Winsted, Connecticut: Actex. \cr

}
\author{
Peter Adamic, Alicja Wolny-Dominiak}

\examples{
data(lifeData)
res <- cdlei(lifeData$age, lifeData$qtau, lifeData$qhiv, 0.02, 100000)
str(res)
res$cdlei

}
