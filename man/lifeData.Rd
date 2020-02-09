\name{lifeData}
\alias{lifeData}
\docType{data}
\title{
HIV-related deaths from Colorado, USA, between 2000-2012.
}
\description{
Input data matrix consists of the probabilities of death from all causes, and by HIV only, for ages 0 to 103 (inclusive).
}
\usage{data("lifeData")}
\format{
  A data frame with 104 observations on the following 3 variables.
  \describe{
    \item{\code{age}}{a numeric vector}
    \item{\code{qtau}}{a numeric vector}
    \item{\code{qhiv}}{a numeric vector}
  }
}


\source{
Data source: Colorado Department of Public Health and Environment.
}

\examples{
data(lifeData)
str(lifeData)
}
