\name{burtTable}
\alias{burtTable}

\title{Creates Burt Matrix}
\description{Utility function to produce a Burt matrix out of a data-frame.
}
\usage{
burtTable(data)
}

\arguments{
  \item{data}{Data frame to be converted.}
}

\seealso{\code{\link{expandFrame}}, \code{\link{mkIndiList}}}
\examples{

## sleeping bags
data(sleeping)
sleeping_cat <- sleeping
temp_cat <- cut(sleeping$Temperature, c(-20, -1, 7), labels = c("warm", "cold")) 
sleeping_cat$Temperature <- temp_cat
weight_cat <- cut(sleeping$Weight, c(700, 1100, 2200), labels = c("light", "heavy")) 
sleeping_cat$Weight <- weight_cat
price_cat <- cut(sleeping$Price, c(100, 250, 350, 700), 
labels = c("cheap", "medium", "expensive"))  
sleeping_cat$Price <- price_cat
sleeping_cat
burtTable(sleeping_cat)

}

\keyword{models}

