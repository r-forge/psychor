\name{med1way}
\alias{med1way}
\alias{print.med1way}
\title{
Heteroscedastic one-way ANOVA for medians.
}
\description{
This function computes a one-way ANOVA for the medians. Homoscedasticity assumption not required. There shouldn't be too many ties.
}
\usage{
med1way(formula, data,iter = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
an object of class formula.
}
  \item{data}{
an optional data frame for the input data.
}
  \item{iter}{
number of iterations.
}
}
\value{
Returns an object of class \code{med1way} containing:

\item{test}{value of the test statistic}
\item{crit.val}{critical value}
\item{p.value}{p-value}
\item{call}{function call}
}
\references{
Wilcox, R. (2012). Introduction to Robust Estimation and Hypothesis Testing (3rd ed.). Elsevier.
}
\seealso{
\code{\link{t1way}},\code{\link{t1waybt}}
}
\examples{
med1way(libido ~ dose, data = viagra)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }