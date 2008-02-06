\name{smacofIndDiff}
\alias{smacofIndDiff}

\title{smacof for Individual Differences}
\description{Performs smacof for individual differences also known as Three-Way smacof on a list of dissimilarity matrices. Various restrictions decompositions and restrictions on the weight matrix are provided.
}

\usage{
smacofIndDiff(delta, ndim = 2, weightmat = NULL, init = NULL, metric = TRUE,
              ties = "primary", constraint = NULL, verbose = FALSE, modulus = 1,
              itmax = 100, eps = 1e-6)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{delta}{A list of dissimilarity matrices or a list objects of class \code{dist}}
  \item{ndim}{Number of dimensions}
  \item{weightmat}{Optional matrix with dissimilarity weights}
  \item{init}{Matrix with starting values for configurations (optional)}
  \item{metric}{If \code{FALSE} non-metric MDS is performed}
  \item{ties}{Tie specification for non-metric MDS}
  \item{constraint}{If \code{NULL}, INDSCAL is performed, \code{"idioscal"} uses the Carrol-Chang decomposition, \code{"diagonal"} the Tucker-Harshman decomposition, and \code{"identity"} Schoenemann's identity restriction.} 
  \item{verbose}{If \code{TRUE}, intermediate stress is printed out}
  \item{modulus}{Number of smacof iterations per monotone regression call}
  \item{itmax}{Maximum number of iterations}
  \item{eps}{Convergence criterion}
}
\details{
to be added
}
\value{
  \item{obsdiss}{List of observed dissimilarities, normalized}
  \item{confdiss}{List of configuration dissimilarities}
  \item{conf}{List of matrices of final configurations}
  \item{stress.m}{stress value for metric MDS}
  \item{stress.nm}{stress value for non-metric MDS (if computed)}
  \item{stress.uc}{Unconstrained stress value}
  \item{stress.co}{Constrained stress value}
  \item{ndim}{Number of dimensions}
  \item{model}{Type of smacof model}
  \item{niter}{Number of iterations}
  \item{nobj}{Number of objects}

}
\references{de Leeuw, J. \& Mair, P. (2008). Multidimensional scaling using majorization: The R package smacof.}
\author{Jan de Leeuw and Patrick Mair}

\seealso{to be added}
\examples{
data(perception)
res <- smacofIndDiff(perception)
res
summary(res)
}

\keyword{models}