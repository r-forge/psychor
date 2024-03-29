\name{plot.anacor}
\alias{plot.anacor}
\alias{screeplot.anacor}
\alias{screeplot}

\title{Plots for anacor solution}
\description{These functions produce various plots for objects of class \code{"anacor"} 
}
\usage{
\method{plot}{anacor}(x, plot.type = "jointplot", plot.dim = c(1,2), col.row = "cadetblue", 
col.column = "coral1", catlabels = list(label.row = TRUE, label.col = TRUE, 
col.row = "cadetblue", col.column = "coral1", cex = 0.8, pos = 3),
legpos = "top", arrows = c(FALSE, FALSE), conf = 0.95, wlines = 0, asp = 1, pch = 20, 
xlab, ylab, main, type, xlim, ylim, cex.axis2, ...)

}

\arguments{
  \item{x}{Object of class \code{"anacor"}}
  \item{plot.type}{Type of plot to be produced (details see below): 2-D and 3-D for \code{"jointplot"}, \code{"rowplot"}, and \code{"colplot"}; 2-D for \code{"regplot"}, \code{"graphplot"}, \code{"benzplot"}, \code{"transplot"}, and \code{"orddiag"}.}
  \item{plot.dim}{Vector of length 2 with Dimensions to be plotted. For \code{"regplot"} a single value should be provided, for  \code{"transplot"} more than two dimensions are allowed, and for \code{"benzplot"} this argument is ignored.}
  \item{col.row}{Color row categories}
  \item{col.column}{Color column categories}
  \item{catlabels}{Various parameter settings for labels}
  \item{legpos}{Position of the legend (for \code{"transplot"} only)}
  \item{conf}{Ellipsoid confidence level for \code{"jointplot"}, \code{"rowplot"}, and \code{"colplot"}, assuming that the ellipse where computed in \code{anacor()}. If \code{NULL}, no ellipsoids are drawn.}
  \item{arrows}{Whether arrows from the origin to the row scores (first element) or column scores (second element) should be drawn.}
  \item{wlines}{For \code{"graphplot"} only: If 0, all lines are of the same thickness. For values > 0 line thickness indicates the strength of the pull}
  \item{asp}{Aspect ratio.}
  \item{pch}{Symbol for plotting points.}
  \item{xlab}{Label x-axis.}
  \item{ylab}{Label y-axis.}
  \item{xlim}{Scale x-axis.}
  \item{ylim}{Scale y-axis.}
  \item{main}{Plot title.}
  \item{type}{Whether points, lines or both should be plotted; for \code{"regplot"} and \code{"transplot"} only.}
  \item{cex.axis2}{For \code{"regplot"} only. The magnification to be used for the category labels in the scaled solution relative to the current setting of cex.}
  \item{\dots}{Additional graphical parameters.}
}
\details{
The following plot types are provided: \code{"jointplot"} plots row and column scores into the same device, \code{"rowplot"} and \code{"colplot"} plot the row scores and column scores, respectively, in separate devices. For these types of plots 3-dimensional versions are provided. The graph plot is an unlabeled version of the joint plot where the points are connected by lines. Options are provided (\code{wlines}) to steer the line thickness indicating the connection strength.

The regression plot (\code{"regplot"}) provides two plots. 
First, the unscaled solution is plotted. A frequency grid for the row 
categories (x-axis) and column categories (y-axis) is produced. 
The regression line is based on the category weighted means of the relative frequencies:  
the blue line on the column-wise means on the x-axis and the column category on the y-axis, 
the red line is based on the row categories on the x-axis and the row-wise means on the y-axis. 
In a second device the scaled solution is plotted. The frequency grid is determined by 
the row scores (x-axis) and the column scores(y-axis). 
Now, instead of the row/column categories, the column scores (black line y-axis) 
and the row scores (red line x-axis) are used.

The transformation plot (\code{"transplot"}) plots the row/column categories against the row/column scores. The Benzecri plot (\code{"benzplot"}) plots the observed distances against the fitted distances. It is assumed that the CA result is Benzecri scaled. The ordination diagram (\code{"orddiag"}) for CCA produces a joint plot and includes the column and row covariates based on intraset correlations. 
}

\references{De Leeuw, J. and Mair, P. (2009). Simple and Canonical Correspondence Analysis Using the R Package anacor. Journal of Statistical Software, 31(5), 1-18. \url{https://www.jstatsoft.org/v31/i05/}}
\author{Jan de Leeuw, Patrick Mair}

\seealso{\code{\link{anacor}}}

\examples{

## symmetric map
data(tocher)
res <- anacor(tocher)
plot(res, conf = NULL, main = "Symmetric Map")

## simple CA on Tocher data, asymmetric coordinates
res <- anacor(tocher, scaling = c("standard", "Benzecri"))
res

## Regression plots using Glass data
data(glass)
res <- anacor(glass)
plot(res, plot.type = "regplot", xlab = "fathers occupation", ylab = "sons occupation")


## Benzecri Plots for bitterling data
data(bitterling)
res1 <- anacor(bitterling, ndim = 2, scaling = c("Benzecri", "Benzecri"))
res2 <- anacor(bitterling, ndim = 5, scaling = c("Benzecri", "Benzecri"))
res2
plot(res1, plot.type = "benzplot", main = "Benzecri Distances (2D)")
plot(res2, plot.type = "benzplot", main = "Benzecri Distances (5D)")

## Column score plot,transformation plot, and ordination diagram for canonical CA
data(maxwell)
res <- anacor(maxwell$table, row.covariates = maxwell$row.covariates, 
scaling = c("Goodman", "Goodman"))
res
plot(res, plot.type = "colplot", xlim = c(-1.5,1), conf = NULL)
plot(res, plot.type = "transplot", legpos = "topright")
plot(res, plot.type = "orddiag")
}

\keyword{hplot}

