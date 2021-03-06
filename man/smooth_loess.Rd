% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/09-smooth.R
\name{smooth_loess}
\alias{smooth_loess}
\title{LOWESS smoother}
\usage{
smooth_loess(x, ...)
}
\arguments{
\item{x}{\verb{[univariate vector]}

Univariate vector, numeric or ts object with only one dimension.}

\item{...}{Further arguments passed to \code{\link[stats:lowess]{lowess}}}
}
\description{
\ifelse{html}{\out{<a href='https://www.tidyverse.org/lifecycle/#experimental'><img src='figures/lifecycle-experimental.svg' alt='Experimental lifecycle'></a>}}{\strong{Experimental}}

Locally-weighted polynomial regression.
}
\examples{

x <- co2
plotx(smooth_loess(x))
}
