\name{pvarC}
\alias{pvarC}
\title{p-variation calculation (in C++)}
\usage{
pvarC(x, p, LSI = 3L)
}
\arguments{
  \item{x}{a (non-empty) numeric vector of data values or
  an object of the class \code{pvar}.}

  \item{p}{a positive number indicating the power p in
  p-variation.}

  \item{LSI}{a length of small interval. It must be a
  positive odd number. This parameter do not have effect on
  final result, but might influence the speed of
  calculation.}
}
\value{
An object of the class \code{pvar}.
}
\description{
An internal function(written in C++) that calculates
p-variation.
}
\details{
This is a waking horse of this packages, nonetheless, users
should not call this function directly (rather use
\code{\link{pvar}}).
}
\keyword{internal}

