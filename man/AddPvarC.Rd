\name{AddPvarC}
\alias{AddPvarC}
\title{Addition of p-variation (in C++)}
\usage{
AddPvarC(PV1, PV2, AddIfPossible = TRUE)
}
\arguments{
  \item{PV1}{an object of the class \code{pvar}.}

  \item{PV2}{an object of the class \code{pvar}, which has
  the same \code{p} value as PV1 object.}

  \item{AddIfPossible}{\code{logical}. If TRUE (the
  default), then is is assumed, that two samples has common
  point.}
}
\value{
An object of the class \code{pvar}.
}
\description{
An internal function(written in C++) that merges two
objects of pvar and efectivly recalculates the p-variation
of joined sample.
}
\details{
This is an internal function, therfore, ussers should not
call this sunction directly (rathfer use
\code{\link{AddPvar}}).
}
\keyword{internal}

