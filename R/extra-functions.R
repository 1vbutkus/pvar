
#' str
#' 
#' @param x asd
#' @param y asd 
#' @export
`%.%` <- function(x, y) paste(x, y, sep = "")



######### Generating functions ######### Brownian bridege

#' rwiener
#' 
#' @param frequency asd 
#' @param end asd
#' @export
rwiener <- function(frequency = 1000, end = 1) {
  z <- c(0, cumsum(rnorm(end * frequency)/sqrt(frequency)))
  ts(z, start = 0, end = 1, frequency = frequency)
}


#' rbridge
#' 
#' nuoroda i e10...
#' @param frequency asd 
#' @param end asd
#' @export
rbridge <- function(frequency = 1000, end = 1) {
  z <- rwiener(frequency = frequency, end = end)
  ts(z - time(z) * as.vector(z)[frequency], start = 0, frequency = frequency)
}

#' rcumbin
#' 
#' @param frequency asd 
#' @param end asd
#' @param prob sdf
#' @export
rcumbin <- function(frequency = 1000, end = 1, prob = 0.5) {
  z <- c(0, cumsum(rbinom(frequency, 1, prob = prob) * 2 - 1))
  ts(z, start = 0, end = 1, frequency = frequency)
} 
