#' p-variation summation function
#'
#' It is the sum of absolute differences in the power of p.
#' 
#' This is a function that must be maximised by taking a proper subset of \code{x}, i.e. if \code{prt} is a
#' p-variation partition of sample \code{x}, then \code{Sum_p(x[prt], p) == pvar(x, p)}.
#'
#' @param x a numeric vector of data values.
#' @param p a number indicating the power in summing function.
#' @param lag a number, indicating the lag.
#' @export 
#' @return The number equal to \code{sum((abs(diff(x, lag)))^p)}
#' @seealso \code{\link{pvar}}
#' @examples
#' x = rbridge(1000)
#' pv = pvar(x, 2); pv  		
#' Sum_p(x[pv$partition], 2)	
Sum_p <- function(x, p, lag = 1) {
  sum((abs(diff(x, lag)))^p)
}


#' p-variation calculation
#' 
#' Calculates p-variation of the sample. 
#' 
#' See (in pvar-package)...
#' 
#' @param x a (non-empty) numeric vector of data values or an object of the class \code{pvar}.
#' @param p a positive number indicating the power p in p-variation. 
#' @param TimeLabel numeric, a time index of \code{x}. Used only for plotting. 
#' @param LSI a length of small interval. It must be a positive odd number. 
#' This parameter do not have effect on final rezult, 
#' but maigth influence the speed of calculation. 
#' @export
#' @return An object of the class \code{pvar}. Namely, it is a list that contains
#' \item{value}{a value of p-variation.}
#' \item{x}{a vector of original data \code{x}.}
#' \item{p}{the value of p.}
#' \item{partition}{a vector of indexes that indicates the partition that achieves the maximum.}
#' \item{dname}{a name of data vector (optional).}
#' \item{TimeLabel}{a time label of \code{x} (optional).}
#' @author Vygantas Butkus <Vygantas.Butkus@@gmail.com>
#' @seealso One of the p-variations application is realized in \code{\link{PvarBreakTest}} function. 
#' @examples
#' ### randomised data:
#' x = rbridge(1000)
#' 
#' ### the main functions:
#' pv = pvar(x, 2)
#' print(pv)
#' summary(pv)
#' plot(pv)
#'
#' ### The value of p-variation is    
#' pv; Sum_p(x[pv$partition], 2)  
#' 
#' ### The meaning of partition points:
#' pv.PP = pvar(x[pv$partition], TimeLabel=time(x)[pv$partition], 2)
#' pv.PP == pv.PP
#' op <- par(mfrow = c(2, 1))
#' plot(pv, main="pvar with original data")
#' plot(pv.PP, main="the same pvar without meaningless points")
#' par(op)
pvar <- function(x, p, TimeLabel = as.vector(time(x)), LSI = 3) {
  
  NAInd <- is.na(x)
  if (any(NAInd)) {
    warning("NA values was removed.")
    if (sum(NAInd) == length(x)) {
      stop("There are no non-NA values.")
    }
    x <- x[!NAInd]
    TimeLabel <- TimeLabel[!NAInd]
  }
  if (length(x) == 0) {
    stop("There are no non-NA values.")
  }
  
  LSI <- as.integer(LSI)
  if (LSI < 1) {
    warning("LSI must be positive odd number. LSI changed to 3")
    LSI <- 3
  }
  
  if (LSI%%2 == 0) {
    LSI <- LSI - 1
    warning("LSI must be positive odd number. LSI changed to " %.% LSI)
  }
  
  
  if (length(p) != 1) {
    if (length(p) < 1) {
      stop("The length of 'p' is zero.")
    }
    warning("The 'p' must be a scalar. Only first element is used.")
    p <- p[1]
  }
  
  if (p <= 0) {
    stop("'p' must be positive.")
  }
  
  if (length(x) < 1) {
    stop("The length of 'x' is zero.")
  }
  
  ### check taime label
  if (length(TimeLabel) != length(x)) {
    TimeLabel <- seq_along(x)
    warning("TimeLabel must have the same length as 'x'. TimeLabel changed to `seq_along(x)`")
  }
  
  
  dname <- deparse(substitute(x))
  if (length(dname) > 1) 
    dname <- paste(dname, collapse = "")
  if (nchar(dname) > 50) 
    dname <- substr(dname, 1, 50) %.% "..."
  
  
  ### p-variacion calculus
  if (p <= 1) {
    partition <- seq_along(x)
    pvar.value <- Sum_p(x, p)
    ans <- list(value = c(`p-variation` = pvar.value), p = p, dname = dname, x = as.vector(x), TimeLabel = TimeLabel, partition = partition)
    class(ans) <- "pvar"
  } else {
    ans <- pvarC(as.vector(x), p, LSI = LSI)
    ans[["dname"]] <- dname
    ans[["TimeLabel"]] <- TimeLabel
  }
  
  ### Check possible error:
  if (abs(ans$value - Sum_p(x[ans$partition], p)) > 1/10^8) {
    warning("Sorry, something wrong: The Sum_p value in partition points is not equal to p-variation value. \n            Please contact maintainer of the package.")
  }
  
  ans
}

#' @rdname pvar 
#' @param object an objct of the class \code{pvar}.
#' @param \dots further arguments.
#' @export
summary.pvar <- function(object, ...) {
  class(object) <- c("summary.pvar", "pvar")
  object
}


#' @rdname pvar 
#' @param main a \code{main} parameter in \code{plot} function.
#' @param ylab a \code{ylab} parameter in \code{plot} function.
#' @param sub a \code{sub} parameter in \code{plot} function.
#' @param col.PP the color of partition points.
#' @param cex.PP the cex of partition points.
#' @export
plot.pvar <- function(x, main = "p-variation", ylab = x$dname, sub = "p=" %.% round(x$p, 5) %.% ", p-variation: " %.% formatC(x$value, 
  5, format = "f"), col.PP = 2, cex.PP = 0.5, ...) {
  if (length(x$TimeLabel) > 0) {
    Time <- x$TimeLabel
  } else {
    Time <- time(x$x)
  }
  plot(Time, x$x, type = "l", sub = sub, ylab = ylab, main = main, ...)
  
  points(Time[x$partition], x$x[x$partition], cex = cex.PP, pch = 19, col = col.PP, bg = col.PP)
  
}

#' @method print pvar
print.pvar <- function(x, ...) {
  print(x$value)
}

#' @method print summary.pvar
print.summary.pvar <- function(x, ...) {
  cat("The summary of p-variation:\n")
  cat("Value: " %.% formatC(x$value) %.% ", p = " %.% x$p %.% "\n")
  cat("Data: " %.% x$dname %.% ", n=" %.% length(x$x) %.% "\n")
  
  if (length(x$x) > 6) {
    cat("\nData vector (n=" %.% length(x$x) %.% "): " %.% paste(formatC(head(x$x, 6)), collapse = ", ") %.% ", ...\n")
  } else {
    cat("\nData vector (n=" %.% length(x$x) %.% "): " %.% paste(formatC(head(x$x, 6)), collapse = ", ") %.% ".\n")
  }
  
  if (length(x$partition) > 6) {
    cat("Partition has " %.% length(x$partition) %.% " points: " %.% paste(formatC(head(x$partition, 6)), collapse = ", ") %.% ", ...\n")
  } else {
    cat("Partition has " %.% length(x$partition) %.% " points: " %.% paste(formatC(head(x$partition, 6)), collapse = ", ") %.% ".\n")
  }
  
}


#' Addition of p-variation
#' 
#' Merges two objects of p-variation and efectivly recalculates the p-variation of joined sample.
#' 
#' Note: a short form of \code{AddPvar(pv1, PV2} is \code{PV1 + PV2}. 
#' 
#' @param PV1 an object of the class \code{pvar}. 
#' @param PV2 an object of the class \code{pvar}, which has the same \code{p} value as PV1 object.
#' @param AddIfPossible \code{logical}. If TRUE (the default), then is is assumed, that two samples has common point. 
#' @export
#' @return An object of the class \code{pvar}. See \code{\link{pvar}}.
#' @examples
#' ### creating two pvar objects:
#' x = rwiener(1000)
#' PV1 = pvar(x[1:500], 2)
#' PV2 = pvar(x[500:1000], 2)
#' 
#' layout(matrix(c(1,3,2,3), 2, 2))
#' plot(PV1)
#' plot(PV2)
#' plot(AddPvar(PV1, PV2))
#' layout(1)
#' 
#' ### AddPvar(PV1, PV2) is eqivavalent to PV1 + PV2
#' IsEqualPvar(AddPvar(PV1, PV2), PV1 + PV2)
AddPvar <- function(PV1, PV2, AddIfPossible = TRUE) {
  if (class(PV1) != "pvar" | class(PV2) != "pvar") {
    stop("In `AddPvar` function, PV1 and PV2 must be of the class `pvar`")
  }
  if (PV1$p != PV2$p) {
    stop("Function `AddPvar` is meaningfull only with the same `p`.")
  }
  p <- PV1$p
  
  if (p > 1) {
    ans <- AddPvarC(PV1, PV2, AddIfPossible)
  } else {
    add <- AddIfPossible & (PV1$x[length(PV1$x)] == PV2$x[1])
    ans <- list()
    ans$p <- p
    if (add) {
      ans$x <- c(PV1$x, PV2$x[-1])
    } else {
      ans$x <- c(PV1$x, PV2$x)
    }
    ans$partition <- seq_along(ans$x)
    ans$value <- c(`p-variation` = Sum_p(ans$x, ans$p))
  }
  ans$TimeLabel <- time(ans$x)
  
  dnamePV1 <- deparse(substitute(PV1))
  if (nchar(dnamePV1) > 20) 
    dnamePV1 <- substr(dnamePV1, 1, 20) %.% "..."
  dnamePV2 <- deparse(substitute(PV2))
  if (nchar(dnamePV2) > 20) 
    dnamePV2 <- substr(dnamePV2, 1, 20) %.% "..."
  ans$dname <- dnamePV1 %.% " + " %.% dnamePV2
  class(ans) <- "pvar"
  
  if (abs(ans$value - Sum_p(ans$x[ans$partition], p)) > 1/10^8) {
    warning("Sorry, something wrong: The Sum_p value in partition points is not equal to p-variation value. \n            Please contact maintainer of the package.")
  }
  
  ans
}

#' @export
Ops.pvar <- function(e1, e2) {
  if (nargs() == 1) 
    stop("unary ", .Generic, " not defined for pvar objects")
  
  boolean <- switch(.Generic, `<` = , `>` = , `==` = , `!=` = , `<=` = , `>=` = TRUE, FALSE)
  if (boolean) 
    return(eval(call(.Generic, unname(e1$value), unname(e2$value))))
  
  if (.Generic == "+") 
    return(AddPvar(e1, e2))
  
  stop(.Generic, " not defined for pvar objects")
}


SafeNearComparison <- function(...) all(isTRUE(all.equal(...)))

#' Test if two `pvar` objects are eqivalnt.
#' 
#' Two \code{pvar} objects are considered to be equal 
#' if they have the same \code{x}, \code{p}, \code{value} and the same value of \code{x} 
#' in the points of \code{partition} (the index of partitions are not nesesary the same).
#' All other atributes like \code{dname} or \code{TimeLabel} are not important.
#' 
#' @param PV1 an object of the class \code{pvar}. 
#' @param PV2 an object of the class \code{pvar}. 
#' @export 
IsEqualPvar <- function(PV1, PV2) {
  
  SafeNearComparison(unname(PV1$value), unname(PV2$value)) & SafeNearComparison(as.vector(PV1$p), as.vector(PV2$p)) & SafeNearComparison(as.vector(PV1$x), 
    as.vector(PV2$x)) & SafeNearComparison(as.vector(PV1$x[PV1$partition]), as.vector(PV2$x[PV2$partition]))
  
} 
