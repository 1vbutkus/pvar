
### Sp sunction
Sum_p <- function(x, p, lag = 1) {
  sum((abs(diff(x, lag)))^p)
}


#' pvar
#' 
#' @param x asd
#' @param p asd 
#' @param TimeLabel asd 
#' @param LSI asd 
#' @export
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


#' @param \dots asd
#' @rdname pvar 
#' @export
print.pvar <- function(x, ...) {
  print(x$value)
}

#' summary.pvar
#' 
#' @param object asd
#' @param \dots asd
#' @export
summary.pvar <- function(object, ...) {
  class(object) <- c("summary.pvar", "pvar")
  object
}

#' print.summary.pvar
#' 
#' @export
#' @param x asd
#' @param \dots asd
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
    cat("With " %.% length(x$partition) %.% " meaningful points: " %.% paste(formatC(head(x$partition, 6)), collapse = ", ") %.% ", ...\n")
  } else {
    cat("With " %.% length(x$partition) %.% " meaningful points: " %.% paste(formatC(head(x$partition, 6)), collapse = ", ") %.% ".\n")
  }
  
}

#' plot.pvar 
#' 
#' @export
#' @param x asd
#' @param main asd
#' @param ylab asd
#' @param sub asd
#' @param col.PP asd
#' @param cex.PP asd
#' @param \dots asd
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


SafeRoundCompare <- function(...) all(isTRUE(all.equal(...)))

IsEqualPvar <- function(PV1, PV2) {
  
  SafeRoundCompare(unname(PV1$value), unname(PV2$value)) & SafeRoundCompare(as.vector(PV1$p), as.vector(PV2$p)) & SafeRoundCompare(as.vector(PV1$x), 
    as.vector(PV2$x)) & SafeRoundCompare(as.vector(PV1$partition), as.vector(PV2$partition))
  
} 
