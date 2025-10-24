samp_dist_from_x <- function(x, n, B = 1e3, replace = FALSE, FUN = mean, ...){
  res <- vector(mode = "numeric", length = B)
  for(i in seq_along(res)){
    xs <- sample(x, n, replace = replace)
    res[i] <- FUN(xs, ...)
  }
  return(res)
}

samp_dist_from_dist <- function(dist, n, B = 1e3, dist.args = NULL, FUN = mean, ...){
  if(is.null(dist.args)){
    dist.args <- list(n = n)
  } else{
    dist.args$n <- n
  }

  res <- vector(mode = "numeric", length = B)
  for(i in 1:B){
    xs <- do.call(dist, dist.args)
    res[i] <- FUN(xs, ...)
  }
  return(res)
}

#' @export
samp_dist <- function(n, x = NULL, dist = NULL, dist.args = NULL, FUN = mean, replace = FALSE, B = 1e3, ...){
  if(is.null(x) & is.null(dist)){
    stop("x or dist need to be provided!")
  }

  if(!is.null(x)){
    res <- samp_dist_from_x(x = x, n = n, B = B, replace = replace, FUN = FUN, ...)
  } else{
    res <- samp_dist_from_dist(dist = dist, n = n, B = B, dist.args = dist.args, FUN = FUN, ...)
  }

  attr(res, "n") <- n
  attr(res, "B") <- B
  if(!is.null(dist)){
    attr(res, "dist") <- as.character(substitute(dist))
    if(is.null(dist.args)){
      attr(res, "dist.args") <- .get_non_empty_formals(dist)
    } else{
      attr(res, "dist.args") <- dist.args
    }
    attr(res, "call") <- .call2string(attr(res, "dist"), attr(res, "dist.args"))
  } else{
    attr(res, "call") <- sprintf("x (mean = %.3f, sd = %.3f, n = %.0f)", mean(x), sd(x), length(x))
  }
  attr(res, "FUN") <- as.character(substitute(FUN))

  class(res) <- c("sdist", class(res))
  return(res)
}

#' @method plot sdist
#' @export
plot.sdist <- function(x){
  hist(x, 
       breaks = 50, 
       main = sprintf("Sampling Distribution\n(n = %s)", attr(x, "n")), 
       xlab = "x", 
       col = "dodgerblue")
}

#' @method print sdist
#' @export
print.sdist <- function(x){
  xs <- x
  attributes(xs) <- NULL
  print(xs)
  invisible(x)
}

#' @method summary sdist
#' @export

summary.sdist <- function(x){
  cat("Sampling distribution summary", "\n")
  cat(sprintf("From ~ %s", attr(x, "call")), "\n")
  cat("\n")
  cat(
    sprintf("n = %.0f\nmean = %.3f\nse = %.3f", attr(x, "n"), mean(x), sd(x))
  )
}

.get_non_empty_formals <- function(x){
  ff <- formals(x)
  ff[!sapply(ff, inherits, "name")]
}

.call2string <- function(FUN, args){
  arg_strings <- mapply(function(name, value) {
    paste0(name, 
           " = ", 
           paste(deparse(value), collapse = ""))
  }, names(args), args)
  paste0(FUN, "(", paste(arg_strings, collapse = ", "), ")")
}
