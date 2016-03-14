#' Perform Python-like list comprehensions in R
#' 
#' Evaulates \code{expr} for each value passed in keyword args, as in \code{foreach}
#'
#' @param expr The expression to evaluate
#' @param ... The keyword arguments to be passed to \code{foreach}
#' @param condition An optional condition to evaluate before returning a result for each item
#' @param .combine A combine function to be passed to \code{foreach}
#'
#' @return A vector of out values.
#' @export
#'
#' @examples
#' cormatrix <- structure(list(As = c(NA, 0.597861790755909, NA, NA, NA, NA, 
#' 1, NA, NA, NA), Pb = c(NA, NA, 0.756832335863065, NA, NA, NA, 
#' NA, NA, NA, NA), Zn = c(NA, 0.581426445499464, NA, NA, 0.562975973699551, 
#' NA, 0.589760572528395, NA, NA, NA), V = c(NA, NA, NA, NA, -0.558938721862234, 
#' NA, NA, NA, 0.796385319562529, NA), Cr = c(NA, NA, 0.815492040792347, 
#' NA, NA, NA, NA, NA, NA, NA)), .Names = c("As", "Pb", "Zn", "V", 
#' "Cr"), row.names = c("S", "Cl", "Ti", "Mn", "Fe", "Co", "As", 
#' "Mo", "Ag", "Sn"), class = "data.frame")
#' 
#' lc(!all(is.na(cormatrix[i,])), i=1:nrow(cormatrix))
#' 
lc <- function(expr, ..., condition, .combine=c) {
  `%do%` <- foreach::`%do%`
  expr <- deparse(substitute(expr))
  if(missing(condition)) {
    foreach::foreach(..., .combine=.combine) %do% {
      eval(parse(text=expr))
    }
  } else {
    condition <- deparse(substitute(condition))
    foreach::foreach(..., .combine=.combine) %do% {
      if(eval(parse(text=condition))) {
        eval(parse(text=expr))
      }
    }
  }
}
