#' Clean Duplicates in Dataframes
#'
#' function to merge duplicate rows in a data.frame based on a set of id fields.
#' other fields are concatenated together using paste(x, collapse="| "). This allows
#' for cleaning based on all available information.
#'
#' @param df Data frame containing duplicates
#' @param idfields The fields that should be unique
#' @param sep The separator to pass to \code{paste()} when concatenating duplicate
#'   fields together
#'
#' @return The resulting data frame.
#' @export
#'
#' @examples
#' df <- data.frame(x=c(1,2,3,4,1,2,3,4), 
#'                  y=c(1,2,3,4,1,2,3,4), 
#'                  label=c("one", "two", "three", "four", 
#'                          "five", "six", "seven", "eight"))
#' df
#' mergeduplicates(df, idfields=c("x", "y"))
#' 
#' 
mergeduplicates <- function(df, idfields, sep="|") {
  foreach <- foreach::foreach
  `%do%` <- foreach::`%do%`
  
  uniqueids <- unique(df[idfields])
  
  foreach(i=1:nrow(uniqueids), .combine=rbind) %do% {
    ids = uniqueids[i, ]
    indexfilter <- rep(TRUE, nrow(df))
    for(id in idfields) {
      indexfilter <- indexfilter & (df[[id]]==ids[[id]])
    }
    duplicaterows <- df[indexfilter,]
    
    rowaslist <- list()
    
    for(column in names(duplicaterows)) {
      if(!(column %in% idfields)) {
        value <- paste(unique(duplicaterows[[column]]), collapse=sep)
      } else {
        value <- duplicaterows[[column]][1] #they should all be the same
      }
      rowaslist[[column]] <- value
    }
    
    data.frame(rowaslist)
  }
}
