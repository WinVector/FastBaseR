

#' Compute per-group cumulative maxs
#'
#' @param col vector to cumulative max
#' @param first_indices numeric index of first index in each group
#' @return per-group cumulative max
#'
#' @examples
#'
#' v <- c(2, 3, 1, 7, 5)
#' fi <- c(1, 1, 3, 3, 3)
#' cummax_g(v, fi)
#'
#' @export
#'
cummax_g <- function(col, first_indices) {
  na_ind <- is.na(col)
  if(all(na_ind)) {
    return(col)
  }
  minv <- min(col, na.rm = TRUE) - 1
  col[na_ind] <- minv
  n <- length(col)
  dels <- c(col[1], pmax(col[-1] - col[-n], 0))
  running_max <- cumsum(dels)
  offset <- col - running_max
  running_max <- running_max  + offset[first_indices]
  running_max[running_max<minv] <- NA
  running_max
}



#' Compute per-group maxs
#'
#' @param col vector to  max
#' @param first_indices numeric index of first index in each group
#' @param na.rm logical if TRUE remove NAs
#' @return per-group max
#'
#' @examples
#'
#' v <- c(2, 3, 1, 7, 5)
#' fi <- c(1, 1, 3, 3, 3)
#' max_g(v, fi)
#'
#' @export
#'
max_g <- function(col, first_indices, na.rm = FALSE) {
  na_ind <- is.na(col)
  if(all(na_ind)) {
    return(col)
  }
  cmx <- cummax_g(col, first_indices)
  last_indices <- last_in_each_group(first_indices)
  res <- cmx[last_indices]
  if(!na.rm) {
    res[group_na_mask(col)] <- NA
  }
  res
}


#' Compute per-group arg-max
#'
#' @param col vector to  max
#' @param first_indices numeric index of first index in each group
#' @return per-group arg-max
#'
#' @examples
#'
#' v <- c(2, 3, 1, 7, 5)
#' fi <- c(1, 1, 3, 3, 3)
#' argmax_g(v, fi)
#'
#' @export
#'
argmax_g <- function(col, first_indices) {
  na_ind <- is.na(col)
  if(all(na_ind)) {
    return(col)
  }
  mx <- max_g(col, first_indices, na.rm = TRUE)
  n <- length(col)
  want <- ifelse(is.na(col), -(n+2), ifelse(col>=mx, -seq_len(n), -(n+1)))
  mw <- cummax_g(want, first_indices)
  mw[mw< (-n)] <- NA
  last_indices <- last_in_each_group(first_indices)
  -mw[last_indices]
}
