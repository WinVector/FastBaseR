
#' Compute per-group cumulative sums
#'
#' @param col vector to cumulative sum
#' @param first_indices numeric index of first index in each group
#' @return per-group cumulative sum
#'
#' @examples
#'
#' v <- c(2, 3, 1, 4, 5)
#' fi <- c(1, 1, 3, 3, 3)
#' cumsum_g_no_NA(v, fi)
#'
#' @export
#'
cumsum_g_no_NA <- function(col, first_indices) {
  if(any(is.na(col))) {
    stop("FastBaseR::cumsum_g_no_NA saw NA")
  }
  running_sum <- cumsum(col)
  offset <- col - running_sum
  running_sum <- running_sum  + offset[first_indices]
  running_sum
}

# forward grouped NA mask
# FastBaseR:::cum_na_mask(c(2, 3, 1, NA, 5), c(1, 1, 3, 3, 3))
cum_na_mask <- function(col, first_indices) {
  na_ind <- is.na(col)
  if(!any(na_ind)) {
    logical(length(col))
  }
  if(all(na_ind)) {
    return(rep(TRUE, length(col)))
  }
  mask <- numeric(length(col))
  mask[na_ind] <- 1
  mask <- cumsum_g_no_NA(mask, first_indices)
  mask>0.5
}


#' Compute per-group cumulative sums
#'
#' @param col vector to cumulative sum
#' @param first_indices numeric index of first index in each group
#' @param na.rm locial if TRUE remove NAs (empty sum is zero).
#' @return per-group cumulative sum
#'
#' @examples
#'
#' v <- c(2, 3, 1, NA, 5)
#' fi <- c(1, 1, 3, 3, 3)
#' cumsum_g(v, fi)
#'
#' @export
#'
cumsum_g <- function(col, first_indices, na.rm = FALSE) {
  na_ind <- is.na(col)
  if(!any(na_ind)) {
    return(cumsum_g_no_NA(col, first_indices))
  }
  if(all(na_ind)) {
    if(na.rm) {
      return(numeric(length(col)))
    }
    return(col)
  }
  mask <- NULL
  if(!na.rm) {
    mask <- cum_na_mask(col, first_indices)
  }
  col[na_ind] <- 0
  running_sum <- cumsum_g_no_NA(col, first_indices)
  if(!na.rm) {
    running_sum[mask] <- NA
  }
  running_sum
}

# grouped NA mask
# FastBaseR:::group_na_mask(c(2, 3, 1, NA, 5), c(1, 1, 3, 3, 3))
group_na_mask <- function(col, first_indices) {
  mask1 <- cum_na_mask(col, first_indices)
  last_indices <- last_in_each_group(first_indices)
  mask1[last_indices]
}


#' Compute per-group  sums
#'
#' @param col vector to  sum
#' @param first_indices numeric index of first index in each group
#' @param na.rm locial if TRUE remove NAs
#' @return per-group sum
#'
#' @examples
#'
#' v <- c(2, 3, 1, NA, 5)
#' fi <- c(1, 1, 3, 3, 3)
#' sum_g(v, fi)
#'
#' @export
#'
sum_g <- function(col, first_indices, na.rm = FALSE) {
  na_ind <- is.na(col)
  if(all(na_ind)) {
    if(na.rm) {
      return(numeric(length(col)))
    }
    return(col)
  }
  mask <- NULL
  if(!na.rm) {
    mask <- group_na_mask(col, first_indices)
  }
  cs <- cumsum_g(col, first_indices, na.rm = TRUE)
  last_indices <- last_in_each_group(first_indices)
  res <- cs[last_indices]
  if(!na.rm) {
    res[mask] <- NA
  }
  res
}

