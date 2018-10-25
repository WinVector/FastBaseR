

#' Mark the first row in each group of rows.
#'
#' @param data a data.frame ordered with respect to groups
#' @param group_cols character names of the grouping columns
#' @return numeric vector with index of first row in each group
#'
#' @examples
#'
#' d <- wrapr::build_frame(
#'     "g", "v" |
#'     1  , 1L  |
#'     0  , 2L  |
#'     0  , 3L  |
#'     1  , 4L  |
#'     1  , 5L  )
#'
#' d <- d[wrapr::orderv(d), , drop = FALSE]
#' d$fi <- mark_first_in_each_group(d, "g")
#'
#' print(d)
#'
#' @export
#'
mark_first_in_each_group <- function(data, group_cols) {
  n <- nrow(data)
  if(n<=1) {
    if(n<=0) {
      return(numeric(0))
    }
    return(1)
  }
  group_start_index <- c(TRUE, logical(n-1))
  for(ci in group_cols) {
    v1 <- data[[ci]][-1]
    na1 <- is.na(v1)
    v2 <- data[[ci]][-n]
    na2 <- is.na(v2)
    group_start_index <- group_start_index |
      c(TRUE, ifelse(na1 | na2, na1 != na2, v1 != v2))
  }
  group_start_index <- ifelse(group_start_index, seq_len(n), 0)
  group_start_index <- cummax(group_start_index)
  group_start_index
}

#' Build the indexes of last indexes for each group
#'
#' @param first_indices first index of each item (in order).
#' @return vector of last indices
#'
#' @examples
#'
#' fi <- c(1, 1, 3, 3, 3)
#' last_in_each_group(fi)
#'
#' @export
#'
#'
last_in_each_group <- function(first_indices) {
  n <- length(first_indices)
  firsts <- sort(unique(first_indices))
  mp <- numeric(n)
  mp[firsts] <- c(firsts[-1]-1, n)
  mp[first_indices]
}
