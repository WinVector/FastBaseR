

#' Forward step a matrix one iteration of Conway's come of life.
#'
#' Assumes zero-padded on all sides.
#'
#' @param d a matrix of logical values
#' @return next step matrix
#'
#' @seealso \code{\link{read_cells}}, \code{\link{write_mat_region}}
#'
#'
#' @examples
#'
#' d <- matrix(data = FALSE, nrow = 10, ncol = 10)
#' glider_txt <- "
#' .O
#' ..O
#' OOO
#' "
#' glider <- read_cells(glider_txt)
#' d <- write_mat_region(d, 5, 5, glider)
#' life_step(d)
#'
#'
#' @export
#'
life_step <- function(d) {
  # form the neighboring sums
  nrow <- dim(d)[[1]]
  ncol <- dim(d)[[2]]
  d_eu <- rbind(d[-1, , drop = FALSE], 0)
  d_ed <- rbind(0, d[-nrow, , drop = FALSE])
  d_le <- cbind(d[ , -1, drop = FALSE], 0)
  d_re <- cbind(0, d[ , -ncol, drop = FALSE])
  d_lu <- cbind(d_eu[ , -1, drop = FALSE], 0)
  d_ru <- cbind(0, d_eu[ , -ncol, drop = FALSE])
  d_ld <- cbind(d_ed[ , -1, drop = FALSE], 0)
  d_rd <- cbind(0, d_ed[ , -ncol, drop = FALSE])
  pop <- d_eu + d_ed + d_le + d_re + d_lu + d_ru + d_ld + d_rd
  d <- (pop==3) | (d & (pop>=2) & (pop<=3))
  d
}


#' Read life text file format.
#'
#' Format is: rows of non-empty lines of . and O with ! headers
#'
#' @param txt incoming text as a single string
#' @return matrix
#'
#' @seealso \code{\link{life_step}}, \code{\link{write_mat_region}}
#'
#' @examples
#'
#' glider <- "
#' .O
#' ..O
#' OOO
#' "
#' read_cells(glider)
#'
#' @export
#'
read_cells <- function(txt) {
  lines <- strsplit(txt, "[\r\n]+")[[1]]
  lines <- trimws(lines, which = "both")
  lines <- lines[nchar(lines)>0]
  lines <- lines[!grepl("^!", lines)]
  nrow <- length(lines)
  ncol <- max(nchar(lines))
  d <- matrix(FALSE, nrow = nrow, ncol = ncol)
  for(i in seq_len(nrow)) {
    li <- lines[[i]]
    for(j in seq_len(nchar(li))) {
      cij <- substr(li, j, j)
      if(cij=="O") {
        d[i,j] <- TRUE
      }
    }
  }
  d
}

#' Write a region of a life board.
#'
#' @param d life board matrix
#' @param i row to start writing
#' @param j column to start writing
#' @param p pattern matrix
#'
#' @seealso \code{\link{life_step}}, \code{\link{read_cells}}
#'
#' @export
#'
write_mat_region <- function(d, i, j, p) {
  nrow <- dim(p)[[1]]
  ncol <- dim(p)[[2]]
  for(pi in seq_len(nrow)) {
    for(pj in seq_len(ncol)) {
      d[i + pi - 1, j + pj -1] <- p[pi, pj]
    }
  }
  d
}

