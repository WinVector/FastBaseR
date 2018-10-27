

# https://www.r-bloggers.com/animated-plots-with-r/



d <- matrix(data = FALSE, nrow = 100, ncol = 100)


glider_gun_txt <- "
!http://www.conwaylife.com/patterns/gosperglidergun.cells
!Name: Gosper glider gun
!Author: Bill Gosper
!The first known gun and the first known finite pattern with unbounded growth.
!www.conwaylife.com/wiki/index.php?title=Gosper_glider_gun
........................O
......................O.O
............OO......OO............OO
...........O...O....OO............OO
OO........O.....O...OO
OO........O...O.OO....O.O
..........O.....O.......O
...........O...O
............OO"


glider_gun <- read_cells(glider_gun_txt)
d <- write_mat_region(d, 20, 20, glider_gun)


# https://stackoverflow.com/questions/28035831/how-to-build-a-crossword-like-plot-for-a-boolean-matrix

par(mar=rep(0, 4))
o <- cbind(c(row(d)), c(col(d))) - 1
o1 <- o[, 1]
o2 <- o[, 2]
o3 <- o[, 1] + 1
o4 <- o[, 2] + 1

for(i in seq_len(1000)) {
  fname <- paste0("plts/plt_", sprintf("%05.0f", i), ".png")
  png(fname)
  plot.new()
  plot.window(xlim=c(0, ncol(d)), ylim=c(0, nrow(d)), asp=1)
  rect(o1, o2, o3, o4, col=t(d)[, ncol(d):1], border = FALSE)
  d <- life_step(d)
  dev.off()
}

## use ImageMagic in bash
# convert *.png -loop 0 glider_gun.gif

