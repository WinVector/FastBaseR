

# https://www.r-bloggers.com/animated-plots-with-r/



grid_size = 200
d <- matrix(data = FALSE, nrow = grid_size, ncol = grid_size)
plt_size = 5*grid_size


glider_gun_txt <- "
! http://www.conwaylife.com/patterns/gosperglidergun.cells
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

mwss_txt <- "
! http://www.conwaylife.com/patterns/mwss.cells
!Name: MWSS
!Author: John Conway
!The third most common spaceship (after the glider and lightweight spaceship).
!www.conwaylife.com/wiki/index.php?title=Middleweight_spaceship
...O
.O...O
O
O....O
OOOOO
"
mwss <- read_cells(mwss_txt)

reflector_txt <- "
! http://www.conwaylife.com/patterns/pentadecathlon.cells
!Name: Pentadecathlon
!Author: John Conway
!10 cells placed in a row evolve into this object, which is the most natural oscillator of period greater than 3. In fact, it is the fifth or sixth most common oscillator overall, being about as frequent as the clock, but much less frequent than the blinker, toad, beacon or pulsar.
!www.conwaylife.com/wiki/index.php?title=Pentadecathlon
..O....O
OO.OOOO.OO
..O....O
"
reflector <- read_cells(reflector_txt)

d <- write_mat_region(d, 20, 20, glider_gun)
d <- write_mat_region(d, 80, 20, glider_gun)
d <- write_mat_region(d, 120, 20, glider_gun)
d <- write_mat_region(d, 20, grid_size - 20, mwss)
d <- write_mat_region(d, 60, grid_size - 20, mwss)
d <- write_mat_region(d, 100, grid_size - 20, mwss)
d <- write_mat_region(d, grid_size - 15, grid_size - 20, reflector)
d <- write_mat_region(d, grid_size - 50, grid_size - 50, reflector)
d <- write_mat_region(d, grid_size - 100, grid_size - 50, reflector)
d <- write_mat_region(d, grid_size - 50, grid_size - 100, reflector)


# https://stackoverflow.com/questions/28035831/how-to-build-a-crossword-like-plot-for-a-boolean-matrix

par(mar=rep(0, 4))
o <- cbind(c(row(d)), c(col(d))) - 1
o1 <- o[, 1]
o2 <- o[, 2]
o3 <- o[, 1] + 1
o4 <- o[, 2] + 1

dir.create("plts", showWarnings = FALSE)
for(i in seq_len(2000)) {
  fname <- paste0("plts/plt_", sprintf("%05.0f", i), ".png")
  png(fname, width = plt_size, height = plt_size, antialias = "none")
  plot.new()
  plot.window(xlim=c(0, ncol(d)), ylim=c(0, nrow(d)), asp=1)
  rect(o1, o2, o3, o4, col=t(d)[, ncol(d):1], border = FALSE)
  d <- life_step(d)
  dev.off()
}
setwd("plts")
plts <- sort(list.files(".", "^.*\\.png$"))
system2("convert", args = c(plts, c("-loop", "0", "../extras/ConwayLife/glider_gun2.gif")))
setwd("..")
## use ImageMagic in bash
# convert *.png -loop 0 glider_gun2.gif

