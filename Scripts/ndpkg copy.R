ndpkg <- function(p){
  if(!is.element(p,installed.packages()[,1]))
  {install.packages(p, dep = T)}
  require(p,character.only = T)
}
sapply(c("ggplot2", "raster", "sp","reshape2"), ndpkg)
