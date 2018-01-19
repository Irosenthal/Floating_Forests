library(rgeos)
library(sp)
library(raster)
library(rgdal)


california <- readOGR("../../data/coastlines/California", layer="California")
tasmania <- readOGR("../../data/coastlines/tasmania", layer="Tasmania")
falklands <- readOGR("../../data/coastlines/falklands", layer="Falklands")

plotCoast <- function(acoast, bg = "lightblue", col = "brown", ...){
  plot(acoast,  bg=bg, col=col, ...)
}

jpeg("../../figures/coastlines/california.jpg")
plotCoast(california, col = "tan")
dev.off()


jpeg("../../figures/coastlines/tasmania.jpg")
plotCoast(tasmania, col = "tan")
dev.off()


jpeg("../../figures/coastlines/falklands.jpg")
plotCoast(falklands, col = "tan")
dev.off()