set.seed(732017)
mt <- circleUnif(15) + 1
mt <- rbind(mt, matrix(runif(8,0,2), ncol = 2) )
createPlot(mt, diam = .1, triangles = TRUE)
temp <- ripsDiag(mt, maxdimension = 1, maxscale = 2)$diagram
diams <- seq(0,1.5, .025)
oopt = ani.options(interval = .25)
saveGIF({for (i in diams) {
  g<-createPlot(mt, diam = i, coordx = c(-1, 3), coordy = c(-1,3), triangles = TRUE, showPlot = FALSE, roundDiameter  = 1)
  print(g)
} }, movie.name="Rips.gif",ani.width = 840, ani.height =450)

g1 <- createPlot(mt, diam = .1, triangles = TRUE)
g2 <- createPlot(mt, diam = 0.8, triangles = TRUE)
g3 <- createPlot(mt, diam = 1, triangles = TRUE)
g4 <- createPlot(mt, diam = 1.3, triangles = TRUE)
#library(cowplot)
p <- plot_grid(g1, g2, g3, g4) 
title <- ggdraw() + draw_label("Rips Simplices")
ppp <- plot_grid(title, p, ncol = 1, rel_heights = c(0.1,1))
save_plot(filename = "springer_template/springer_template_files/figure-latex/rips_circle.pdf",plot = ppp)

createPlot(mt, diam = 1.3, triangles = TRUE)

plot(mt)
library(TDA)
a <- ripsDiag(X = mt,
         maxdimension = 1,
         maxscale = 5,
         library = "GUDHI",
         printProgress = FALSE) 
plot(a$diagram)

knitr::kable(as.data.frame(a$diagram[1:20, 1:3]), digits = 2, format = "latex")

