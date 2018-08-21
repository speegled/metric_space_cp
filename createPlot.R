createPlot <-  function(centers, diam, showPlot = TRUE, coordx, coordy, triangles = FALSE, roundDiameter = 0, mag = 1) {
  
  if(!is.data.frame(centers)) {
    centers <- as.data.frame(centers)
    names(centers) <- c("x", "y")
  }
  
  names(centers) <- c("x", "y")
  
  if(missing(coordx)) coordx <- c(min(centers$x) - diam, max(centers$x) + diam) 
  if(missing(coordy)) coordy <- c(min(centers$y - diam), max(centers$y) + diam) 
  
  connectByLine <- data.frame(x = numeric(0), y = numeric(0), xend = numeric(0), yend = numeric(0))
  
  distances <- as.matrix(dist(centers))
  for(i in 2:nrow(centers)) 
    for(j in 1: (i - 1)) {
      if(distances[i,j] < diam + .00001)
        connectByLine <- rbind(connectByLine, data.frame(x = centers$x[i], y = centers$y[i], xend = centers$x[j], yend = centers$y[j]))
    }
  
  
  myCircles <- data.frame(x <- numeric(0), y = numeric(0), group = integer(0))
  for(i in 1:nrow(centers)) {
    angle <- seq(-pi, pi, length = 50)
    myCircles <- rbind(myCircles, data.frame(x = centers$x[i] + diam/2 * sin(angle), y = centers$y[i] + diam/2 * cos(angle), group = i))
  }
  
  connectTriangle <- data.frame(x1 = numeric(0), x2 = numeric(0), x3 = numeric(0), y1 = numeric(0), y2 = numeric(0), y3 = numeric(0), group = numeric(0))
  curGroup <- 1
  if(triangles) {
  for(i in 2:nrow(centers)) {
    for(j in 1:(i - 1)) 
      for(k in 1:nrow(centers))
        if(i!= j && i!= k && j!= k && distances[i,j] < diam + .000001 && distances[j,k] < diam + .000001 && distances[i,k] < diam + .000001) {
          connectTriangle <- rbind(connectTriangle, data.frame(x = centers$x[i], y = centers$y[i], group = curGroup), 
                                   data.frame(x = centers$x[j], y = centers$y[j], group = curGroup),
                                   data.frame(x = centers$x[k], y = centers$y[k], group = curGroup))
          curGroup <- curGroup + 1
        }
          
  }
  }
  
  connect4 <- data.frame(x1 = numeric(0), x2 = numeric(0), x3 = numeric(0), x4 = numeric(0), y1 = numeric(0), y2 = numeric(0), y3 = numeric(0), y4 = numeric(0), group = numeric(0))
  curGroup4 <- 1
  if(FALSE) {
    for(i in 2:nrow(centers)) {
      for(j in 1:(i - 1)) 
        for(k in 1:nrow(centers)) 
          for(l in 1:nrow(centers)) 
            if(i!= j && i!= k && j!= k && l!= i && l != j && l!= k && 
               distances[i,j] < diam + .000001 && distances[j,k] < diam + .000001 && distances[i,k] < diam + .000001 &&
               distances[i,l] < diam + .000001 && distances[j,l] < diam + .000001 && distances[l,k] < diam + .000001) {
                 connect4 <- rbind(connectTriangle, data.frame(x = centers$x[i], y = centers$y[i], group = curGroup4), 
                                     data.frame(x = centers$x[j], y = centers$y[j], group = curGroup4),
                                     data.frame(x = centers$x[k], y = centers$y[k], group = curGroup4),
                                     data.frame(x = centers$x[l], y = centers$y[l], group = curGroup4))
              curGroup <- curGroup + 1
          }
      
    }
  }
  
  
  
  if(roundDiameter) {
    diam <-round(10 ** (roundDiameter) * diam + .00000001) * 10 ** (-1 * roundDiameter)
  }
 
  if(mag != 1) {
  if(curGroup > 1) {
  g <- ggplot(centers, aes(x = x, y = y)) + geom_point(mapping = aes(size = mag)) + 
    geom_path(data = myCircles, mapping = aes(x = x, y = y, group = group, size = mag/5)) + 
    geom_polygon(data = connectTriangle, aes(x = x, y = y, group = group, fill = group, alpha = 0.5, size = mag)) + 
    geom_segment(data = connectByLine, aes(x = x, y = y, xend = xend, yend = yend, size = rep(mag, length(connectByLine$x)))) +
    coord_cartesian(xlim = coordx, ylim = coordy) +
    ggtitle(paste0("Diameter = ", as.character(round(diam, 3)))) +
    theme(legend.position = "none") + 
    theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=26, hjust=0))
  } else {
    g <- ggplot(centers, aes(x = x, y = y)) + geom_point(mapping = aes(size = mag)) + 
      geom_path(data = myCircles, mapping = aes(x = x, y = y, group = group, size = mag/5)) + 
      geom_segment(data = connectByLine, aes(x = x, y = y, xend = xend, yend = yend, size = rep(mag, length(connectByLine$x)))) +
      coord_cartesian(xlim = coordx, ylim = coordy) +
      ggtitle(paste0("Diameter = ", as.character(round(diam, 3)))) + 
      theme(legend.position = "none") + 
      theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=26, hjust=0))
  }
  }
  
  if(mag == 1) {
    if(curGroup > 1) {
      g <- ggplot(centers, aes(x = x, y = y)) + geom_point(size = 0.5) + 
        geom_path(data = myCircles, mapping = aes(x = x, y = y, group = group), alpha = 0.25) + 
        geom_polygon(data = connectTriangle, aes(x = x, y = y, group = group, fill = group, alpha = 0.5)) + 
        geom_segment(data = connectByLine, aes(x = x, y = y, xend = xend, yend = yend)) +
        coord_cartesian(xlim = coordx, ylim = coordy) +
        ggtitle(paste0("Diameter = ", as.character(round(diam, 3)))) +
        theme(legend.position = "none")
      #+ 
      #  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=26, hjust=0))
    } else {
      g <- ggplot(centers, aes(x = x, y = y)) + geom_point(size = 0.5) + 
        geom_path(data = myCircles, mapping = aes(x = x, y = y, group = group), alpha = 0.25) + 
        geom_segment(data = connectByLine, mapping = aes(x = x, y = y, xend = xend, yend = yend)) +
        coord_cartesian(xlim = coordx, ylim = coordy) +
        ggtitle(paste0("Diameter = ", as.character(round(diam, 3)))) + 
        theme(legend.position = "none") 
      #+ 
      #  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", face="bold", size=26, hjust=0))
    }   
  }
  
  if(showPlot) print(g)
  
  return(g)
} 
