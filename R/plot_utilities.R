
nullplot <- function(xrange = c(0,1), yrange = c(0,1), boxtype = "L"){
  plot(1, 1, type = "n", bty = boxtype,
       xlim = xrange, ylim = yrange, 
       xlab = "", ylab = "", 
       xaxt = "n", yaxt = "n")
}