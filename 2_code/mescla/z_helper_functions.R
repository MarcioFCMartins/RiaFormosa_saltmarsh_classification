draw_quadrat <- function(theta, edge_size, x, y){
 # Our quadrat will be inscribed in a circle with radius r
  r <- sqrt(2 * edge_size^2)/2
  
  # Each point will be pi/4 radians away from each other, offset 
  # by the transect theta
  square <- c(pi/4 + theta, 
              pi/4*3 + theta, 
              pi/4*5 + theta, 
              pi/4*7 + theta, 
              pi/4 + theta)
  # Get the quadrat points
  # Offset by x and y, plus a value that makes one corner "snap"
  #   to the given x and y
  points <- matrix(c(sin(square) * r + x + sin(pi/4+theta) * r, 
                     cos(square) * r + y + cos(pi/4+theta) * r),
                   ncol = 2)
  return(st_polygon(list(points)))
}

calc_orientation <- function(delta_x,delta_y){
  b <- c(delta_x, delta_y)
  a <- c(0,1)
  # This calculates the angle to a vertical line (between 0 and pi)
  theta <- acos(sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b))))
  # Correct to be between 0 and 2pi (conditions change depending on half of the unit circle it lands)
  if(delta_x >= 0){
    theta
  } else {
    (pi - theta) + pi
  }
}

break_vector <- function(name, start_x, start_y, theta, length, n_segments){
  scale <- length / n_segments
  start <- data.frame("quad_index" = 0,
                      "name" = name,
                      "x" = start_x,
                      "y" = start_y)
  points <- data.frame(
    "quad_index" = numeric(),
    "name" = character(),
    "x" = numeric(),
    "y" = numeric())
  points <- rbind(start, points)
  
  for(i in 1:n_segments){
    x <- start_x + (sin(theta) * scale * i)
    y <- start_y + (cos(theta) * scale * i)
    index <- i
    points <- rbind(c(index, name, x, y), points)
  }
  points
}

# The following code demonstrates the logic behind this function visually
y = 0 # center y
x = 0
ref_vector <- c(1,0)
n = 100 # nr of pts
edge_size = 1 # radius
o = pi/4
r <- sqrt(2 * edge_size^2)/2
circle = seq(0, 2 * pi, length.out = n)
square = c(pi/4 + o, pi/4*3 + o, pi/4*5 + o, pi/4*7 + o, pi/4 + o)
par(mfrow=c(1,2))
par(mar=c(5, 4, 4, 2), xpd=TRUE)
plot(sin(circle)*r+x, cos(circle)*r+y, type = 'l', asp = 1) # test
points(x,y, pch = 19, col = "blue")
# Draw the transect orientation
arrows(x0 = -sin(o)*r,
       x1 = sin(o)*r,
       y0 = -cos(o)*r,
       y1 = cos(o)*r, col = "red")
# Draw the offset applied to the square
lines(c(x, sin(pi/4+o)*r),c(y, cos(pi/4+o)*r), col = "green")
lines(sin(square)*r+x, cos(square)*r+y)
legend("bottomright", 
       inset=c(0,1),
       bty = "n",
       legend = c("transect orientation", 
                                 "quadrat offset", 
                                 "quadrat point"),
       fill = c("red", "green", "blue"))
plot(sin(square)*r+x-sin(pi/4+o)*r, cos(square)*r+y-cos(pi/4+o)*r, type = "l", asp = 1)
points(x,y, pch = 19, col = "blue")

