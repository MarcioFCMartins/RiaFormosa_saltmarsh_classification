# Functions to convert a transect (with known start and end)
# into individual, equal quadrats along those transects
# Uses basic trigonometry and linear algebra (thank god for stackoverflow)


#  Calculate vector orientation -------------------------------------------
# https://stackoverflow.com/questions/1897704/angle-between-two-vectors-in-r/24999820
# https://www.math.uh.edu/~jmorgan/Math6397/day13/LinearAlgebraR-Handout.pdf - page 5

# I translate the transect vector to the origin
# (i.e. calculate delta x and delta y)
# I then create a second vector b, with a y of 1 and x of zero (aka North)
# Returts the radians between those 2 vectors (orientation)
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


# Break vector into known number of equal segments ------------------------
# Takes the start position, orientation and length of a vector
# Breaks it into n segments
# Returs a data.frame with points for the start of each segment
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


# Draw a square of known size, position and orientation -------------------
# Takes a position, orientation and edge size for each quadrat
# Returns polygons for each quadrat
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

