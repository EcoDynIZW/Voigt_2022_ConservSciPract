
## Uniform Sunflower Function ##################################################
## from StackOverflow:
## stackoverflow.com/questions/28567166/uniformly-distribute-x-points-inside-a-circle

sunflower <- function(n, alpha = 2, geometry = c('planar','geodesic')) {
  b <- round(alpha*sqrt(n))  # number of boundary points
  phi <- (sqrt(5)+1)/2  # golden ratio

  r <- radius(1:n,n,b)
  theta <- 1:n * ifelse(geometry[1] == 'geodesic', 360*phi, 2*pi/phi^2)

  df <- tibble(
    x = r*cos(theta),
    y = r*sin(theta)
  )
}

radius <- function(k,n,b) {
  ifelse(
    k > n-b,
    1,
    sqrt(k-1/2) / sqrt(n-(b+1)/2)
  )
}

# example:
#sunflower(500, 2, 'planar')
