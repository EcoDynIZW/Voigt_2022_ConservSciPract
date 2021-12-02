## from StackOverflow:
## stackoverflow.com/questions/28567166/uniformly-distribute-x-points-inside-a-circle


#' Create uniform circular sunflower distribution
#'
#' @param n Number of points.
#' @param alpha A number indicating the evenness.
#' @param geometry A string indicating the pattern (planar or geodesic)
#' @return A data frame containing the 2D position of n points.
#' @examples
#' sunflower(n = 500, alpha =2, geometry = 'planar')
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


#' Calculate radius for sunflower function
#'
#' @param n Number of points.
#' @param k Index.
#' @param b Golden ratio parameter (?)
#' @return Radius from the center for a given point
radius <- function(k,n,b) {
  ifelse(
    k > n-b,
    1,
    sqrt(k-1/2) / sqrt(n-(b+1)/2)
  )
}
