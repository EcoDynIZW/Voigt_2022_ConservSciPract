#' Create uniform circular sunflower distribution
#' stackoverflow.com/questions/28567166/uniformly-distribute-x-points-inside-a-circle
#'
#' @param n Number of points.
#' @param alpha A number indicating the evenness.
#' @param geometry A string indicating the pattern (planar or geodesic)
#' @return A data frame containing the 2D position of n points.
#' @examples
#' sunflower(n = 500, alpha = 2, geometry = 'planar')
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


#' Generate circular distributions
#'
#' @param prop_monitored Area of the acoustic area monitored as a half circle
#'                       below the center. A number greater than 0 and less than
#'                       or equal to 0.5 (as a half circle can cover max 50% of
#'                       a circle).
#' @param n Number of individual transits (points) simulated.
#' @param distribution A string indicating the pattern of the distribution. One
#'                     of "uniform", "random", "inner", "outer", "top" or "bottom".
#' @param seed A number passed as seeding for the random number generator.
#' @param pattern A string indicating the pattern in case of a uniform
#'                distribution which can be either "planar" or "geodesic".
#' @param skewness A positive number determining the skewness of the directional
#'                 distributions "top" and "bottom". The greater the number,
#'                 the higher the clustering of points towards the top and
#'                 bottom of the circle, respectively.
#' @param var A logical input indicating if fatalities are drawn with variation
#' @param report_counts A logical input if the function should print the counts
#'                      of monitored and non-monitored transits.
#' @return A data frame containing the 2D position of n points.
#' @examples
#' generate_distribution(prop_monitored = .05, n = 500L, distribution = "uniform")
#' generate_distribution(prop_monitored = .05, n = 500L, distribution = "top", skeweness = 5)
generate_distribution <- function(prop_monitored, n, distribution, seed = NULL,
                                  pattern = "planar", skewness = 1, var = TRUE, report_counts = FALSE) {

  if(!is.numeric(prop_monitored) | prop_monitored <= 0 | prop_monitored > .5) stop('prop_monitored should be a number greater than 0 and less than or equal to 0.5.')
  if(!is.integer(n) | n <= 0) stop('n should be a positive integer number.')
  if(!distribution %in% c("uniform", "random", "inner", "outer", "top", "bottom")) stop('distribution should be one of uniform", "random", "inner", "outer", "top" or "bottom".')
  if(!is.integer(seed) & !is.null(seed)) stop('seed should be a integer number.')
  if(distribution %in% c("uniform") & !pattern %in% c("planar", "geodesic")) stop('pattern should be either "planar" or "geodesic" in case of an uniform distribution.')
  if(distribution %in% c("bottom", "top", "inner", "outer") & skewness < 0) stop('skewness should be a number greater than 0.')
  if (!is.logical(report_counts)) stop('report_counts should be logical, either TRUE or FALSE.')

  ## INPUT VARIABLES #########################################################

  ## seed
  if (is.null(seed)) {
    set.seed(as.integer(runif(min = 1, max = 9999999, n = 1)))
  } else {
    set.seed(seed)
  }

  ## DEPENDENT VARIABLES #####################################################

  ## radius circle (~ length rotor blades)
  radius_rotor <- 1
  ## -> hardcoded since we scale are_monitored and keep area_rotor constant

  ## radius acoustic monitoring via % area covered + length rotor blades
  area_rotor <- pi * radius_rotor^2
  area_monitored <- area_rotor * prop_monitored
  radius_monitored <- sqrt(area_monitored / pi)

  ## mask for rotor area
  circle <- spatstat.geom::disc(
    radius = radius_rotor,
    centre = c(0, 0),
    mask = FALSE,
    npoly = 5000
  )

  ## radius acoustic monitoring via % area covered + length rotor blades
  area_rotor <- pi * radius_rotor^2
  area_monitored <- area_rotor * prop_monitored
  radius_monitored <- sqrt(2 * area_monitored / pi)

  ## GENERATE DISTRIBUTION ###################################################
  if(distribution == "uniform") pp <- sunflower(n, radius_rotor, pattern)
  if(distribution == "random")  pp <- spatstat.core::rpoint(n, win = circle)
  if(distribution == "inner")   pp <- spatstat.core::rpoint(n, function(x,y) {(1 - (abs(x^2 + y^2)) + .01)^skewness}, win = circle)
  if(distribution == "outer")   pp <- spatstat.core::rpoint(n, function(x,y) {((abs(x^2 + y^2)) + .01)^skewness}, win = circle)
  if(distribution == "bottom")  pp <- spatstat.core::rpoint(n, function(x,y) {100 * exp(-skewness*y)}, win = circle)
  if(distribution == "top")     pp <- spatstat.core::rpoint(n, function(x,y) {100 * exp(skewness*y)}, win = circle)

  ## turn into data frame
  df_pp <- as.data.frame(pp)

  ## ESTIMATE BATS INSIDE MONITORING AREA ####################################

  ## add distance to center
  df_pp$dist <- df_pp$x^2 + df_pp$y^2
  df_pp$dist <- ifelse(df_pp$dist > radius_rotor, radius_rotor, df_pp$dist) ## -> again, radius is hardcoded

  ## estimate points inside acoustic monitoring area
  ## bats inside the (doubled) circle but not above the center
  df_pp$monitored <- ifelse(df_pp$dist <= radius_monitored^2 & df_pp$y < 0, TRUE, FALSE)

  ## check counts
  if (report_counts == TRUE) print(dplyr::count(df_pp, monitored))

  ## sample fatalities
  df_pp$id <- 1:n
  #n_monitored <- nrow(dplyr::filter(df_pp, monitored == TRUE))
  n_monitored <- n

  ## sample bat pass id that get hit
  if (var == TRUE) {
    corr_fatality <- rnorm(1, .01, .005) ##(runif(1, 0, 1) + runif(1, 0, 1)) / 100
    if (corr_fatality < 0) corr_fatality <- 0
  } else {
    corr_fatality <- .01
  }

  n_fatality <- round(n * corr_fatality)
  id_fatality <- sample(1:n, size = n_fatality)
  df_pp$fatality <- ifelse(df_pp$id %in% id_fatality, TRUE, FALSE)

  ## store inputs
  if (distribution %in% c("bottom", "top", "inner", "outer")) {
    df_pp$distribution <- paste0(distribution, "_", skewness)
  } else {
    df_pp$distribution <- distribution
  }
  df_pp$prop_monitored <- prop_monitored
  df_pp$n <- n
  df_pp$seed <- seed

  return(df_pp)
}





#' Simulate multiple runs with a range of distributions and other parameter inputs
#'
#' @param runs Number of simulations per parameter combination.
#' @param prop_monitored Area of the acoustic area monitored as a half circle
#'                       below the center. A number greater than 0 and less than
#'                       or equal to 0.5 (as a half circle can cover max 50% of
#'                       a circle).
#' @param n Number of individual transits (points) simulated.
#' @param skewness Vector of skewness parameters for the directional distributions
#'                "top" and "bottom".
#' @return A tibble.
simulate_multiple_distributions <- function(runs, prop_monitored, n, skewness) {

  ## generate input table of all parameter combinations
  ## no skewness values for uniform and random
  input <- expand.grid(
    distribution = c("uniform", "random"),
    prop_monitored = prop_monitored,
    n = n,
    skewness = NA,
    seed = as.integer(1:runs)
  )

  input_skewed <- expand.grid(
    distribution = c("top", "bottom", "inner", "outer"),
    prop_monitored = prop_monitored,
    n = n,
    skewness = skewness,
    seed = as.integer(1:runs)
  )

  input <- rbind(input, input_skewed)

  d <- pmap_dfr(
    input,
    ~ generate_distribution(distribution = ..1, prop_monitored = ..2, n = ..3, skewness = ..4, seed = ..5) %>%
      dplyr::add_count(distribution, prop_monitored, seed, name = "n") %>%
      dplyr::group_by(distribution, prop_monitored, seed, n) %>%
      dplyr::summarize(
         n_monitored = sum(monitored),
         n_fatalities = sum(fatality),
         n_fatalities_monitored = sum(fatality[which(monitored == TRUE)]),
         .groups = "drop"
      ) %>%
      dplyr::mutate(prop_n_monitored = n_monitored / n)
  )

  return(d)
}






## Plot circular distributions generated with generate_distribution()
#'
#' @param data A data set containing at least the numeric columns "x" and "y"
#'             as well as a logical column called "monitored".
#' @param title A strings used as title (optional).
#' @param color Color used to visualize transits within the monitoring area (optional).
#' @param print Logical. Should the plot be printed?
#' @param save Logical. Should the plot be saved to disc? All plots are stored
#'             under "./plots". If no file name (see "filename") is provided,
#'             the plot is saved as "plot_distribution_{date-time}".
#' @param filename  A string indicating the file name (optional).
#' @return A ggplot.
#' @examples
#' plot_distribution(generate_distribution(prop_monitored = .05, n = 500L, distribution = "uniform"))
plot_distribution <- function(data, title = NULL, color = "orange2", print = TRUE, save = FALSE, filename = NULL) {
  if (!is.data.frame(data)) stop('data should be a data frame.')
  if (unique(!c("x", "y", "monitored") %in% names(data))) stop('data should contain the following columns: "x", "y" and "monitored".')
  if (!is.character(title) & !is.null(title)) stop('title should be of type character.')
  if (!is.logical(save)) stop('save should be logical, either TRUE or FALSE.')

  circle <- spatstat.geom::disc(
    radius = 1,  ## for now hard-coded as in generate_distributions()
    centre = c(0, 0),
    mask = FALSE,
    npoly = 5000
  )

  if (is.null(title)) title <- paste0(nrow(data), " bats, ", data$distribution, " distribution, ", data$prop_monitored * 100, "% covered")

  g <-
    ggplot2::ggplot(data, ggplot2::aes(x, y)) +
    ggplot2::geom_path(data = as.data.frame(circle), color = "grey67", size = 2) +
    ggplot2::geom_point(data = dplyr::filter(data, monitored == FALSE), alpha = .7) +
    ggplot2::geom_point(data = dplyr::filter(data, monitored == TRUE), color = color, alpha = .7) +
    ggplot2::coord_fixed() +
    ggplot2::scale_x_continuous(limits = c(-1, 1)) +
    ggplot2::scale_y_continuous(limits = c(-1, 1)) +
    ggplot2::ggtitle(title)

 if (print == TRUE) print(g)

  if (save == TRUE) {

    path <- paste0(getwd(), "/plots")
    if (!dir.exists(path)) dir.create(path)
    if (is.null(filename)) {
      filepath <- paste0(path, "./plot_distribution_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    } else{
      filepath <- paste0(path, "/plot_distribution_", stringr::str_remove(filename, ".pdf|.png|.jpg|.jpeg|.tiff|.bmp"), ".pdf")
    }

    ggsave(filepath, width = 6, height = 6, device = cairo_pdf)
  }

  return(g)
}
