#' `return_one_of_two()` is an example of a function that uses a default value
#' (`return_x = TRUE`), control flow (`if`), and a `return()` call
#'
## ---- return_one_of_two
return_one_of_two <- function(x, y, return_x = TRUE) {
  if (return_x) {
    return(x)
  }

  y
}

## ---- area_acres
#' `area_acres()` is an example of a mutate function that takes a vector (a `sfc`
#' list in this case) and returns an equal length vector
area_acres <- function(x, ..., digits = 2) {
  geom_area <- sf::st_area(x, ...)
  geom_area <- units::set_units(geom_area, "acres")
  area <- as.numeric(geom_area)
  round(area, digits = digits)
}

## ---- mean_area
#' `mean_area()` is an example of a summary type vector function that takes a
#' vector and returns a length 1 vector
mean_area <- function(x, ..., value = "acres", na.rm = FALSE, digits = 2) {
  geom_area <- sf::st_area(x, ...)
  geom_area <- units::set_units(geom_area, value = value, mode = "standard")
  area <- as.numeric(geom_area)
  round(mean(area, na.rm = na.rm), digits = digits)
}

## ---- select_starts_with
#' `select_starts_with()` is an example of a data frame function that takes a
#' data frame and returns a data frame with the columns selected using the match
#' parameter
select_starts_with <- function(data, match, ...) {
  dplyr::select(data, dplyr::starts_with(match, ...))
}

## ---- bind_sf_area_acres
#' `bind_sf_area_col()` is an example of a data frame function that takes a `sf`
#' data frame and returns a `sf` data frame with an additional column named
#' "area"
bind_sf_area_acres <- function(data, ...) {
  stopifnot(inherits(data, "sf"))

  sf_column <- attributes(data)$sf_column

  dplyr::mutate(
    data,
    area = area_acres(data[[sf_column]], ...),
    .before = all_of(sf_column)
  )
}

## ---- make_sf_plot
#' `make_sf_plot()` is a sample function provided to illustrate the structure of
#' a mapping or plotting function
make_sf_plot <- function(data, mapping = aes(), ...) {
  ggplot2::ggplot(data = data, mapping = mapping) +
    ggplot2::geom_sf(...)
}
