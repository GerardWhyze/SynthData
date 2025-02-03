# global --------------------------------------------------------------------

library(shiny)
library(MASS)  # for mvrnorm
library(DT)
library(bslib)
library(ggplot2)
library(fitdistrplus)


#Helper function to round poisson values to nearest integer
pois_density <- function(x, lambda) {
  dpois(round(x), lambda = lambda)
}
