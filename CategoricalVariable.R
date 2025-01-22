# CategoricalVariable  --------------------------------------------------------------------
# Child class for categorical variables

library(R6)

CategoricalVariable <- R6::R6Class(
  "CategoricalVariable",
  inherit = SyntheticVariable,
  public = list(
    categories = NULL,
    
    initialize = function(varName, desc = "", categories = c("Cat1", "Cat2")) {
      super$initialize(varName, "categorical", desc)
      self$categories = categories
    },
    
    getSummary = function() {
      baseSum <- super$getSummary()
      paste(
        baseSum,
        "\nCategories:", paste(self$categories, collapse=", ")
      )
    }
  )
)
