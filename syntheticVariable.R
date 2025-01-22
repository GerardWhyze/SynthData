# SyntheticVariable --------------------------------------------------------------------
# Abstract parent class for synthetic variables

library(R6)

SyntheticVariable <- R6::R6Class(
  "SyntheticVariable",
  public = list(
    varName = NULL,
    varType = NULL,  # "numeric" or "categorical"
    desc    = NULL,
    
    initialize = function(varName, varType, desc = "") {
      self$varName = varName
      self$varType = varType
      self$desc    = desc
    },
    
    # Generic summary method (overridden in child classes if needed)
    getSummary = function() {
      paste(
        "Variable:", self$varName,
        "\nType:", self$varType,
        "\nDesc:", self$desc
      )
    }
  )
)
