# NumericVariable --------------------------------------------------------------------
# Child class for numeric variables

library(R6)

NumericVariable <- R6::R6Class(
  "NumericVariable",
  inherit = SyntheticVariable,
  public = list(
    distType = NULL,
    mean     = 0,
    sd       = 1,
    minVal   = 0,
    maxVal   = 1,
    meanlog  = 0,
    sdlog    = 1,
    corr     = NULL,  # e.g. list(varName = "...", value = 0.3)
    
    initialize = function(varName, desc = "", distType = "Normal") {
      super$initialize(varName = varName, varType = "numeric", desc = desc)
      self$distType = distType
    },
    
    getSummary = function() {
      baseSum <- super$getSummary()
      
      # Build a param string (for demonstration)
      paramStr <- switch(
        self$distType,
        "Normal"    = paste0("mean=", self$mean, ", sd=", self$sd),
        "Uniform"   = paste0("[", self$minVal, ", ", self$maxVal, "]"),
        "Lognormal" = paste0("meanlog=", self$meanlog, ", sdlog=", self$sdlog),
        # etc. for other distributions
        ""
      )
      
      corrStr <- if (!is.null(self$corr)) {
        paste0("Corr with ", self$corr$varName, "=", self$corr$value)
      } else ""
      
      paste(
        baseSum,
        "\nDistribution:", self$distType,
        "\nParameters:", paramStr,
        if (nzchar(corrStr)) paste0("\nCorrelation: ", corrStr) else ""
      )
    }
  )
)
