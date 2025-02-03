VariableModalManager <- R6Class(
  "VariableModalManager",
  public = list(
    existingNumericVars = NULL,
    initialize = function(existingNumericVars = NULL) {
      self$existingNumericVars <- existingNumericVars
    },
    getModal = function() {
      modalDialog(
        title = "Add Variable",
        textInput("varName_modal", "Variable Name:"),
        selectInput(
          "varType_modal",
          "Variable Type:",
          choices = c("numeric", "categorical")
        ),
        conditionalPanel(
          condition = "input.varType_modal == 'numeric'",
          selectInput(
            "distType_modal",
            "Distribution Type:",
            choices = c(
              "Normal",
              "Uniform",
              "Lognormal",
              "Exponential",
              "Gamma",
              "Beta",
              "Poisson",
              "Binomial",
              "NegBinomial",
              "ChiSquared",
              "StudentT",
              "F"
            )
          ),
          conditionalPanel(
            condition = "input.distType_modal == 'Normal'",
            numericInput("param_normal_mean", "Mean:", value = 0),
            numericInput("param_normal_sd", "SD:", value = 1)
          ),
          conditionalPanel(
            condition = "input.distType_modal == 'Uniform'",
            numericInput("param_unif_min", "Min:", value = 0),
            numericInput("param_unif_max", "Max:", value = 1)
          ),
          conditionalPanel(
            condition = "input.distType_modal == 'Lognormal'",
            numericInput("param_lnorm_meanlog", "Meanlog:", value = 0),
            numericInput("param_lnorm_sdlog", "SDlog:", value = 1)
          )
          # You can add additional conditional panels for other distributions as needed.
        ),
        conditionalPanel(
          condition = "input.varType_modal == 'categorical'",
          textInput("cat1", "Category 1:"),
          textInput("cat2", "Category 2:"),
          textInput("cat3", "Category 3:")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("acceptVar", "Add Variable", class = "btn-success")
        )
      )
    }
  )
)

# Helper function that instantiates the R6 class and returns the modal.
getAddVariableModal <- function(existingNumericVars = NULL) {
  manager <- VariableModalManager$new(existingNumericVars)
  return(manager$getModal())
}
