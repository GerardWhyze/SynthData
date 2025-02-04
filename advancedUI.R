library(shiny)
library(R6)

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
          radioButtons(
            "paramType_modal",
            "Parameter Type:",
            choices = c("Variable Parameters", "Group Specific Parameters"),
            inline = TRUE
          ),
          conditionalPanel(
            condition = "input.paramType_modal == 'Variable Parameters'",
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
            ),
            conditionalPanel(
              condition = "input.distType_modal == 'Exponential'",
              numericInput("param_exp_rate", "Rate:", value = 1, min = 0)
            ),
            conditionalPanel(
              condition = "input.distType_modal == 'Gamma'",
              numericInput(
                "param_gamma_shape",
                "Shape:",
                value = 2,
                min = 0
              ),
              numericInput(
                "param_gamma_rate",
                "Rate:",
                value = 1,
                min = 0
              )
            ),
            conditionalPanel(
              condition = "input.distType_modal == 'Beta'",
              numericInput(
                "param_beta_alpha",
                "Alpha:",
                value = 2,
                min = 0
              ),
              numericInput(
                "param_beta_beta",
                "Beta:",
                value = 2,
                min = 0
              )
            ),
            conditionalPanel(
              condition = "input.distType_modal == 'Poisson'",
              numericInput(
                "param_pois_lambda",
                "Lambda:",
                value = 1,
                min = 0
              )
            ),
            conditionalPanel(
              condition = "input.distType_modal == 'Binomial'",
              numericInput(
                "param_binom_size",
                "Size (n):",
                value = 10,
                min = 1
              ),
              numericInput(
                "param_binom_prob",
                "Probability (p):",
                value = 0.5,
                min = 0,
                max = 1,
                step = 0.01
              )
            ),
            conditionalPanel(
              condition = "input.distType_modal == 'NegBinomial'",
              numericInput(
                "param_nbinom_size",
                "Size (# successes):",
                value = 5,
                min = 1
              ),
              numericInput(
                "param_nbinom_prob",
                "Probability (success):",
                value = 0.5,
                min = 0,
                max = 1,
                step = 0.01
              )
            ),
            conditionalPanel(
              condition = "input.distType_modal == 'ChiSquared'",
              numericInput(
                "param_chisq_df",
                "Degrees of Freedom (df):",
                value = 2,
                min = 1
              )
            ),
            conditionalPanel(
              condition = "input.distType_modal == 'StudentT'",
              numericInput(
                "param_t_df",
                "Degrees of Freedom (df):",
                value = 5,
                min = 1
              )
            ),
            conditionalPanel(
              condition = "input.distType_modal == 'F'",
              numericInput("param_f_df1", "DF1:", value = 5, min = 1),
              numericInput("param_f_df2", "DF2:", value = 5, min = 1)
            )
          ),
          conditionalPanel(condition = "input.paramType_modal == 'Group Specific Parameters'", # Render dynamic UI inputs for group-specific parameters.
                           uiOutput("groupParamInputs")),
          # Global correlation inputs are shown only if group-specific correlations are not checked.
          conditionalPanel(
            condition = "!input.groupCorr_checkbox",
            selectInput(
              "corrWith_modal",
              "Correlate with:",
              choices = c("None", self$existingNumericVars)
            ),
            numericInput(
              "corrValue_modal",
              "Correlation Value:",
              value = 0,
              min = -1,
              max = 1
            )
          ),
          checkboxInput("groupCorr_checkbox", "Group Specific Correlations", value = FALSE),
          conditionalPanel(condition = "input.groupCorr_checkbox == true", uiOutput("groupCorrInputs"))
        ),
        conditionalPanel(
          condition = "input.varType_modal == 'categorical'",
          checkboxInput("grouping_modal", "Use as Grouping Variable?", value = FALSE),
          # Always show six separate text boxes for the categories.
          textInput("cat1", "Category 1:", value = ""),
          textInput("cat2", "Category 2:", value = ""),
          textInput("cat3", "Category 3:", value = ""),
          textInput("cat4", "Category 4:", value = ""),
          textInput("cat5", "Category 5:", value = ""),
          textInput("cat6", "Category 6:", value = "")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("acceptVar", "Add Variable", class = "btn-success")
        )
      )
    }
  )
)

getAddVariableModal <- function(existingNumericVars = NULL) {
  manager <- VariableModalManager$new(existingNumericVars)
  return(manager$getModal())
}
