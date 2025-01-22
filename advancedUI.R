
# advancedUI --------------------------------------------------------------


# A helper function returning the modal dialog UI for adding a variable.
# This includes all distribution parameter fields for numeric variables,
# plus category fields for categorical variables.
getAddVariableModal <- function(existingNumericVars = character()) {
  modalDialog(
    title = "Define a New Variable",
    size = "l",
    fluidPage(
      fluidRow(
        column(6,
               textInput("varName_modal", "Variable Name:", value = "")
        ),
        column(6,
               selectInput("varType_modal", "Variable Type:",
                           choices = c("numeric", "categorical"))
        )
      ),
      # Short description
      textAreaInput("varDesc_modal", "Short Description (<= 50 words):", value="", rows=3),
      
      # Numeric variable distribution choices
      conditionalPanel(
        condition = "input.varType_modal == 'numeric'",
        selectInput("distType_modal", "Distribution:",
                    choices = c("Normal", "Uniform", "Lognormal",
                                "Exponential", "Gamma", "Beta",
                                "Poisson", "Binomial", "NegBinomial",
                                "ChiSquared", "StudentT", "F"),
                    selected = "Normal"),
        
        # Normal
        conditionalPanel(
          condition = "input.distType_modal == 'Normal'",
          numericInput("param_normal_mean", "Mean:", 0),
          numericInput("param_normal_sd",   "SD:",   1, min=0)
        ),
        # Uniform
        conditionalPanel(
          condition = "input.distType_modal == 'Uniform'",
          numericInput("param_unif_min", "Min:", 0),
          numericInput("param_unif_max", "Max:", 1)
        ),
        # Lognormal
        conditionalPanel(
          condition = "input.distType_modal == 'Lognormal'",
          numericInput("param_lnorm_meanlog", "Mean (log-scale):", 0),
          numericInput("param_lnorm_sdlog",   "SD (log-scale):",   1, min=0)
        ),
        # Exponential
        conditionalPanel(
          condition = "input.distType_modal == 'Exponential'",
          numericInput("param_exp_rate", "Rate:", 1, min=0)
        ),
        # Gamma
        conditionalPanel(
          condition = "input.distType_modal == 'Gamma'",
          numericInput("param_gamma_shape", "Shape:", 2, min=0),
          numericInput("param_gamma_rate",  "Rate:",  1, min=0)
        ),
        # Beta
        conditionalPanel(
          condition = "input.distType_modal == 'Beta'",
          numericInput("param_beta_alpha", "Alpha:", 2, min=0),
          numericInput("param_beta_beta",  "Beta:",  2, min=0)
        ),
        # Poisson
        conditionalPanel(
          condition = "input.distType_modal == 'Poisson'",
          numericInput("param_pois_lambda", "Lambda:", 1, min=0)
        ),
        # Binomial
        conditionalPanel(
          condition = "input.distType_modal == 'Binomial'",
          numericInput("param_binom_size", "Size (n):", 10, min=1),
          numericInput("param_binom_prob", "Prob (p):", 0.5, min=0, max=1, step=0.01)
        ),
        # Negative Binomial
        conditionalPanel(
          condition = "input.distType_modal == 'NegBinomial'",
          numericInput("param_negbin_size", "Size (# of successes):", 5, min=1),
          numericInput("param_negbin_prob", "Prob (success):", 0.5, min=0, max=1, step=0.01)
        ),
        # ChiSquared
        conditionalPanel(
          condition = "input.distType_modal == 'ChiSquared'",
          numericInput("param_chisq_df", "df:", 2, min=1)
        ),
        # StudentT
        conditionalPanel(
          condition = "input.distType_modal == 'StudentT'",
          numericInput("param_t_df", "df:", 5, min=1)
        ),
        # F
        conditionalPanel(
          condition = "input.distType_modal == 'F'",
          numericInput("param_f_df1", "df1:", 5, min=1),
          numericInput("param_f_df2", "df2:", 5, min=1)
        ),
        
        # Correlation with an existing numeric variable
        selectInput("corrWith_modal", "Correlation with Existing Numeric Var:",
                    choices = c("None", existingNumericVars)),
        conditionalPanel(
          condition = "input.corrWith_modal != 'None'",
          numericInput("corrValue_modal", "Correlation Coefficient:",
                       value=0, min=-0.99, max=0.99, step=0.01),
          helpText("Approximate correlation by transforming from correlated standard normals.")
        )
      ),
      
      # Categorical variable categories
      conditionalPanel(
        condition = "input.varType_modal == 'categorical'",
        helpText("Define up to 6 categories. Leave blank for unused categories."),
        fluidRow(
          column(6, textInput("cat1", "Category 1:", value="Cat1")),
          column(6, textInput("cat2", "Category 2:", value="Cat2"))
        ),
        fluidRow(
          column(6, textInput("cat3", "Category 3:", value="")),
          column(6, textInput("cat4", "Category 4:", value=""))
        ),
        fluidRow(
          column(6, textInput("cat5", "Category 5:", value="")),
          column(6, textInput("cat6", "Category 6:", value=""))
        )
      )
    ),
    footer = tagList(
      modalButton("Cancel"),
      actionButton("acceptVar", "Accept", class="btn-primary")
    )
  )
}
