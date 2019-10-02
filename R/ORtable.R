################################################################################
###
###    Project: R package risyphus
###
### PI/Contact: Fridtjof Thomas
###
###    Purpose: Function to compile OR-table (one variable at at time)
###
###       Code: Fridtjof Thomas, 06/21/2018
###               Last modified: Under GIT version control.
###
################################################################################
###    History: >Short description of major changes to code, if applicable<
###
###
################################################################################
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#
# To build documentation, run
# > devtools::document()





# Function determining the number of needed rows for the OR table
# From risyphus::BLtable.layout
#' Function to gather information about the OR-table before computing begins
#'
#' \code{ORtable.layout} is called by \code{\link{ORtable}} to gather information about the table before computing begins
#'
#' @inheritParams ORtable
#' @return Data frame containing layout information of the table to be computed
#'    having these columns:
#'  \describe{
#'  \item{Variable, Table.label, etc.}{Inherited from data set submitted as 'info'}
#'  \item{n_rows}{Number of rows in the table for the corresponding variable}
#'  \item{first_row}{First table row used for the corresponding variable}
#'  \item{last_row}{Last table row used for the corresponding variable}
#' }
#' @examples
#' \dontrun{ Called by other function - not intended to be called by user directly. }
#' @export
ORtable.layout <- function(data, info){
  ### NOTE: Uses factor levels to determine table
  ###       - all dichotomous or factor-variables need to be R-factors!
  table.info <- data.frame(info, n_rows=NA, first_row=NA, last_row=NA)
  first_j <- 0 # Header information will be in colnames().
  last_j <- 0
  for (k in (1:length(table.info$Variable))){
    if (table.info[k,"Type"] == "Continuous"){
      table.info[k, "n_rows"] <- 1
      first_j <- last_j + 1 # Last row from variable just before the current one.
      last_j <- last_j + 1
      table.info[k, "first_row"] <- first_j
      table.info[k, "last_row"] <- last_j
    } else if (table.info[k,"Type"] %in% c("Dichotomous", "Factor")){
      this.variable <- table.info[k,"Variable"]
      needed.rows <- length(levels(data[,this.variable])) + 1 # 1 for name and overall p-value.
      table.info[k, "n_rows"] <- needed.rows
      first_j <- last_j + 1 # Last row from variable just before the current one.
      last_j <- last_j + needed.rows
      table.info[k, "first_row"] <- first_j
      table.info[k, "last_row"] <- last_j
    }
  }
  return(table.info)
}


# Based on risyphus::table.text

#' Function to compute and render text entries for OR-table
#'
#' @param this.data Data used for the computations
#' @param this.outcome The name of the variable (column) in \code{data} that is
#'    the binary outcome for the logistic regression models.
#' @param this.var Variable in the table (must exist in \code{this.data})
#' @param this.type Variable type for table
#' @inheritParams ORtable
#' @return Output used by \code{ORtable()}
#' @examples
#' \dontrun{ Called by other function - not intended to be called by user directly. }
#' @export
ORtable.text <- function(this.data, this.outcome, this.var,
                         this.type, sign.digits, sign.digits.OR, pvalue.digits, pvalue.cutoff, less.than.character){

  if (this.type == "Continuous") { # Logistic regression, one line output.

    glm_One <- stats::glm(
      stats::as.formula(paste(this.outcome, " ~ ", this.var, sep = "")),
      data = this.data, family = stats::binomial)

    this.pvalue <- summary(glm_One)$coefficients[-1, "Pr(>|z|)"]

    # First level is reference level.
    # exp(coefficients(glm_One))[-1] # Drop intercept.
    # exp(confint.default(glm_One))[-1, 1] # Drop intercept.
    # summary(glm_One)$coefficients[-1, "Pr(>|z|)"] # Drop intercept.
    #

    list(
      Col1 = paste0(formatC(exp(stats::coefficients(glm_One))[-1], digits=sign.digits.OR, format="f")),
      Col2 =
        paste0(
          "(",
          formatC(exp(stats::confint.default(glm_One))[-1, 1], digits=sign.digits.OR, format="f"),
          "-",
          formatC(exp(stats::confint.default(glm_One))[-1, 2], digits=sign.digits.OR, format="f"),
          ")"
        ),
      Col3 = ifelse(this.pvalue < pvalue.cutoff,
                    paste(less.than.character, pvalue.cutoff, sep=""),
                    formatC(this.pvalue, digits=pvalue.digits, format="f"))
    )
  } else if ( (this.type %in% c("Dichotomous", "Factor") ) ) {

    glm_One <- stats::glm(
      stats::as.formula(paste(this.outcome, " ~ ", this.var, sep = "")),
      data = this.data, family = stats::binomial)

    # If two factor levels only, overall p-value will be marginal p-value
    # of not-reference level, otherwise overall p-value will be
    # based on based on likelihood ratio test.
    # Reason: Otherwise slight numerical differences in displayed p-values
    #         due to different approximations might occur.

    this.pvalue.coeff <- summary(glm_One)$coefficients[-1, "Pr(>|z|)"]

    if (length(this.pvalue.coeff) == 1){
      this.pvalue <- this.pvalue.coeff
    } else{
      this.pvalue <- stats::anova(glm_One, test = "LRT")[2, "Pr(>Chi)"] # Overall p-value based on likelihood ratio test.
    }


    # First level is reference level.
    # exp(coefficients(glm_One))[-1] # Drop intercept.
    # exp(confint.default(glm_One))[-1, 1] # Drop intercept.
    # summary(glm_One)$coefficients[-1, "Pr(>|z|)"] # Drop intercept.
    #

    # Table: First row cotains only overall p-value, and first factor level is reference-level.
    list(
      Col1 = c(" ", "Reference",
               paste0(formatC(exp(stats::coefficients(glm_One))[-1], digits=sign.digits.OR, format="f"))),
      Col2 = c(" ", " ",
               paste0("(",
                      formatC(exp(stats::confint.default(glm_One))[-1, 1], digits=sign.digits, format="f"),
                      "-",
                      formatC(exp(stats::confint.default(glm_One))[-1, 2], digits=sign.digits, format="f"),
                      ")")
      ),
      Col3 = c(ifelse(this.pvalue < pvalue.cutoff,
                      paste(less.than.character, pvalue.cutoff, sep=""),
                      formatC(this.pvalue, digits=pvalue.digits, format="f")),
               " ",
               ifelse(this.pvalue.coeff < pvalue.cutoff,
                      paste(less.than.character, pvalue.cutoff, sep=""),
                      formatC(this.pvalue.coeff, digits=pvalue.digits, format="f"))
      )
    )
  } else { # Serves to catch errors:
    list(
      Col1 = "??",
      Col2 = "??",
      Col3 = "??"
    )
  }

}

# Test of function:
# ORtable.text(this.data = shunt_first, this.outcome="Return_to_OR_in_30_days",
#              this.var = "Race",
#             this.type = "Factor",
#             sign.digits = 4, sign.digits.OR = 4, pvalue.digits = 3, pvalue.cutoff = 0.001,
#             less.than.character = "<")
#
#
# glm_One <- glm( Return_to_OR_in_30_days ~ Race, data = shunt_first, family = binomial)
# exp(confint.default(glm_One))[-1, ]



#-------------------------------------------------------------------------------


### From risyphus::BLtable
#' Function to compile OR-table.
#'
#' @param data A data frame that contains the data (one row per participant).
#' @param this.outcome The name of the variable (column) in \code{data} that is
#'    the binary outcome for the logistic regression models.
#' @param info A data frame with specifications for the BL-table. (See Examples below.)
#' @param sign.digits Digits used for parameter estimate
#' @param sign.digits.OR Digits used for odds ratio with conf.intervals
#' @param pvalue.digits Digits used for p-values.
#' @param pvalue.cutoff Cutoff for changing p-values to simply 'smaller than', e.g., p < 0.001.
#' @param test.input Whether the input is tested for logical consistency.  Warning: testing is not
#'    comprehensive.  Setting \code{test.input = FALSE} will bypass logical tests.
#' @param factor.level.bullet The character to be prepended to factor levels (listing). The default
#'    is \code{factor.level.bullet = "- "} (hyphen/minus) but it can be reset to any character string. If the result is
#'    passed on to html-rendering an en-dash can be produced by \code{factor.level.bullet = "&#8211; "} (HTML-code for the en-dash).
#'    Other useful options for (subsequent) HTML-rendering: \code{factor.level.bullet = "&nbsp; &nbsp; "} forces non-breaking space and effectively
#'    leads to indentation of factor levels; \code{factor.level.bullet = "&#187; "} prints right double angle quotes in HTML.
#' @param less.than.character The character to be prepended to the cutoff-value for the p-values (e.g., '< 0.001'). The default
#'    is \code{less.than.character = "< "} but it can be reset to any character string. If the result is
#'    passed on to html-rendering this symbol can be produced by \code{less.than.character = "&lt; "}
#'    (HTML-friendly name for 'less than' since '<' is used in HTML-tags and might not render correctly as a character; e.g.,
#'    when using function htmlTable() in the htmlTable-package).
#' @param linebreak.tag Can be used to insert a linebreak-command in the table headers to bring the counts to a new line.
#'    The default is \code{linebreak.tag = ""} effectively not inserting any such command.  For subsequent HTML-rendering
#'    \code{linebreak.tag = "<br>"} (HTML tag to insert a single line break) is useful.
#' @return An R character-matrix containing the compiled table information.
#' @examples
#' \dontrun{
#' ORtable(data = example_data, info = example_variables,
#'   this.outcome = "Return_within_90_days", test.input = TRUE)
#' }
#' @export
ORtable <- function(data, this.outcome, info, sign.digits=2, sign.digits.OR=3, pvalue.digits=3, pvalue.cutoff=0.001,
                    test.input = FALSE,
                    factor.level.bullet = "- ", less.than.character = "< ", linebreak.tag = ""){

  # Convert tibbles to regular data frames without need to load package "tibbles":
  # Note: a tibble tests TRUE for as.data.frame(tbl).
  if ("tbl" %in% class(data)) { data <- as.data.frame(data) }

  # Test input:
  if (test.input) {  } # Currently no meaningful tests implemented.
  ### ADD TEST FOR FACTORS for all factors!

  # Create subset with existing group-information
  this.data <- data[ (!is.na(data[ , this.outcome])), ]
  if ( dim(this.data)[1] != dim(data)[1] ) {
    warning("Rows have been dropped due to missing information in the provided outcome-variable.")
  }

  # Compute layout information:
  this.layout <- ORtable.layout(this.data, info)

  # Set up the matrix that will hold the table information:
  mytable <- matrix(data = " ", nrow = max(this.layout$last_row), ncol = 3) # Filled with space that will not 'show' in table.
  colnames(mytable) <- c(
    paste0("OR"), paste0("95% CI"), paste0("p-value")
  )
  rownames(mytable) <- 1:dim(mytable)[1]

  # Fill table with values variable by variable:
  for (l in 1:length(this.layout$Variable)){
    this.var <- this.layout$Variable[l]
    this.label <- this.layout$Table.label[l]
    this.type <- this.layout$Type[l]
    this.n_rows <- this.layout$n_rows[l]
    this.first_row <- this.layout$first_row[l]
    this.last_row <- this.layout$last_row[l]



    # this.text <- table.text(this.data, this.var, group.var, this.GroupA, this.GroupB,
    #                         this.type, this.test, sign.digits, sign.digits.prop, pvalue.digits, pvalue.cutoff, less.than.character)
    #

    this.text <- ORtable.text(data, this.outcome, this.var, this.type,
                              sign.digits, sign.digits.OR, pvalue.digits, pvalue.cutoff,
                              less.than.character)

    rownames(mytable)[this.first_row] <- this.label # Variable name for table from layout description.


    #    mytable[this.first_row,"p-value"] <- this.text$Col3 # p-value goes always into the same line than variable name/label.
    if (this.n_rows == 1){ # All values go in a single row:
      mytable[this.first_row, 1] <- this.text$Col1
      mytable[this.first_row, 2] <- this.text$Col2
      mytable[this.first_row,"p-value"] <- this.text$Col3
    } else { # More than one row (applies to factors only)
      rownames(mytable)[(this.first_row+1):this.last_row] <-
        paste(factor.level.bullet, levels(this.data[,this.var]), sep = "") # Factor levels pulled from data set.
      mytable[(this.first_row+1):this.last_row, 1] <- this.text$Col1[-1]
      mytable[(this.first_row+1):this.last_row, 2] <- this.text$Col2[-1]
      mytable[this.first_row:this.last_row,"p-value"] <- this.text$Col3 # Writes even overall p-value.
    }

    # mytable[this.first_row,"p-value"] <- this.text$Col4 # p-value goes always into the same line than variable name/label.
    # if (this.n_rows == 1){ # All values go in a single row:
    #   mytable[this.first_row, 1] <- this.text$Col1
    #   mytable[this.first_row, 2] <- this.text$Col2
    #   mytable[this.first_row, 3] <- this.text$Col3
    #
    # } else { # More than one row (applies to factors only)
    #   rownames(mytable)[(this.first_row+1):this.last_row] <-
    #     paste(factor.level.bullet, names(this.text$Col1), sep = "") # rownames from table become names in extracted columns.
    #   mytable[(this.first_row+1):this.last_row, 1] <- this.text$Col1
    #   mytable[(this.first_row+1):this.last_row, 2] <- this.text$Col2
    #   mytable[(this.first_row+1):this.last_row, 3] <- this.text$Col3
    #}
  }

  return(mytable)

}


#######################
##### End of file #####
#######################
