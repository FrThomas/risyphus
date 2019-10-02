################################################################################
###
###    Project: R package risyphus
###
### PI/Contact: Fridtjof Thomas
###
###    Purpose: Function to compile table (one group)
###
###       Code: Fridtjof Thomas, 07/10/2018
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



#' Function to gather information about the ONE-table before computing begins
#'
#' \code{ONEtable.layout} is called by \code{\link{ONEtable}} to gather information about the table before computing begins
#'
#' @inheritParams ONEtable
#' @return Data frame containing layout information of the table to be computed
#'    having these columns:
#'  \describe{
#'  \item{Variable, Table.label, Type, etc.}{Inherited from data set submitted as 'info'}
#'  \item{n_rows}{Number of rows in the table for the corresponding variable}
#'  \item{first_row}{First table row used for the corresponding variable}
#'  \item{last_row}{Last table row used for the corresponding variable}
#' }
#' @examples
#' \dontrun{ Called by other function - not intended to be called by user directly. }
#' @export
ONEtable.layout <- function(data, info){
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
      needed.rows <- length(levels(data[,this.variable])) + 1 # 1 for name and p-value.
      table.info[k, "n_rows"] <- needed.rows
      first_j <- last_j + 1 # Last row from variable just before the current one.
      last_j <- last_j + needed.rows
      table.info[k, "first_row"] <- first_j
      table.info[k, "last_row"] <- last_j
    }
  }
  return(table.info)
}



#' Function to compute and render text entries for ONE-table
#'
#' @param data Data used for the computations
#' @param this.var Variable in the table (must exist in \code{data})
#' @param this.type Variable type for table
#' @inheritParams ONEtable
#' @return Output used by \code{ONEtable()}
#' @examples
#' \dontrun{ Called by other function - not intended to be called by user directly. }
#' @export
ONEtable.text <- function(data, this.var, this.type,
                       sign.digits, sign.digits.prop){
  this.data.All <- data[ , this.var] # Results in vector for a dataframe but in dataframe for a dplyr tibble.

  if (this.type == "Continuous") {
    list(
      Col1 = paste(formatC(mean(this.data.All, na.rm=TRUE), digits=sign.digits, format="f"), " (",
                   formatC(stats::sd(this.data.All, na.rm=TRUE), digits=sign.digits, format="f"), ")", sep = "")
    )
  } else if ( (this.type %in% c("Dichotomous", "Factor")) ) {

    this.table <- table(data[, this.var])
    this.table.prop <- (prop.table(this.table)) * 100 # Relative counts in %.

    table.combined <- matrix(
      paste(as.numeric(this.table), " (", formatC(as.numeric(this.table.prop), digits=sign.digits.prop, format="f"), ")", sep=""),
      nrow = dim(this.table)[1],
      dimnames = dimnames(this.table))

    list(
      Col1 = table.combined
      )
  } else { # Serves to catch errors:
    list(
      Col1 = "??"
    )
  }

}



#' Function to compile ONE-table.
#'
#' @param data A data frame that contains the data (one row per participant).
#' @param info A data frame with specifications for the ONE-table. (See Examples below.)
#' @param sign.digits Digits used for mean, standard deviation, etc.
#' @param sign.digits.prop Digits used for proportion of cases for factors (and missing entries, if requested).
#' @param test.input Whether the input is tested for logical consistency.  Warning: testing is not
#'    comprehensive.  Setting \code{test.input = FALSE} will bypass logical tests.
#' @param show.missing.info If \code{show.missing.info = TRUE} information about
#'    missing information in a variable is added to the table. \code{show.missing.info = FALSE}
#'    suppresses that information.
#' @param factor.level.bullet The character to be prepended to factor levels (listing). The default
#'    is \code{factor.level.bullet = "- "} (hyphen/minus) but it can be reset to any character string. If the result is
#'    passed on to html-rendering an en-dash can be produced by \code{factor.level.bullet = "&#8211; "} (HTML-code for the en-dash).
#'    Other useful options for (subsequent) HTML-rendering: \code{factor.level.bullet = "&nbsp; &nbsp; "} forces non-breaking space and effectively
#'    leads to indentation of factor levels; \code{factor.level.bullet = "&#187; "} prints right double angle quotes in HTML.
#' @param linebreak.tag Can be used to insert a linebreak-command in the table headers to bring the counts to a new line.
#'    The default is \code{linebreak.tag = ""} effectively not inserting any such command.  For subsequent HTML-rendering
#'    \code{linebreak.tag = "<br>"} (HTML tag to insert a single line break) is useful.
#' @return An R character-matrix containing the compiled table information.
#' @examples
#' ONEtable(data = example_data, info = example_variables, show.missing.info = TRUE)
#' @export
ONEtable <- function(data, info, sign.digits=2, sign.digits.prop=1,
                    show.missing.info = TRUE,
                    test.input = FALSE,
                    factor.level.bullet = "- ", linebreak.tag = ""){

  # Convert tibbles to regular data frames without need to load package "tibbles":
  # Note: a tibble tests TRUE for as.data.frame(tbl).
  if ("tbl" %in% class(data)) { data <- as.data.frame(data) }

  # Test input:
  if (test.input) {  }
  ### ADD TEST FOR FACTORS for all factors!

  # Compute layout information:
  this.layout <- ONEtable.layout(data, info)

  # Set up the matrix that will hold the table information:
  mytable <- matrix(data = " ", nrow = max(this.layout$last_row), ncol = 1) # Filled with space that will not 'show' in table.
  colnames(mytable) <- c(
    paste("All ", linebreak.tag,"(N = ", dim(data)[1], ")", sep="")
  )
  rownames(mytable) <- 1:dim(mytable)[1]

  # Fill table with values variable by variable:
  for (l in 1:length(this.layout$Variable)){
    this.var <- this.layout$Variable[l]
    if (show.missing.info){ # Add info about missing entries.
      # Compile information about missing values.
      n_recorded <- sum(!is.na(data[, this.var]))
      n_missing <- sum(is.na(data[, this.var]))
      percent_missing <- formatC(100*(n_missing/(n_missing + n_recorded)),
                                 digits=sign.digits.prop, format = "f")
      this.label  <- paste(this.layout$Table.label[l],
                           " (", n_missing, " of ", n_missing + n_recorded, " missing; ",
                           formatC(percent_missing, digits=sign.digits.prop, format="f"), "%)", sep = "")
    } else {
      this.label <- this.layout$Table.label[l] # No info added.
    }

    this.type <- this.layout$Type[l]
    this.n_rows <- this.layout$n_rows[l]
    this.first_row <- this.layout$first_row[l]
    this.last_row <- this.layout$last_row[l]

    this.text <- ONEtable.text(data, this.var, this.type,
                               sign.digits, sign.digits.prop)

    rownames(mytable)[this.first_row] <- this.label

    if (this.n_rows == 1){ # All values go in a single row:
      mytable[this.first_row, 1] <- this.text$Col1
      } else { # More than one row (applies to factors only)
      rownames(mytable)[(this.first_row+1):this.last_row] <-
        paste(factor.level.bullet, rownames(this.text$Col1), sep = "") # rownames from table become names in extracted columns.
      mytable[(this.first_row+1):this.last_row, 1] <- this.text$Col1
      }
    }

  return(mytable)

}


#######################
##### End of file #####
#######################
