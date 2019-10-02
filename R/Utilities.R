################################################################################
###
###    Project: R package risyphus
###
### PI/Contact: Fridtjof Thomas
###
###    Purpose: Regularly used functions
###
###       Code: Fridtjof Thomas, 07/28/2016
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




#' Function to summarize data in a data frame.
#'
#'\code{ListVariables} takes a data frame and lists all existing variables incl. summary descriptions.
#'
#' @param data A data frame.
#' @return A character matrix useful to print directly or as input to R-Markdown (e.g., using the kable-function) listing
#'  \describe{
#'  \item{Variable}{Variable name}
#'  \item{Missing entries (count)}{The number of (formally) missing values (note: 'coded missingness' such a -99 would not be counted as missing
#'        and would be an acceptable input value).}
#'  \item{Unique entries (count incl missing if applicable)}{The number of distinct entry-values, incl. formally missing as one such entry, if applicable.}
#'  \item{List of actual entries}{Listing actually occuring distinct values in order (typicaly alphabetic order).
#'        If there are 11 or more distinct entries the first and last three are printed separated by '(...)' to indicate additional entries in between.}
#' }
#' @examples
#' ListVariables(data = example_data)
#' @export
#'
ListVariables <- function(data){

  # Convert tibbles to regular data frames without need to load package "tibbles":
  # Note: a tibble tests TRUE for as.data.frame(tbl).
  if ("tbl" %in% class(data)) { data <- as.data.frame(data) }

  this.data <- data
  mytable <- data.frame(Variable=names(this.data), Missing=NA, Not.Missing=NA, N.unique=NA, Values=NA)

  for(var in mytable$Variable){
    mytable[mytable$Variable==var,"Missing"] <- sum(is.na(this.data[,var]))
    mytable[mytable$Variable==var,"Not.Missing"] <- sum(!is.na(this.data[,var]))
    mytable[mytable$Variable==var,"N.unique"] <- length(unique(this.data[,var])) # Counts NA as a unique value.
    if (mytable[mytable$Variable==var,"N.unique"] <= 10) {
      mytable[mytable$Variable==var,"Values"]  <- paste(sort(unique(this.data[,var])), collapse=", ")
    } else{
      #mytable[mytable$Variable==var,"Values"]  <- "To be implemented
      mytable[mytable$Variable==var,"Values"]  <- paste( paste(utils::head(sort(unique(this.data[,var])), n=3), collapse=", "),
                                                         ", (...), ",
                                                         paste(utils::tail(sort(unique(this.data[,var])), n=3), collapse=", "),
                                                         sep="")

    }

  }

  ## Make matrix (character) out of table to add dimnames that can be passed to xtable():
  mytable.text <- as.matrix(mytable)  # Should coerces everything into text if some text is contained in the dataframe.
  dimnames(mytable.text) <- list(row.names(mytable), # Keeps row-numbers from the data frame.
                                 c("Variable", "Missing entries (count)", "Existing entries (count)", "Unique entries (count incl missing if applicable)", "List of actual entries"))

  return(mytable.text)
}



#' Function to test the identity of variable names in two data sets.
#'
#'\code{CheckNames} takes two data frames and compares the included variable names.
#'
#' @param data1 A data frame.
#' @param data2 A data frame.
#' @param identical.order Whether the variables/columns are required to be in the same position
#'    in both data sets.  The default is \code{identical.order = TRUE}, if \code{identical.order = FALSE}
#'    the test is with respect to identical spelling only and irrespective of the relative position in the respective data frames.
#' @param names.data1.only Not yet implemented but intended as follows: If \code{names.data1.only = TRUE} testing is only with respect to the existence and spelling
#'    of all variables in \code{data1} (useful for testing if a new version of a data set (\code{data2}) contains all of the
#'    variables in an earlier version (\code{data1}) where it is understood that new variables may have been added to the new version).
#' @return \code{TRUE} or \code{FALSE} depending on the testing result.
#' @examples
#' CheckNames(data1 = example_data, data2 = example_data) # Trivially \code{TRUE}, of course.
#' @export
#'
CheckNames <- function(data1, data2, identical.order = TRUE, names.data1.only = FALSE){
  if (!identical.order) {
    if (all( (names(data1) %in% names(data2)) &  (names(data2) %in% names(data1)))){
      return(TRUE)
    } else{
      return(FALSE)
    }
  }
  else { # In all other cases names and position have to be identical.
    if (all( names(data1) == names(data2)) ){
      return(TRUE)
    } else{
      return(FALSE)
    }
  }
}







#' Function to render BMI-groups based on BMI-values.
#'
#'\code{BMIgroup} takes a vector with BMI values and returns a vector with resulting BMI-groups (as ordered factor).
#'
#' @param BMI.values The input vector containing BMI-values.
#' @return A vector of BMI groups as ordered factor.
#' @examples
#' BMIgroup(BMI.values = c(18.6, 22.5, 14.3, 28.4, 38.3))
#' @export
#'
BMIgroup <- function(BMI.values){
  if (!(is.vector(BMI.values))) {
    stop("The provided input in function BMIgroup() is not a vector and must be a vector.")
  }
  if (!(is.numeric(BMI.values))) {
    stop("The provided input in function BMIgroup() is not numeric and must be numeric.")
  }

  myBMI <- ifelse(is.na(BMI.values), NA,
                  ifelse(BMI.values < 18.5, 1,
                         ifelse(
                           BMI.values < 25, 2,
                           ifelse(
                             BMI.values < 30, 3,
                             4
                           )
                         )
                  )
  )

  myBMI <-  factor(myBMI,
                   levels = c(1, 2, 3, 4),
                   labels = c("Underweight (below 18.5)", "Normal (18.5 - 24.9)",
                              "Overweight (25.0 - 29.9)", "Obese (exceeding 30)"),
                   ordered = TRUE
  )

   return(myBMI)

}


#######################
##### End of file #####
#######################
