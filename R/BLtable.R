############################################################################
###
###    Project: Tools/utilities for studies with 2 groups
###
### PI/Contact: Fridtjof Thomas
###
###    Purpose: Function to compile BL-table
###
###
###       Code: Fridtjof Thomas, 06/27/2016
###               Last modified: Under GIT version control.
###
############################################################################
###    History: >Short description of major changes to code, if applicable<
###
###
############################################################################
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



#' Synthetic data for compiling a table with 2 groups
#'
#' A dataset containing typical entries for baseline tables
#'
#' @format A data frame with 100 rows and 5 variables:
#' \describe{
#'  \item{ID}{Fictitious ID}
#'  \item{Group}{group membership - GroupA or GroupB (as text)}
#'  \item{Gender}{Fictitious gender as example of a binary variable (as text)}
#'  \item{Age}{Fictitious age in years as example of a continuous variable}
#'  \item{Some.measure}{Fictitious continuous measure with skewed distribution.}
#'  \item{Educ}{Fictitious education level as example of a factor (with 8 possibly unused factor levels)}
#' }
#' @source Synthetic data created by the package author
"example_data"

set.seed(356)
example_data <- data.frame(
  ID = paste("ID", formatC(1:100, width=3, flag="0"), sep=""),
  Group = factor(sample(rep(c("GroupA", "GroupB"), each = 50), size = 100, replace = FALSE)),
  Gender = factor(sample(c("male", "female"), size = 100, replace = TRUE, prob = c(0.3, 0.70))),
  Age = round(rnorm(100, 35, 4), digits = 2),
  Some.measure = rweibull(100, shape = 20, scale = 10),
  Educ = factor(sample(1:8, size = 100, replace = TRUE, prob = c(rep(0.10, 4), 0.30,  rep(0.10, 3))),
                levels = 1:8,
                labels = c(
                  "Did not finish elementary school",
                  "Finished middle school (8th grade)",
                  "Finished some high school",
                  "High school graduate or G.E.D",
                  "Vocational or training school after high school",
                  "Some College or Associate degree",
                  "College graduate or Baccalaureate Degree",
                  "Masters or Doctoral Degree (PhD, MD, JD, etc)"
                ),
                ordered = TRUE
                ),
  stringsAsFactors = FALSE
)


#' Description for compilation of table (participant data set)
#'
#' A dataset containing the variables and tests to be included in the baseline table
#'
#' @format A data frame with 3 rows and 5 columns:
#' \describe{
#'  \item{Variable}{Variable name}
#'  \item{BLtable}{Indicator whether to include in BL table}
#'  \item{Table.label}{Label to use in the created table}
#'  \item{Type}{What type of variable this is: Continuous, Dichotomous, or Factor?}
#'  \item{Test}{Test to be used for p-value: t-test, Wilcoxon, Chi-square, or Fisher?)}
#' }
#' @source Synthetic data created by the package author
"example_variables"
example_variables <- data.frame(
  Variable = c("Gender", "Age", "Some.measure", "Educ"),
  BLtable = 1,
  Table.label = c("Gender", "Age in years", "Some skewed measure", "Education"),
  Type = c("Dichotomous", "Continuous", "Continuous", "Factor"),
  Test = c("Chi-square", "t-test", "Wilcoxon", "Fisher"),
  stringsAsFactors = FALSE
  )

# Put these data sets into the right place in the right format:
devtools::use_data(example_data, example_variables, overwrite = TRUE)



#' Function to test the dichotomous variable identifying the two groups in the BL-table.
#'
#' @inheritParams BLtable
#' @return Prints warning message and/or stops processing with error message printed.
#' @examples
#' Called by other function - not intended to be called by user directly.
grouptest <- function(data, group.var){
  if (!(group.var %in% names(data))) {
    stop("The provided grouping variable does not exist in the provided data set.")
  }
  if (any(is.na(data[ , group.var]))) {
    warning("The grouping variable contains NAs and these rows will be ignored when compiling the table.")
  }
  if ( !(is.factor(data[ , group.var])) ) {
    stop("The grouping variable is not a factor and must be a formal factor in R.")
  }
  if (length(unique(data[ (!is.na(data[ , group.var])), group.var])) != 2) {
    stop("The grouping variable does not define two groups.")
  }

}



#' Function to gather information about the table before computing begins
#'
#' \code{BLtable.layout} is called by \code{\link{BLtable}} to gather information about the table before computing begins
#'
#' @inheritParams BLtable
#' @return Data frame containing layout information of the table to be computed
#'    having these columns:
#'  \describe{
#'  \item{Variable, Table.label, Type, Test, etc.}{Inherited from data set submitted as 'info'}
#'  \item{n_rows}{Number of rows in the table for the corresponding variable}
#'  \item{first_row}{First table row used for the corresponding variable}
#'  \item{last_row}{Last table row used for the corresponding variable}
#' }
#' @examples
#' Called by other function - not intended to be called by user directly.
#' @export
BLtable.layout <- function(data, info){
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



#' Function to compute and render text entries for BL-table
#'
#' @param this.data Data used for the computations
#' @param this.var Variable in the table (must exist in \code{this.data})
#' @param group.var The name of the variable (column) in \code{data} that partitions
#'    the data set into two groups.
#' @param this.GroupA Name (factor level) of 'Group A' in the table
#' @param this.GroupB Name (factor level) of 'Group B' in the table
#' @param this.type Variable type for table
#' @param this.test Test that determines p-value
#' @inheritParams BLtable
#' @return Output used by \code{BLtable()}
#' @examples
#' Called by other function - not intended to be called by user directly.
#' @export
table.text <- function(this.data, this.var, group.var, this.GroupA, this.GroupB,
                       this.type, this.test, sign.digits, sign.digits.prop, pvalue.digits, pvalue.cutoff, less.than.character){
  this.data.All <- this.data[ , this.var]
  this.data.GroupA <- this.data[this.data[ , group.var] == this.GroupA, this.var]
  this.data.GroupB <- this.data[this.data[ , group.var] == this.GroupB, this.var]

  if ((this.type == "Continuous") & (this.test %in% c("t-test", "Wilcoxon"))) {
    if (this.test == "t-test") {
      this.pvalue <- t.test(this.data.GroupA, this.data.GroupB, alternative = "two.sided")$p.value
    } else if (this.test == "Wilcoxon") {
      this.pvalue <- wilcox.test(this.data.GroupA, this.data.GroupB, alternative = "two.sided")$p.value
    } else {
      this.pvalue <- "?.???"
    }


    list(
      Col1 = paste(formatC(mean(this.data.All, na.rm=TRUE), digits=sign.digits, format="f"), " (",
                   formatC(sd(this.data.All, na.rm=TRUE), digits=sign.digits, format="f"), ")", sep = ""),
      Col2 =
        paste(formatC(mean(this.data.GroupA, na.rm=TRUE), digits=sign.digits, format="f"), " (",
              formatC(sd(this.data.GroupA, na.rm=TRUE), digits=sign.digits, format="f"), ")", sep = ""),
      Col3 =
        paste(formatC(mean(this.data.GroupB, na.rm=TRUE), digits=sign.digits, format="f"), " (",
              formatC(sd(this.data.GroupB, na.rm=TRUE), digits=sign.digits, format="f"), ")", sep = ""),
      Col4 = ifelse(this.pvalue < pvalue.cutoff,
                    paste(less.than.character, pvalue.cutoff, sep=""),
                    formatC(this.pvalue, digits=pvalue.digits, format="f"))
    )
  } else if ( (this.type %in% c("Dichotomous", "Factor")) & (this.test %in% c("Chi-square", "Fisher")) ) {

    this.table <- table(this.data[, this.var], this.data[,group.var])
    this.table.margin <- addmargins(this.table, margin = 2, FUN = sum, quiet = TRUE)
    this.table.prop <- (prop.table(this.table.margin, margin = 2)) * 100 # Relative counts in %.

    table.combined <- matrix(
      paste(as.numeric(this.table.margin), " (", formatC(as.numeric(this.table.prop), digits=sign.digits.prop, format="f"), ")", sep=""),
      nrow = dim(this.table.margin)[1],
      dimnames = dimnames(this.table.margin))

    this.pvalue <- ifelse(this.test == "Chi-square",
                          chisq.test(this.table)$p.value,
                          ifelse(this.test == "Fisher",
                                 fisher.test(this.table, simulate.p.value = TRUE, B = 500000)$p.value,
                                 "?.???"
                                 )
    )

    list(
      Col1 = table.combined[,"sum"],
      Col2 = table.combined[,this.GroupA],
      Col3 = table.combined[,this.GroupB],
      Col4 = ifelse(this.pvalue < pvalue.cutoff,
                    paste(less.than.character, pvalue.cutoff, sep=""),
                    formatC(this.pvalue, digits=pvalue.digits, format="f"))
    )
  } else { # Serves to catch errors:
    list(
      Col1 = "??",
      Col2 = "??",
      Col3 = "??",
      Col4 = "?.???"
    )
  }

}



#' Function to compile BL-table.
#'
#' @param data A data frame that contains the baseline (BL) data (one row per participant).
#' @param info A data frame with specifications for the BL-table. (See Examples below.)
#' @param group.var The name of the variable (column) in \code{data} that partitions
#'    the data set into two groups.
#' @param sign.digits Digits used for mean, standard deviation, etc.
#' @param sign.digits.prop Digits used for proportion of cases for factors.
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
#' BLtable(data = example_data, info = example_variables, group.var = "Group", test.input = TRUE)
#' @export
BLtable <- function(data, info, group.var, sign.digits=2, sign.digits.prop=1, pvalue.digits=3, pvalue.cutoff=0.001,
                    test.input = TRUE,
                    factor.level.bullet = "- ", less.than.character = "< ", linebreak.tag = ""){
  # Test input:
  if (test.input) { grouptest(data, group.var) }
  ### ADD TEST FOR FACTORS for all factors!

  # Create subset with existing group-information
  this.data <- data[ (!is.na(data[ , group.var])), ]
  if ( dim(this.data)[1] != dim(data)[1] ) {
    warning("Rows have been dropped due to missing information in the provided grouping-variable.")
  }

  # Compute layout information:
  this.layout <- BLtable.layout(this.data, info)

  this.GroupA <- levels(this.data[ ,group.var])[1]
  this.GroupB <- levels(this.data[ ,group.var])[2]

  # Set up the matrix that will hold the table information:
  mytable <- matrix(data = " ", nrow = max(this.layout$last_row), ncol = 4) # Filled with space that will not 'show' in table.
  colnames(mytable) <- c(
    paste("All ", linebreak.tag,"(N = ", sum(!is.na(this.data[ , group.var]), na.rm=TRUE), ")", sep=""),
    paste(this.GroupA, linebreak.tag, " (N = ", sum(this.data[ , group.var] == this.GroupA, na.rm=TRUE), ")", sep=""),
    paste(this.GroupB, linebreak.tag," (N = ", sum(this.data[ , group.var] == this.GroupB, na.rm=TRUE), ")", sep=""),
    "p-value"
  )
  rownames(mytable) <- 1:dim(mytable)[1]

  # Fill table with values variable by variable:
  for (l in 1:length(this.layout$Variable)){
    this.var <- this.layout$Variable[l]
    this.label <- this.layout$Table.label[l]
    this.type <- this.layout$Type[l]
    this.test <- this.layout$Test[l]
    this.n_rows <- this.layout$n_rows[l]
    this.first_row <- this.layout$first_row[l]
    this.last_row <- this.layout$last_row[l]


    this.text <- table.text(this.data, this.var, group.var, this.GroupA, this.GroupB,
                            this.type, this.test, sign.digits, sign.digits.prop, pvalue.digits, pvalue.cutoff, less.than.character)

    rownames(mytable)[this.first_row] <- this.label

    mytable[this.first_row,"p-value"] <- this.text$Col4 # p-value goes always into the same line than variable name/label.
    if (this.n_rows == 1){ # All values go in a single row:
      mytable[this.first_row, 1] <- this.text$Col1
      mytable[this.first_row, 2] <- this.text$Col2
      mytable[this.first_row, 3] <- this.text$Col3

    } else { # More than one row (applies to factors only)
      rownames(mytable)[(this.first_row+1):this.last_row] <-
        paste(factor.level.bullet, names(this.text$Col1), sep = "") # rownames from table become names in extracted columns.
      mytable[(this.first_row+1):this.last_row, 1] <- this.text$Col1
      mytable[(this.first_row+1):this.last_row, 2] <- this.text$Col2
      mytable[(this.first_row+1):this.last_row, 3] <- this.text$Col3
    }
  }

  return(mytable)

}




###########################################################################
### Basic idea of such a function "generateinputtable()" and first code ###
### version contributed by Tristan Hayes 7/26/16 ("create input csv.R") ###
###########################################################################
#' Function to create .
#'
#'\code{generateinputtable} takes an R data frame and creates table-input useful for \code{\link{BLtable}}.
#'
#' @param inputset A data frame that contains the baseline (BL) data (one row per participant).
#'    Intended use: same data set as used by \code{\link{BLtable}}.
#' @return A data frame useful to create table-input for for \code{\link{BLtable}}.
#' @examples
#' generateinputtable(example_data)
#' @export
#'
generateinputtable <- function(inputset) {


### These variabels: Variable BLtable  Table.label        Type       Test


  #Create List of Variables and Set Initial Values
  variable_list<-data.frame(names(inputset))
  colnames(variable_list)[1] <- "Variable"
  variable_list$Label <- variable_list$Variable
  variable_list$Type <- NA
  variable_list$Test <- NA
  variable_list$IgnorePvalue <- "No"

  #Go through list of variables and identify likely settings
  #Assumes first variable is a unique ID for all obs
  i <- 2
  for (i in 2:nrow(variable_list)) {
    #If the variable is a factor, then it first checks if dichotomous
    if (is.factor(inputset[[i]])) {

      if (nlevels(inputset[[i]])==2) {
        variable_list[i,"Type"] <- "Dichotomous"
        variable_list[i,"Test"] <- "Chi-square"
      }
      #when factors are greater than 2
      else {
        variable_list[i,"Type"] <- "Factor"
        variable_list[i,"Test"] <- "Fisher"
      }
    }

    #Last, assumes variable is continuous
    else {
      variable_list[i,"Type"] <- "Continuous"
      variable_list[i,"Test"] <- "t-test"
    }

    i+1
  }
  return(variable_list)
}



#######################
##### End of file #####
#######################
