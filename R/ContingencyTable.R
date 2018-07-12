################################################################################
###
###    Project: R package risyphus
###
### PI/Contact: Fridtjof Thomas
###
###    Purpose: Function to compile a continguency table with margins and
###             proportions
###
###       Code: Fridtjof Thomas, 07/11/2018
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



#' Function to compile a contingency table.
#'
#' @param data A data frame that contains the data.
#' @param row.var A factor with factor levels defining the rows of the contingency table.
#' @param col.var A factor with factor levels defining the columns of the contingency table.
#' @param show.props Which proportions for cell counts to show.
#'        \itemize{
#'         \item{If \code{show.props = "none"}, no proportions are shown;}
#'         \item{if \code{show.props = "cell"}, proportions are relative to total;}
#'         \item{if \code{show.props = "row"}, proportion is relative to row-totals
#'          (no proportions of the column totals are shown to facilitate reading of the table);}
#'         \item{if \code{show.props = "col"}, proportion is relative to column-totals
#'          (no proportions of the row totals are shown to facilitate reading of the table).}
#'        }
#' @param sign.digits.prop Number of digits for the displayed proportion of cell counts.
#' @param test.input Whether the input is tested for logical consistency.  Warning: testing is not
#'    comprehensive.  Setting \code{test.input = FALSE} will bypass logical tests.
#' @return An R character-matrix containing the compiled contingency table information.
#' @examples
#' CONtable(example_data, row.var = "Educ", col.var = "Gender", show.props = "row", sign.digits.prop=1)
#' @export
CONtable <- function(data, row.var, col.var, show.props = "cell", sign.digits.prop=1, test.input = TRUE){
  # Test input:
  if (test.input) {  }
  ### ADD TEST FOR FACTORS for all factors!

  table.center <- table(data[, row.var], data[,col.var])
  if (show.props == "none"){ # No proportions are added.
    table.expanded <- addmargins(table.center)
    table.render <- matrix(
      paste(table.expanded),
      nrow = dim(table.expanded)[1],
      dimnames = dimnames(table.expanded))
  } else if (show.props == "cell") { # Proportion with respect to total.
    table.expanded <- addmargins(table.center)
    this.table.prop <- (prop.table(table.expanded)*4) * 100 # *4: true cells, plus 2 x margins, plus total
    table.render <- matrix(
      paste(as.numeric(table.expanded), " (", formatC(as.numeric(this.table.prop), digits=sign.digits.prop, format="f"), "%)", sep=""),
      nrow = dim(table.expanded)[1],
      dimnames = dimnames(table.expanded))
  } else if (show.props == "row") { # Proportions with respect to row-totals.
    table.expanded <- addmargins(table.center)
    this.table.prop <- prop.table(table.center, margin = 1)
    this.table.prop <- cbind(this.table.prop,  rowSums(this.table.prop)) * 100
    # Stitch together with main part (add last row with col. totals afterwards):
    this.main <- table.expanded[1:(dim(table.expanded)[1] - 1), ]
    table.render <- matrix(
      paste(as.numeric(this.main), " (", formatC(as.numeric(this.table.prop), digits=sign.digits.prop, format="f"), "%)", sep=""),
      nrow = dim(this.main)[1],
      dimnames = dimnames(this.main))
    table.render <- rbind(table.render, table.expanded[dim(table.expanded)[1],])
    rownames(table.render)[dim(table.render)[1]] <- "Sum"
    } else if (show.props == "col") { # Proportions with respect to col-totals.
    table.expanded <- addmargins(table.center)
    this.table.prop <- prop.table(table.center, margin = 2)
    this.table.prop <- rbind(this.table.prop,  colSums(this.table.prop)) * 100
    # Stitch together with main part (add last col with row totals afterwards):
    this.main <- table.expanded[, 1:(dim(table.expanded)[2] - 1)]
    table.render <- matrix(
      paste(as.numeric(this.main), " (", formatC(as.numeric(this.table.prop), digits=sign.digits.prop, format="f"), "%)", sep=""),
      nrow = dim(this.main)[1],
      dimnames = dimnames(this.main))
    table.render <- cbind(table.render, table.expanded[,dim(table.expanded)[2]])
    colnames(table.render)[dim(table.render)[2]] <- "Sum"

    }

  return(table.render)
}


#######################
##### End of file #####
#######################
