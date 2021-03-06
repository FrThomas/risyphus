% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ContingencyTable.R
\name{CONtable}
\alias{CONtable}
\title{Function to compile a contingency table.}
\usage{
CONtable(data, row.var, col.var, show.props = "cell",
  sign.digits.prop = 1, test.input = TRUE)
}
\arguments{
\item{data}{A data frame that contains the data.}

\item{row.var}{A factor with factor levels defining the rows of the contingency table.}

\item{col.var}{A factor with factor levels defining the columns of the contingency table.}

\item{show.props}{Which proportions for cell counts to show.
\itemize{
 \item{If \code{show.props = "none"}, no proportions are shown;}
 \item{if \code{show.props = "cell"}, proportions are relative to total;}
 \item{if \code{show.props = "row"}, proportion is relative to row-totals
  (no proportions of the column totals are shown to facilitate reading of the table);}
 \item{if \code{show.props = "col"}, proportion is relative to column-totals
  (no proportions of the row totals are shown to facilitate reading of the table).}
}}

\item{sign.digits.prop}{Number of digits for the displayed proportion of cell counts.}

\item{test.input}{Whether the input is tested for logical consistency.  Warning: testing is not
comprehensive.  Setting \code{test.input = FALSE} will bypass logical tests.}
}
\value{
An R character-matrix containing the compiled contingency table information.
}
\description{
Function to compile a contingency table.
}
\examples{
CONtable(example_data, row.var = "Educ", col.var = "Gender", show.props = "row", sign.digits.prop=1)
}
