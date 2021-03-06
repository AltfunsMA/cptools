% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sequence_checks.R
\name{find_acno_incomplete}
\alias{find_acno_incomplete}
\title{Find gaps in AC No. sequence in Indian states}
\usage{
find_acno_incomplete(
  df,
  checkCols = NULL,
  comparison = `==`,
  step = NULL,
  verbose = FALSE
)
}
\arguments{
\item{df}{A dataframe. The default assumes one column for state names and one
column for AC numbers. The state column name is automatically detected and
the AC number column name assumed to be either AC_NO or ac_no.}

\item{checkCols}{To override defaults, provide a character vector with
exactly two strings. The first names the grouping variable, and the second,
the accompanying numeric sequence variable.}

\item{comparison}{A relational operator for the check, with back-ticks.}

\item{step}{Step to be used in comparison. Defaults to the difference 
between the first and second elements of the vector.}

\item{verbose}{Should messages be reported?}
}
\value{
If any gaps found, the function returns a data frame with two rows before and one after the gap in the
  sequence, extracted by group. If no gaps found, NULL invisibly.
}
\description{
wrapper around \code{find_seq_incomplete} with additional checks for
  use with (Indian) Assembly Constituency data

  Identifies **gaps** only, if the sequence is broken because of a
  duplication, use \code{duplicheck}.
}
