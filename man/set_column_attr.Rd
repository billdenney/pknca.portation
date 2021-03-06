% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/general.R
\name{set_column_attr}
\alias{set_column_attr}
\alias{get_column_attr}
\title{Set attributes of columns of a data.frame (or similar object)}
\usage{
set_column_attr(.data, ..., .attr = "label", .allow_overwrite = TRUE,
  .allow_multiple_values = TRUE, .clear = FALSE)

get_column_attr(.data, .attr = "label")
}
\arguments{
\item{.data}{The data.frame or similar object}

\item{...}{A named list of column name = attribute value.}

\item{.attr}{The attribute name to set on the column}

\item{.allow_overwrite}{Allow the attribute value to be overwritten
if it already exists on the current column.}

\item{.allow_multiple_values}{Allow the same attribute value to exist
on multiple columns simultaneously (this is only checked for newly
set attributes; not checked for existing attributes).}

\item{.clear}{Remove the attribute from all columns (occurs before
any additional processing).}
}
\value{
Set: .data with the column attributes set and/or modified.
  Get: a list of column names and attribute values.
}
\description{
Set attributes of columns of a data.frame (or similar object)
}
\section{Functions}{
\itemize{
\item \code{get_column_attr}: Get the value of an attribute for all columns.
}}

