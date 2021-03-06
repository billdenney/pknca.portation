% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.SDTM.R
\name{as.PKNCAdose.SDTM_EX}
\alias{as.PKNCAdose.SDTM_EX}
\title{Convert an SDTM_PC object to a PKNCAconc object}
\usage{
as.PKNCAdose.SDTM_EX(x, ..., fun_time_start_nominal = sdtm_time_start_nominal,
  fun_time_end_nominal = sdtm_time_end_nominal,
  fun_time_start_actual = NULL, fun_time_end_actual = NULL)
}
\arguments{
\item{x}{The SDTM object to convert}

\item{...}{arguments passed to PKNCAdose}

\item{fun_time_start_nominal, fun_time_end_nominal}{The function to
calculate the nominal start and end times.}

\item{fun_time_start_actual, fun_time_end_actual}{The function to
calculate the actual start and end times.}
}
\value{
A PKNCAdose object
}
\description{
Convert an SDTM_PC object to a PKNCAconc object
}
\seealso{
\code{\link{as.PKNCAconc.SDTM_PC}}
}
