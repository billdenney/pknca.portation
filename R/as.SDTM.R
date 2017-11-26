#' Convert an object into an SDTM object
#'
#' @param x The object to convert
#' @return An object with the same class and classes of the SDTM domain
#'   (like "SDTM_PC") and "SDTM" classes added.
#' @export
as.SDTM <- function(x, ...)
  UseMethod("as.SDTM")

#' @rdname as.SDTM
#' @export
as.SDTM.data.frame <- function(x, ...) {
  detect_SDTM(x)
}

#' @rdname as.SDTM
#' @export
as.SDTM.tbl_df <- function(x, ...) {
  detect_SDTM(x)
}

#' Detect the SDTM domain for an object and add that class to the class
#' of the object.
#'
#' @param x The object to detect using the "DOMAIN" column in the data
#' @return The same object with the SDTM domain and SDTM classes added.
detect_SDTM <- function(x) {
  if ("DOMAIN" %in% names(x)) {
    u_domain <- unique(x[["DOMAIN"]])
    if (length(u_domain) != 1) {
      stop("DOMAIN column exists, but it has multiple values")
    }
  } else {
    stop("DOMAIN column not found")
  }
  class(x) <- c(paste("SDTM", u_domain, sep="_"), "SDTM", class(x))
  x
}

#' Calculate the nominal time from SDTM data
#'
#' @param x The object to extract the nominal time from
#' @param col_day_suffix The suffix (after the domain) for the day
#'   column (e.g. "PCDY")
#' @param col_time_suffix The suffix (after the domain) for the nominal
#'   time numeric column
#' @return The vector of numeric times.
#' @export
sdtm_time_nominal <- function(x, col_day_suffix="DY", col_time_suffix="TPTNUM") {
  pattern_sdtm_domain <- "^SDTM_([A-Z]+)$"
  sdtm_domain <-
    grep(pattern_sdtm_domain, class(x), value=TRUE) %>%
    gsub(x=., pattern=pattern_sdtm_domain, replacement="\\1")
  col_day <- paste0(sdtm_domain, col_day_suffix)
  col_time_num <- paste0(sdtm_domain, col_time_suffix)
  day <- x[[col_day]]
  time <- x[[col_time_num]]
  time[is.na(time)] <- 0
  (day - 1)*24 + time
}

#' @describeIn sdtm_time_nominal Calculate the nominal start time
#' @export
sdtm_time_start_nominal <- function(x, col_day_suffix="STDY", col_time_suffix="TPTNUM") {
  sdtm_time_nominal(x, col_day_suffix=col_day_suffix, col_time_suffix=col_time_suffix)
}
#' @describeIn sdtm_time_nominal Calculate the nominal end time
#' @export
sdtm_time_end_nominal <- function(x, col_day_suffix="ENDY", col_time_suffix="TPTNUM") {
  sdtm_time_nominal(x, col_day_suffix=col_day_suffix, col_time_suffix=col_time_suffix)
}

#' Convert an SDTM_PC object to a PKNCAconc object
#' 
#' @param x The SDTM object to convert
#' @param ... arguments passed to PKNCAconc
#' @param fun_time_nominal The function to calculate the nominal times.
#' @param fun_time_actual The function to calculate the actual times.
#' @return A PKNCAconc object
#' @seealso \code{\link{as.PKNCAdose.SDTM_EX}}
#' @export
as.PKNCAconc.SDTM_PC <- function(x, ...,
                                 fun_time_nominal=sdtm_time_nominal,
                                 fun_time_actual=NULL) {
  x$exclude <- NA_character_
  x$exclude[x$PCSTAT %in% "NOT DONE"] <- "NOT DONE"
  # assign zero to apparent BLQ
  x$PCSTRESN[is.na(x$PCSTRESN) & !(x$PCORRES %in% "")] <- 0
  x$TIME_NOMINAL <- fun_time_nominal(x)
  if (any(mask_nominal <- is.na(x$TIME_NOMINAL) & is.na(x$exclude))) {
    stop("Error assigning nominal times.  The following rows are NA but not excluded:\n  ",
         paste(which(mask_nominal), collapse=", "))
  }
  if (!is.null(fun_time_actual)) {
    x$TIME_ACTUAL <- fun_time_actual(x)
  }
  PKNCAconc(data=x, formula=PCSTRESN~TIME_NOMINAL|STUDYID+USUBJID,
            subject="USUBJID", exclude="exclude", ...)
}

#' Convert an SDTM_PC object to a PKNCAconc object
#'
#' @param x The SDTM object to convert
#' @param ... arguments passed to PKNCAconc
#' @param fun_time_start_nominal,fun_time_end_nominal The function to
#'   calculate the nominal start and end times.
#' @param fun_time_start_actual,fun_time_end_actual The function to
#'   calculate the actual start and end times.
#' @return A PKNCAdose object
#' @seealso \code{\link{as.PKNCAconc.SDTM_PC}}
#' @export
as.PKNCAdose.SDTM_EX <- function(x, ...,
                                 fun_time_start_nominal=sdtm_time_start_nominal,
                                 fun_time_end_nominal=sdtm_time_end_nominal,
                                 fun_time_start_actual=NULL,
                                 fun_time_end_actual=NULL) {
  x$exclude <- NA_character_
  x$exclude[x$EXSTAT %in% "NOT DONE"] <- "NOT DONE"
  x$TIME_START_NOMINAL <- fun_time_start_nominal(x)
  x$TIME_END_NOMINAL <- fun_time_end_nominal(x)
  if (any(mask_nominal <- is.na(x$TIME_START_NOMINAL) & is.na(x$exclude))) {
    stop("Error assigning nominal times.  The following rows are NA but not excluded:\n  ",
         paste(which(mask_nominal), collapse=", "))
  }
  if (!is.null(fun_time_start_actual)) {
    x$TIME_START_ACTUAL <- fun_time_start_actual(x)
  } else {
    x$TIME_START_ACTUAL <- NA_real_
  }
  if (!is.null(fun_time_end_actual)) {
    x$TIME_END_ACTUAL <- fun_time_end_actual(x)
  } else {
    x$TIME_END_ACTUAL <- NA_real_
  }
  x$DURATION_NOMINAL <- x$TIME_END_NOMINAL - x$TIME_START_NOMINAL
  x$DURATION_ACTUAL <- x$TIME_END_ACTUAL - x$TIME_START_ACTUAL
  PKNCAdose(data=x, formula=EXDOSE~TIME_START_NOMINAL|STUDYID+USUBJID,
            exclude="exclude", route="EXROUTE", duration="DURATION_NOMINAL", ...)
}
