#' Convert a PKNCA object into a NONMEM-ready dataset
#' 
#' @param object The object to make into a NONMEM-ready dataset.  To 
#'   ensure consistent output of reset data, it's preferred to provide a
#'   PKNCAdata object.
#' @param ... Ignored
#' @param conc_cmt_map,dose_cmt_map A data.frame (or similar object) 
#'   that indicates the mapping between concentration or dosing data and
#'   compartment names.  See details for more information.
#' @return A data.frame or similar ready for export to a .csv file as a 
#'   NONMEM-ready dataset with an attribute "name_map" for mapping the 
#'   data.frame names to NONMEM names.
#' @details The following "name_map" will be added:
#'   
#'   For every data type:
#'   
#'   \itemize{
#'    \item{ID}{the PKNCA subject}
#'    \item{TIME}{the time (right hand side) column}
#'    \item{EVID}{0 for PKNCAconc, 1 for PKNCAdose, 3 for reset}
#'    \item{EXCLUDETEXT}{the text of the exclude column; this will be
#'      converted to a factor and put into the EXCLUDENUM column on
#'      conversion to a data.frame (and writing to a file)}
#'   }
#'   
#'   For concentration data, name mapping for every data type plus:
#'   
#'   \itemize{
#'    \item{DV}{concentration measurement (or generally, the independent variable for PKNCAconc}
#'   }
#'   
#'   For dose data, name mapping for every data type plus:
#'   
#'   \itemize{
#'    \item{AMT}{amount of the dose for PKNCAdose}
#'    \item{RATE}{the rate of dosing for PKNCAdose}
#'    \item{II}{interdose interval is set to 0}
#'    \item{ADDL}{additional dose count is set to 0}
#'   }
#'   
#'   \code{conc_cmt_map} and \code{dose_cmt_map} will use the 
#'   \code{dplyr::left_join} function to combine with the data, so 
#'   matching columns will define the mapping.  One column must be named
#'   "CMT" in the data.frame.
#'   
#' @export
as.NONMEMdata <- function(object, ...)
  UseMethod("as.NONMEMdata")

#' @describeIn as.NONMEMdata Convert a PKNCAdata object to NONMEM data
#' @export
as.NONMEMdata.PKNCAdata <- function(object, ..., conc_cmt_map, dose_cmt_map) {
  conc <- as.NONMEMdata(object$conc, conc_cmt_map=conc_cmt_map)
  dose <- as.NONMEMdata(object$dose, dose_cmt_map=dose_cmt_map)
  resets <-
    full_join(as.NONMEMreset(object$conc),
              as.NONMEMreset(object$dose))
  # Dose may have a subset of the conc groups, so merge the conc groups
  # to potentially expand the dose data.
  dose <-
    left_join(dose, unique(getGroups(object$conc)))
  ret <- rbind.NONMEMdata(conc, dose, resets)
  class(ret) <- unique(c("NONMEMdata_data", "NONMEMdata", class(ret)))
  ret
}

#' @describeIn as.NONMEMdata Convert a PKNCAconc object to NONMEM data
#' @export
as.NONMEMdata.PKNCAconc <- function(object, ..., conc_cmt_map) {
  if (!is.null(conc_cmt_map)) {
    if (!("CMT" %in% names(conc_cmt_map))) {
      stop("conc_cmt_map must have a column named 'CMT'")
    } else if (any(is.na(conc_cmt_map$CMT))) {
      stop("CMT values cannot be NA")
    }
  }
  # Extract the column names of interest
  parsedForm <- parseFormula(object, require.two.sided=FALSE)
  name_map <- c(ID=object$subject,
                DV=all.vars(parsedForm$lhs),
                TIME=all.vars(parsedForm$rhs),
                EXCLUDETEXT=object$exclude,
                EVID="EVID")
  ret <- object$data
  if (!is.null(conc_cmt_map)) {
    if (length(intersect(names(object$data), names(conc_cmt_map)))) {
      # If there are overlapping names, use them for the join
      ret <- left_join(ret, conc_cmt_map)
    } else if (nrow(conc_cmt_map) == 1) {
      # Otherwise, bolt on the conc_cmt_map
      ret <- cbind(ret, conc_cmt_map)
    } else {
      stop("conc_cmt_map must either have shared names with the concentration data or be a single row.")
    }
    if (any(is.na(ret$CMT))) {
      stop("Incomplete concentration compartment map, some rows are missing CMT value")
    }
  }
  ret$EVID <- 0
  attr(ret, "name_map") <- name_map
  attr(ret, "groups") <- all.vars(parsedForm$groups)
  class(ret) <- c("NONMEMdata_conc", "NONMEMdata", class(ret))
  ret
}

#' @describeIn as.NONMEMdata Convert a PKNCAdose object to NONMEM data
#' @export
as.NONMEMdata.PKNCAdose <- function(object, ..., dose_cmt_map) {
  if (!is.null(dose_cmt_map)) {
    if (!("CMT" %in% names(dose_cmt_map))) {
      stop("dose_cmt_map must have a column named 'CMT'")
    } else if (any(is.na(dose_cmt_map$CMT))) {
      stop("CMT values cannot be NA")
    }
  }
  # Extract the column names of interest
  parsedForm <- parseFormula(object, require.two.sided=FALSE)
  name_map <- c(ID=object$subject,
                AMT=all.vars(parsedForm$lhs),
                RATE="RATE",
                II="II",
                ADDL="ADDL",
                TIME=all.vars(parsedForm$rhs),
                EXCLUDETEXT=object$exclude,
                EVID="EVID")
  # While AMT and TIME are not required for PKNCA, they are required here
  if (!("AMT" %in% names(name_map))) {
    stop("Cannot generate a NONMEMdata object when the dose amount is not given")
  } else if (name_map["AMT"] == ".") {
    stop("Cannot generate a NONMEMdata object when the dose amount is a period ('.')")
  }
  if (!("TIME" %in% names(name_map))) {
    # I don't think that this can happen
    stop("Cannot generate a NONMEMdata object when the time is not given")
  } else if (name_map["TIME"] == ".") {
    stop("Cannot generate a NONMEMdata object when the time is a period ('.')")
  }
  ret <- object$data
  if (!is.null(dose_cmt_map)) {
    if (length(intersect(names(object$data), names(dose_cmt_map)))) {
      # If there are overlapping names, use them for the join
      ret <- left_join(ret, dose_cmt_map)
    } else if (nrow(dose_cmt_map) == 1) {
      # Otherwise, bolt on the dose_cmt_map
      ret <- cbind(ret, dose_cmt_map)
    } else {
      stop("dose_cmt_map must either have shared names with the concentration data or be a single row.")
    }
    if (any(is.na(ret$CMT))) {
      stop("Incomplete concentration compartment map, some rows are missing CMT value")
    }
  }
  ret$EVID <- 1
  ret$II <- 0
  ret$ADDL <- 0
  # Calculate the dosing rate
  duration <- ret[[object$columns$duration]]
  ret$RATE <- 0
  mask_rate <- !is.na(duration) & duration > 0
  if (any(mask_rate)) {
    ret$RATE[mask_rate] <- ret[[name_map["AMT"]]][mask_rate]/duration[mask_rate]
  }
  # A 0 or missing for AMT is not valid (no dose); filter those out
  mask_amt_0 <- ret[[name_map["AMT"]]] %in% c(NA, 0)
  if (any(mask_amt_0)) {
    ret <- ret[!mask_amt_0,,drop=FALSE]
  }
  attr(ret, "name_map") <- name_map
  attr(ret, "groups") <- all.vars(parsedForm$groups)
  class(ret) <- c("NONMEMdata_dose", "NONMEMdata", class(ret))
  ret
}

as.NONMEMdata.NULL <- function(object, ...) {
  NULL
}

#' Generate one NONMEM reset record for each grouping in a data set.
#' 
#' @param object The object to use as the source for the reset record.
#' @return A data.frame or similar object ready for 
#' 
#'   \itemize{
#'     \item{ID}{the PKNCA subject}
#'     \item{TIME}{\code{NA_real_}}
#'     \item{EVID}{3 for all records (indicating reset)}
#'     \item{EXCLUDETEXT}{\code{NA_character_}}
#'   }
#' @export
as.NONMEMreset <- function(object, ...)
  UseMethod("as.NONMEMreset")

as.NONMEMreset.NONMEMdata <- function(object, ...) {
  name_map_orig <- attr(object, "name_map")
  id_col <- na.omit(name_map_orig["ID"])
  exclude_col <- name_map_orig["EXCLUDETEXT"]
  time_col <- attr(object, "name_map")["TIME"]
  ret <- unique(object[,
                       unique(c(id_col, attr(object, "groups"))),
                       drop=FALSE])
  ret[[name_map_orig["TIME"]]] <- NA_real_
  ret[[name_map_orig["EVID"]]] <- 3
  ret[[name_map_orig["EXCLUDETEXT"]]] <- NA_character_
  attr(ret, "name_map") <- c(ID=id_col,
                             name_map_orig[c("TIME", "EVID", "EXCLUDETEXT")])
  attr(ret, "groups") <- attr(object, "groups")
  # Inherit the other classes from the original object
  class(ret) <- unique(c("NONMEMdata_reset", class(object)))
  ret
}

#' @describeIn Generate reset records for objects that can be made into
#'   NONMEMdata objects
#' @export
as.NONMEMreset.default <- function(object, ...) {
 as.NONMEMreset(as.NONMEMdata(object, conc_cmt_map=NULL, dose_cmt_map=NULL), ...)
}

#' @describeIn Returns \code{NULL} (typically in case dose is not
#'   present)
#' @export
as.NONMEMreset.NULL <- function(object, ...) {
  NULL
}

#' Combine multiple NONMEMdata tables while matching/maintaining the
#' "name_map" and "groups" attributes
#' 
#' @param ... values to combine
#' @param deparse.level Ignored
#' @return A new NONMEMdata object with the combined name_maps from the
#'   source.
#' @export
rbind.NONMEMdata <- function(..., deparse.level=0) {
  name_map <-
    lapply(list(...),
           attr,
           which="name_map")
  groups <-
    unique(
      do.call(
        c,
        lapply(list(...),
               attr,
               which="groups")))
  # Ensure that the name maps are compatible
  name_map_master <- c()
  for (name_map_current in name_map) {
    # Ensure that overlapping names match
    for (mapping in intersect(names(name_map_master), names(name_map_current))) {
      if (!identical(name_map_master[[mapping]], name_map_current[[mapping]])) {
        stop(mapping, " mapping does not match between all arguments.")
      }
    }
    # Add non-overlapping maps
    mapping_update <- setdiff(names(name_map_current), names(name_map_master))
    name_map_master[mapping_update] <- name_map_current[mapping_update]
  }
  ret <- bind_rows(...)
  attr(ret, "name_map") <- name_map_master
  attr(ret, "groups") <- groups
  class(ret) <- unique(c("NONMEMdata_reset", "NONMEMdata_reset", class(ret)))
  ret
}

#' Convert a NONMEMdata object to a data frame
#' 
#' @details The data.frame will be sorted by ID, TIME (NA first then
#'   ordered by number), EVID (decreasing), CMT, then the group columns.
#'   Columns will be converted to numeric with the following rules:
#'   
#' \itemize{
#' \item{numeric}{No conversion}
#' \item{logical}{as.numeric (0 or 1)}
#' \item{character}{No conversion, except for the ID column.  The ID
#' column will be converted to a factor if it is not already numeric.}
#' \item{factor}{as.numeric on the factor with a new name for the factor
#' column.  If the factor column is mapped to a NONMEM column (often 
#' used for ID), then the numeric column will be named with the NONMEM 
#' mapping.  If the factor column is not named, the numbered column will
#' have "_numeric" appended to the original column name, the
#' factor will be converted to a character column}
#' \item{any other class}{Coerce to character with as.character}
#' }
#' 
#' Column order will be ID, TIME, EVID, EXCLUDENUM, other numeric
#' columns (after conversion), character columns (after conversion).
#' @param x The object to convert to a data.frame
#' @param ...,stringsAsFactors Ignored (kept as arguments for
#'   compatibility with the generic as.data.frame method)
#' @return A data.frame (or similar object) as described in the details
#' @export
#' @importFrom dplyr arrange
#' @importFrom dplyr "%>%"
as.data.frame.NONMEMdata <- function(x, ..., stringsAsFactors=FALSE) {
  if (!inherits(x, "NONMEMdata")) {
    stop("x must be NONMEMdata")
  }
  name_map <- name_map_updated <- attr(x, "name_map")
  groups <- groups_updated <- attr(x, "groups")
  ret <- x
  # Drop the NONEMEMdata class
  class(ret) <- setdiff(class(x), "NONMEMdata")
  # Convert the ID column to a factor if it's not already numeric
  if (!is.numeric(ret[[name_map["ID"]]])) {
    ret[[name_map["ID"]]] <- factor(ret[[name_map["ID"]]])
  }
  x[[name_map["EXCLUDETEXT"]]] <- factor(x[[name_map["EXCLUDETEXT"]]])
  # Convert columns so that they should all be numeric or character
  original_names <- names(x)
  for (i in seq_len(ncol(x))) {
    if (is.factor(x[[i]])) {
      new_name <- paste0(original_names[[i]], "_numeric")
      ret[[new_name]] <- as.numeric(x[[i]])
      # Map to the numeric value instead of the character value
      if (original_names[[i]] %in% name_map_updated) {
        name_map_updated[name_map_updated %in% original_names[[i]]] <- new_name
      }
      ret[[i]] <- as.character(x[[i]])
    } else if (is.logical(x[[i]])) {
      ret[[i]] <- as.numeric(x[[i]])
    } else if (is.numeric(x[[i]])) {
      # Do nothing
    } else if (is.character(x[[i]])) {
      # Do nothing other than maybe a warning
      if (original_names[[i]] %in% name_map) {
        warning("Character column mapped to value that should be numeric.  Mapping: ",
                original_names[[i]], "=", name_map_updated[name_map_updated == original_names[[i]]])
      }
    } else {
      if (original_names[[i]] %in% name_map_updated) {
        warning("Character column mapped to value that should be numeric.  Mapping: ",
                original_names[[i]], "=", name_map_updated[name_map_updated == original_names[[i]]])
      }
      ret[[i]] <- as.character(x[[i]])
    }
  }
  ret[[name_map_updated["EXCLUDETEXT"]]][is.na(ret[[name_map_updated["EXCLUDETEXT"]]])] <- 0
  numeric_cols <- names(ret)[sapply(ret, is.numeric)]
  character_cols <- names(ret)[sapply(ret, is.character)]
  # All columns at this point should be numeric or character.
  if (length(setdiff(names(ret), c(numeric_cols, character_cols)))) {
    stop("Bug with column type conversion") # nocov
  }
  # Ensure that required columns are in the correct set
  numeric_col_required <- name_map_updated[c("ID", "TIME", "EVID", "EXCLUDETEXT")]
  numeric_col_order <- c(numeric_col_required, "CMT")
  numeric_col_order <-
    c(intersect(numeric_col_order, numeric_cols),
      sort(
        setdiff(numeric_cols, numeric_col_order)))
  character_col_order <- sort(character_cols)
  ret <-
    ret[order(ret[[name_map_updated["ID"]]],
              -is.na(ret[[name_map_updated["TIME"]]]),
              ret[[name_map_updated["TIME"]]],
              -ret[[name_map_updated["EVID"]]],
              ret$CMT),
        c(numeric_col_order, character_col_order),
        drop=FALSE]
  rownames(ret) <- NULL
  attr(ret, "name_map") <- name_map_updated
  attr(ret, "groups") <- groups_updated
  ret
}

#' Write a NONMEMdata object to a .csv file ready for analysis
#' 
#' @param x A NONMEMdata object
#' @param file The file to write to (see \code{write.csv} for details)
#' @param also_RDS Also output an .RDS file with ".RDS" appended to the
#'   filename
#' @details The output will be sorted based on the ID column, the TIME 
#'   column, EVID (decreasing), CMT, then the group columns.
#' @return x (invisibly)
#' @export
write.NONMEMdata <- function(x, file, also_RDS=TRUE) {
  column_name_cleanup <- function(x) {
    gsub("[^0-9A-Za-z]", "", x)
  }
  x_arranged <- as.data.frame(x)
  numeric_cols <- names(x_arranged)[sapply(x_arranged, is.numeric)]
  text_cols <- setdiff(names(x_arranged), numeric_cols)
  name_map <- attr(x_arranged, "name_map")
  mapped_header <- setNames(paste(names(name_map),
                                  column_name_cleanup(name_map),
                                  sep="="),
                            column_name_cleanup(name_map))
  nonmem_header <- setNames(column_name_cleanup(numeric_cols),
                            column_name_cleanup(numeric_cols))
  nonmem_header[names(mapped_header)] <- mapped_header
  message("NONMEM Input:\n",
          paste(
            strwrap(paste(nonmem_header, collapse=" "),
                    width=72, exdent=4, initial="$INPUT "),
            collapse="\n"))
  write.csv.minimal.quote(x=x_arranged,
                          file=file,
                          na=".")
  if (also_RDS) {
    saveRDS(x, file=paste0(file, ".RDS"))
  }
  invisible(x)
}
