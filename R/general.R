#' Set attributes of columns of a data.frame (or similar object)
#'
#' @param .data The data.frame or similar object
#' @param ... A named list of column name = attribute value.
#' @param .attr The attribute name to set on the column
#' @param .allow_overwrite Allow the attribute value to be overwritten
#'   if it already exists on the current column.
#' @param .allow_multiple_values Allow the same attribute value to exist
#'   on multiple columns simultaneously (this is only checked for newly
#'   set attributes; not checked for existing attributes).
#' @param .clear Remove the attribute from all columns (occurs before
#'   any additional processing).
#' @return Set: .data with the column attributes set and/or modified.
#'   Get: a list of column names and attribute values.
#' @export
set_column_attr <- function(.data, ..., .attr="label",
                            .allow_overwrite=TRUE, .allow_multiple_values=TRUE,
                            .clear=FALSE) {
  if (!is.logical(.clear) & length(.clear) == 1) {
    stop(".clear must be a logical scalar.")
  }
  if (.clear) {
    for (i in seq_len(ncol(.data))) {
      attr(.data[[i]], .attr) <- NULL
    }
  }
  if (!is.logical(.allow_overwrite) &
      length(.allow_overwrite) == 1) {
    stop(".allow_overwrite must be a logical scalar.")
  } else if (!is.logical(.allow_multiple_values) &
             length(.allow_multiple_values) == 1) {
    stop(".allow_multiple_values must be a logical scalar.")
  }
  args <- list(...)
  if (length(args) > 0) {
    if (is.null(names(args))) {
      stop("Arguments must be named.")
    } else if (any(names(args) %in% "")) {
      stop("All arguments must be named.")
    } else if (any(!(names(args) %in% names(.data)))) {
      stop("All argument names must refer to a column of .data.")
    }
    if (!.allow_overwrite | !.allow_multiple_values) {
      old_attr <- get_column_attr(.data=.data, .attr=.attr)
      if (!.allow_overwrite) {
        mask_overwrite <- names(args) %in% names(old_attr)
        if (any(mask_overwrite)) {
          stop("The following columns will have attributes overwritten and .allow_overwrite is FALSE: ",
               paste(names(args)[mask_overwrite], collapse=", "))
        }
      }
      if (!.allow_multiple_values) {
        # Merge the old and new attributes
        new_attr <- old_attr
        for (nm in names(args)) {
          new_attr[[nm]] <- args[[nm]]
        }
        args <- new_attr
        for (i in seq_len(length(args) - 1)) {
          for (j in (i+1):length(args)) {
            if (identical(args[[i]], args[[j]])) {
              stop("Identical arguments will be set for ", names(args)[i], " and ", names(args)[j], ".")
            }
          }
        }
      }
    }
    for (nm in names(args)) {
      attr(.data[[nm]], .attr) <- args[[nm]]
    }
  }
  .data
}

#' @describeIn set_column_attr Get the value of an attribute for all columns.
#' @export
#' @importFrom stats setNames
get_column_attr <- function(.data, .attr="label") {
  has_attr <-
    sapply(names(.data),
           FUN=function(nm, .attr) {
             !is.null(attr(.data[[nm]], .attr))
           },
           .attr=.attr)
  # use this so that output has the name
  has_attr_nm <- names(.data)[has_attr]
  lapply(stats::setNames(has_attr_nm, has_attr_nm),
         FUN=function(nm, .attr) {
           attr(.data[[nm]], .attr)
         },
         .attr=.attr)
}

#' Switch the names and values in a vector.
#'
#' @param x The object to switch
#' @return x with the names put into the values and the values put into
#'   the names.
#' @seealso \code{\link{setNames}}
#' @importFrom stats setNames
switchNames <- function(x) {
  stats::setNames(names(x), x)
}
