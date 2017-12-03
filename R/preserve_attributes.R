#' Return a subset of the attributes as a list
#' 
#' @param x The object to extract attributes from
#' @param which A character string of attributes to preserve
#' @param value A named list to assign into the attributes of \code{x}
#' @return A list of attributes
#' @export
preserve_attributes <- function(x, which) {
  attributes(x)[which]
}

#' @describeIn preserve_attributes Assign a subset of attributes to an object
#' @export
"preserve_attributes<-" <- function(x, value) {
  for (n in names(value)) {
    attr(x, n) <- value[[n]]
  }
  x
}
