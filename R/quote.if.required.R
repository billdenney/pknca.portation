#' Update a vector with the minimum quotes required to make a .csv file
#' 
#' @param x The vector (either character or something that can be
#'   coerced into a character vector)
#' @param qmethod,sep,eol See \code{write.csv}
#' @return A vector as a character string, quoted if required
#' @export
quote.if.required <- function(x, qmethod=c("double", "escape"), sep=",", eol="\n") {
  qmethod <- match.arg(qmethod)
  x <- as.character(x)
  mask.quote.sub <- grepl('"', x, fixed=TRUE)
  mask.quote.sep <-
    grepl(sep, x, fixed=TRUE) |
    grepl(eol, x, fixed=TRUE)
  qstring <- switch(qmethod, escape="\\\\\"", double="\"\"")
  x[mask.quote.sub] <-
    paste0('"', gsub('"', qstring, x[mask.quote.sub]), '"')
  x[mask.quote.sep & !mask.quote.sub] <-
    paste0('"', x[mask.quote.sep & !mask.quote.sub], '"')
  x
}

#' Write a .csv file with the minimum amount of quoting
#' 
#' @param x A data.frame or something that can be coerced into a
#'   data.frame
#' @param file The file (See \code{write.csv} for details)
#' @param ... Ignored
#' @param qmethod,row.names,sep,eol See \code{write.csv}
#' @param quote,append,dec Ignored (with a warning)
#' @return
write.csv.minimal.quote <- function(x, file="", ..., qmethod=c("double", "escape"), row.names=FALSE, sep=",", eol="\n", quote, append, dec) {
  if (!missing(quote)) {
    warning("quote argument ignored")
  }
  if (!missing(append)) {
    warning("append argument ignored")
  }
  if (!missing(dec)) {
    warning("dec argument ignored")
  }
  qmethod <- match.arg(qmethod)
  if (!is.data.frame(x)) {
    cn <- colnames(x)
    x <- as.data.frame(x)
    colnames(x) <- cn
  } else {
    cn <- colnames(x)
  }
  cn <- quote.if.required(cn,
                          qmethod=qmethod,
                          sep=sep,
                          eol=eol)
  x <- as.data.frame(lapply(x, quote.if.required,
                            qmethod=qmethod,
                            sep=sep,
                            eol=eol))
  if (is.logical(row.names) && row.names) {
    row.names <- quote.if.required(base::row.names(x),
                                   qmethod=qmethod,
                                   sep=sep,
                                   eol=eol)
  } else if (is.character(row.names)) {
    row.names <- quote.if.required(row.names,
                                   qmethod=qmethod,
                                   sep=sep,
                                   eol=eol)
  }
  write.table(x, file=file, ..., append=FALSE, sep=sep, dec=".", eol=eol, col.names=cn, row.names=row.names, quote=FALSE)
}

#tmp <- data.frame('"abc'=1:3, "def,hij"=c("1,2", "3", '4"5'), klm=6:8)
#names(tmp) <- c('"abc', "def,hij", "klm")
#write.csv.minimal.quote(tmp, file="test.csv")
