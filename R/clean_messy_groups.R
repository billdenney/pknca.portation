#' Simplify data to a single value within a group
#' 
#' During dataset generation, sometimes data from different sources may 
#' put missing values where consistent results should be present.
#' 
#' @param data The dataset (typically a data.frame or similar) to
#'   simplify
#' @param keys Column names (as character strings) for grouping
#' @param values Columns to from the original data as values that must
#'   be unique within a group (defaults to all columns other than
#'   \code{keys})
#' @param missing_value A scalar value indicating what should be 
#'   considered missing and should be used for replacement if all values
#'   are missing (typically \code{NA}).
#' @return A dataset with one row per key and one value in each column.
#' @export
#' @importFrom rlang syms
#' @importFrom dplyr filter_all group_by summarize_all ungroup "%>%"
clean_messy_groups <- function(data, keys, values, missing_value=NA) {
  if (missing(values)) {
    values <- setdiff(names(data), keys)
  }
  data_group <-
    data[,c(keys, values), drop=FALSE] %>%
    dplyr::group_by(!!! rlang::syms(keys))
  # Ensure a single value per group
  value_count <-
    data_group %>%
    summarize_all(function(x, missing_value) {
      length(unique(setdiff(x, missing_value)))
    },
    missing_value=missing_value) %>%
    filter_at(vars(!!! rlang::syms(values)),
              any_vars(. > 1))
  if (nrow(value_count)) {
    print(value_count)
    stop("Above rows cannot be simplified to a single data row")
  }
  # Determine what the single value per group is
  data_group %>%
    summarize_all(function(x, missing_value) {
      ret <- unique(setdiff(x, missing_value))
      if (length(ret) == 0) {
        ret <- class(missing_value, class(x))
      }
      ret
    },
    missing_value=missing_value) %>%
    ungroup()
}

#' @describeIn clean_messy_groups Both clean the groups and replace them
#'   back in the original dataset.
#' @export
#' @importFrom dplyr left_join
replace_messy_groups <- function(data, keys, values, missing_value=NA) {
  if (missing(values)) {
    values <- setdiff(names(data), keys)
  }
  cleaned <- clean_messy_groups(data=data,
                                keys=keys,
                                values=values,
                                missing_value=missing_value)
  left_join(data[,setdiff(names(data), values), drop=FALSE],
            cleaned)
}
