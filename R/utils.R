#' Try apply x to f
#'
#' @param fun function. Function to which you want to apply the value.
#' @param val value. Value to which you want to try apply to the function.
#'
#' @returns list. Details success, value of fun(val) and the error message
#' @noRd

try_apply_function <- function(fun, val) {
  tryCatch(
    {
      list(
        success = TRUE,
        value = fun(val),
        error = NULL
      )
    },
    error = function(e) {
      list(
        success = FALSE,
        value = NULL,
        error = e
      )
    }
  )
}

#' Create a superscript string
#'
#' @param n integer. Value of the superscript
#'
#' @returns string. Unicode representation of the superscript.
#' @noRd

superscript <- function(n) {

  n_map <- c(
    "0" = "\u2070", "1" = "\u00B9", "2" = "\u00B2", "3" = "\u00B3",
    "4" = "\u2074", "5" = "\u2075", "6" = "\u2076", "7" = "\u2077",
    "8" = "\u2078", "9" = "\u2079", "-" = "\u207B"
  )

  chars <- strsplit(as.character(n), "")[[1]]
  paste0(n_map[chars], collapse = "")
}
