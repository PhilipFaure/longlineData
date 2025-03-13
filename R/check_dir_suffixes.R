#' Check if all directories end with specified suffixes.
#'
#' This is an internal function which checks if all directories in a given list end with the specified suffixes.
#'
#' @param dirs A character vector of directory names.
#' @param suffix A character vector of suffixes to check.
#'
#' @return Invisibly returns TRUE if all directories match the suffixes, FALSE otherwise.
#'
check_dir_suffixes <- function(dirs, suffix) {
  if (length(dirs) == 0) {
    return(TRUE) # empty list passes
  }

  all_match <- sapply(dirs, function(dir) {
    any(endsWith(dir, suffix))
  }) |> all()

  if (!all_match) {
    warn_message <- "Not all directories end with the specified suffixes."
    warning(warn_message, immediate. = TRUE, call. = FALSE) # Force immediate display
    invisible(FALSE) # return false invisibly
  } else{
    invisible(TRUE) # return true invisibly
  }
}
