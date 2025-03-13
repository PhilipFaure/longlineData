#' Standardize values in a data frame, excluding the first column.
#'
#' TThis is an internal function which standardizes the values in a data frame, excluding the first column.
#' It handles character and numeric values, replacing missing values and standardizing text.
#'
#' @param df A data frame.
#'
#' @return A data frame with standardized values.
#'
standardize_values_exclude_first <- function(df) {
  if (nrow(df) <= 1 || ncol(df) <= 1) {
    return(df) # Return original df if there's no data to process
  }

  sub_df <- df[ , -1] # Exclude the first column

  for (i in seq_len(nrow(sub_df))) {
    for (j in seq_len(ncol(sub_df))) {
      value <- sub_df[i, j]

      if (is.character(value)) {
        if (is.na(value) || trimws(value) == "") {
          sub_df[i, j] <- "0"
        } else if (value == "NA" || value == "N/A" || value == "N\\A" || value == "na" ) {
          sub_df[i, j] <- 0
        } else {
          sub_df[i, j] <- toupper(value)
        }
      } else if (is.na(value)) {
        sub_df[i,j] <- 0
      }
    }
  }

  df[, -1] <- sub_df # Replace the original subset with standardized values
  return(df)
}
