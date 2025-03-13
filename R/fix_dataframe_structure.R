#' Fix the structure of a data frame by aligning rows based on W/F/D and material columns.
#'
#' This is an internal function which takes a data frame with varying row lengths, identifies the positions
#' of the last "W", "F", or "D" (case-insensitive) and the material column (if present),
#' and aligns the rows by inserting "NO_DATA" values and shifting data to the right.
#'
#' @param df A data frame with varying row lengths.
#'
#' @return A data frame with aligned rows, ensuring all rows have the same length.
#'
fix_dataframe_structure <- function(df) {
  num_cols <- ncol(df)
  num_rows <- nrow(df)

  # Find the position of the last W/F/D and material column (if present)
  last_wfd_col <- sapply(1:num_rows, function(i) {
    for (j in num_cols:1) {
      if (df[i, j] %in% c("W", "F", "D", "w", "f", "d",
                          "W ", "F ", "D ", "w ", "f ", "d ",
                          " W", " F", " D", " w", " f", " d")) {
        return(j)
      }
    }
    return(NA)
  })

  # Find position of material column (if present)
  material_col <- sapply(1:num_rows, function(i) {
    if (is.na(last_wfd_col[i])) {
      return(NA) # Handle cases where WFD is missing
    }
    if (last_wfd_col[i] + 1 <= num_cols) {
      if (is.character(df[i, last_wfd_col[i] + 1])) {
        return(last_wfd_col[i] + 1)
      } else {
        return(NA)
      }
    } else {
      return(NA)
    }
  })

  max_wfd_col <- max(last_wfd_col, na.rm = TRUE)
  max_material_col <- max(material_col, na.rm = TRUE)

  max_cols <- max(max_wfd_col, max_material_col, na.rm = TRUE)

  for (i in 1:num_rows) {
    if (!is.na(last_wfd_col[i]) && last_wfd_col[i] < max_wfd_col) {
      # Shift data to the right, inserting "NO_DATA"
      new_row <- c(df[i, 1:(last_wfd_col[i] - 1)],
                   rep("NO_DATA", max_cols - (length(df[i, 1:(last_wfd_col[i] - 1)]) + 2)),
                   df[i, last_wfd_col[i]],
                   df[i, last_wfd_col[i] + 1])

      df[i, ] <- new_row[1:num_cols]
    }
  }

  return(df)
}
