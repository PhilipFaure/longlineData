#' Process longline catch data from CSV files in a directory.
#'
#' This function reads CSV files from a specified directory, standardizes the data,
#' extracts information from the file names, fixes the dataframe structure,
#' and combines the data into a single data frame.
#'
#' @param inDir The path to the directory containing the CSV files.
#'
#' @return A data frame containing the combined and processed data from all CSV files.
#'
#' @examples
#' \dontrun{
#' # Assuming you have CSV files in a directory named "data"
#' data <- longlineCount("C:/Users/BirdLife/Documents/Data")
#' View(data)
#' }
#'
longlineCount <- function(inDir) {
  fls <- list.files(inDir, full.names = FALSE)
  check_dir_suffixes(fls, ".csv")
  data <- list()
  
  for (f in 1:length(fls)) {
    df <- read.csv(paste0(inDir, "/", fls[f]), stringsAsFactors = FALSE)
    
    # Standardize values
    df <- standardize_values_exclude_first(df)
    
    parts <- strsplit(fls[f], "_|\\.csv")[[1]]
    
    if (length(parts) < 7) {
      warning(paste("Invalid filename format:", fls[f]))
    } else {
      date <- as.Date(parts[1], format = "%Y%m%d")
      start_time <- format(strptime(parts[2], format = "%H%M"), "%H:%M")
      end_time <- format(strptime(parts[3], format = "%H%M"), "%H:%M")
      latitude <- gsub("lat", "", parts[4])
      longitude <- gsub("lon", "", parts[5])
      haul_number <- gsub("H", "", parts[6])
      catch_number <- gsub("C", "", parts[7])
      
      fls_dat <- data.frame(
        date = date,
        start_time = start_time,
        end_time = end_time,
        Y = latitude,
        X = longitude,
        haul_number = haul_number,
        catch_number = catch_number,
        stringsAsFactors = FALSE
      )
      
      df <- cbind(fls_dat, df)
      
      df <- fix_dataframe_structure(df)
      
      if (f == 1) {
        all_colnames <- colnames(df)
      } else {
        if (length(colnames(df)) < length(data)) {
          colnames(df) <- c("date", "start_time", "end_time", "Y", "X", "haul_number", "catch_number", "Line", "Start",
                            paste0("X", 1:(length(colnames(df)) - 11)),
                            "End", "Material")
          missing_cols <- setdiff(colnames(data), colnames(df))
          
          if (length(missing_cols) > 0 && length(missing_cols) < 3) {
            for (col in missing_cols) {
              df[[col]] <- "NO_DATA"
            }
            df <- df[colnames(data)]
            data <- rbind(data, df)
          } else {
            if (length(missing_cols) > 2) {
              for (col in missing_cols[1:(length(missing_cols))]) {
                df[[col]] <- "NO_DATA"
              }
              df <- df[colnames(data)]
              data <- rbind(data, df)
            }
          }
        } else {
          missing_cols <- setdiff(colnames(df), colnames(data))
          if (length(missing_cols) > 0 && length(missing_cols) < 3) {
            for (col in missing_cols) {
              data[[col]] <- "NO_DATA"
            }
            data <- data[colnames(df)]
            data <- rbind(data, df)
          } else {
            for (col in missing_cols[1:(length(missing_cols) - 2)]) {
              data[[col]] <- "NO_DATA"
            }
            data <- data[c("date", "start_time", "end_time", "Y", "X", "haul_number", "catch_number", "Line", "Start",
                           paste0("X", 1:(length(colnames(data)) - 11)),
                           "End", "Material")]
            colnames(df) <- c("date", "start_time", "end_time", "Y", "X", "haul_number", "catch_number", "Line", "Start",
                              paste0("X", 1:(length(colnames(df)) - 11)),
                              "End", "Material")
            df <- df[colnames(data)]
            data <- rbind(data, df)
          }
        }
      }
    }
    
    if (f == 1) {
      data <- df
    }
  }
  return(data)
}