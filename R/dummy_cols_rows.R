## c:/Dropbox/Rpackages/CLmisc/R/dummy_cols_rows.R

##    Chandler Lutz
##    Questions/comments: cl.eco@cbs.dk
##    $Revisions:      1.0.0     $Date:  2019-11-03

## Shamelessly stolen from
## https://github.com/jacobkap/fastDummies

check_type <- function(.data) {
  if (data.table::is.data.table(.data)) {
    data_type <- "is_data_table"
  } else if (tibble::is_tibble(.data)) {
    data_type <- "is_tibble"
  } else {
    data_type <- "is_data_frame"
  }

  return(data_type)
}

fix_data_type <- function(.data, data_type) {
  if (data_type == "is_data_frame") {
    .data <- as.data.frame(.data)
  } else if (data_type == "is_tibble") {
    .data <- tibble::as_tibble(.data)
  }

  return(.data)
}

#' Fast creation of dummy variables
#'
#' dummy_cols() quickly creates dummy (binary) columns from character and
#' factor type columns in the inputted data (and numeric columns if specified.)
#' This function is useful for statistical analysis when you want binary
#' columns rather than character columns.
#'
#' @family dummy functions
#' @seealso \code{\link{dummy_rows}} For creating dummy rows
#'
#' @param .data
#' An object with the data set you want to make dummy columns from.
#' @param select_columns
#' Vector of column names that you want to create dummy variables from.
#' If NULL (default), uses all character and factor columns.
#' @param remove_first_dummy
#' Removes the first dummy of every variable such that only n-1 dummies remain.
#' This avoids multicollinearity issues in models.
#' @param remove_most_frequent_dummy
#' Removes the most frequently observed category such that only n-1 dummies
#' remain. If there is a tie for most frequent, will remove the first
#' (by alphabetical order) category that is tied for most frequent.
#' @param ignore_na
#' If TRUE, ignores any NA values in the column. If FALSE (default), then it
#' will make a dummy column for value_NA and give a 1 in any row which has a
#' NA value.
#' @param split
#' A string to split a column when multiple categories are in the cell. For
#' example, if a variable is Pets and the rows are "cat", "dog", and "turtle",
#' each of these pets would become its own dummy column. If one row is "cat, dog",
#' then a split value of "," this row would have a value of 1 for both the cat
#' and dog dummy columns.
#'
#' @return
#' A data.frame (or tibble or data.table, depending on input data type) with
#' same number of rows as inputted data and original columns plus the newly
#' created dummy columns.
#' @export
#' @examples
#' crime <- data.frame(city = c("SF", "SF", "NYC"),
#'     year = c(1990, 2000, 1990),
#'     crime = 1:3)
#' dummy_cols(crime)
#' # Include year column
#' dummy_cols(crime, select_columns = c("city", "year"))
#' # Remove first dummy for each pair of dummy columns made
#' dummy_cols(crime, select_columns = c("city", "year"),
#'     remove_first_dummy = TRUE)
dummy_cols <- function(.data,
                       select_columns = NULL,
                       remove_first_dummy = FALSE,
                       remove_most_frequent_dummy = FALSE,
                       ignore_na = FALSE,
                       split = NULL) {

  stopifnot(is.null(select_columns) || is.character(select_columns),
            select_columns != "",
            is.logical(remove_first_dummy), length(remove_first_dummy) == 1)

  if (remove_first_dummy == TRUE & remove_most_frequent_dummy == TRUE) {
    stop("Select either 'remove_first_dummy' or 'remove_most_frequent_dummy'
         to proceed.")
  }

  data_type <- check_type(.data)

  if (!data.table::is.data.table(.data)) {
    .data <- data.table::as.data.table(.data)
  }

  # Grabs column names that are character or factor class -------------------
  if (!is.null(select_columns)) {
    char_cols <- select_columns
    cols_not_in_data <- char_cols[!char_cols %in% names(.data)]
    char_cols <- char_cols[!char_cols %in% cols_not_in_data]
    if (length(char_cols) == 0) {
      stop("select_columns is/are not in data. Please check data and spelling.")
    }
  } else if (ncol(.data) == 1) {
    char_cols <- names(.data)
  } else {
    char_cols <- sapply(.data, class)
    char_cols <- char_cols[char_cols %in% c("factor", "character")]
    char_cols <- names(char_cols)
  }

  if (length(char_cols) == 0 && is.null(select_columns)) {
    stop(paste0("No character or factor columns found. ",
                "Please use select_columns to choose columns."))
  }

  if (!is.null(select_columns) && length(cols_not_in_data) > 0) {
    warning(paste0("NOTE: The following select_columns input(s) ",
                   "is not a column in data.\n"),
            paste0(names(cols_not_in_data), "\t"))
  }


  for (col_name in char_cols) {
    # If factor type, order by assigned levels
    if (is.factor(.data[[col_name]])) {
      unique_vals <- levels(.data[[col_name]])
      if (any(is.na(.data[[col_name]]))) {
        unique_vals <- c(unique_vals, NA)
      }
      # Else by order values appear.
    } else {
      unique_vals <- unique(.data[[col_name]])
    }
    unique_vals <- as.character(unique_vals)

    # If there is a split value, splits up the unique_vals by that value
    # and keeps only the unique ones.
    if (!is.null(split)) {
      unique_vals <- unique(trimws(unlist(strsplit(unique_vals, split = split))))
    }

    if (ignore_na) {
      unique_vals <- unique_vals[!is.na(unique_vals)]
    }

    if (remove_most_frequent_dummy) {
      vals <- as.character(.data[[col_name]])
      vals <- data.frame(sort(table(vals), decreasing = TRUE),
                         stringsAsFactors = FALSE)
      if (vals$Freq[1] > vals$Freq[2]) {
        vals <- as.character(vals$vals[2:nrow(vals)])
        unique_vals <- unique_vals[which(unique_vals %in% vals)]
        unique_vals <- vals[order(match(vals, unique_vals))]
      } else {
        remove_first_dummy <- TRUE
      }
    }

    if (remove_first_dummy) {
      unique_vals <- unique_vals[-1]
    }

    data.table::alloc.col(.data, ncol(.data) + length(unique_vals))
    data.table::set(.data, j = paste0(col_name, "_", unique_vals), value = 0L)
    for (unique_value in unique_vals) {
      data.table::set(.data, i =
                        which(data.table::chmatch(
                          as.character(.data[[col_name]]),
                          unique_value, nomatch = 0) == 1L),
                      j = paste0(col_name, "_", unique_value), value = 1L)


      # Sets NA values to NA, only for columns that are not the NA columns
      if (!is.na(unique_value)) {
      data.table::set(.data, i =
                        which(is.na(.data[[col_name]])),
                      j = paste0(col_name, "_", unique_value), value = NA)
      }

      if (!is.null(split)) {
        max_split_length <- max(sapply(strsplit(as.character(.data[[col_name]]),
                                                split = split), length))
        for (split_length in 1:max_split_length) {
          data.table::set(.data, i =
                            which(data.table::chmatch(
                              as.character(trimws(sapply(strsplit(as.character(.data[[col_name]]),
                                                                  split = split),
                                                         `[`, split_length))),
                              unique_value, nomatch = 0) == 1L),
                          j = paste0(col_name, "_", unique_value), value = 1L)

        }
        if (is.na(unique_value)) {
          .data[[paste0(col_name, "_", unique_value)]][which(!is.na(.data[[col_name]]))] <- 0
        }
      }
    }
  }

  .data <- fix_data_type(.data, data_type)
  return(.data)

}


#' Fast creation of dummy rows
#'
#' dummy_rows() quickly creates dummy rows to fill in missing rows
#' based on all combinations of available character, factor, and
#' date columns (if not otherwise specified). This is useful for
#' creating balanced panel data. Columns that are not character,
#' factor, or dates are filled in with NA (or whatever value you
#' specify).
#'
#' @family dummy functions
#' @seealso \code{\link{dummy_cols}} For creating dummy columns
#'
#' @param .data
#' An object with the data set you want to make dummy columns from.
#' @param select_columns
#' If NULL (default), uses all character, factor, and Date columns to produce categories
#' to make the dummy rows by. If not NULL, you manually enter a string or vector of strings of columns name(s).
#' @param dummy_value
#' Value of the row for columns that are not selected.
#' Default is a value of NA.
#' @param dummy_indicator
#' Adds binary column to say if row is dummy or not (i.e. included in
#' original data or not)
#'
#' @return
#' A data.frame (or tibble or data.table, depending on input data type) with
#' same number of columns as inputted data and original rows plus the newly
#' created dummy rows
#' @export
#' @examples
#' crime <- data.frame(city = c("SF", "SF", "NYC"),
#'     year = c(1990, 2000, 1990),
#'     crime = 1:3)
#'
#' dummy_rows(crime)
#' # Include year column
#' dummy_rows(crime, select_columns = c("city", "year"))
#' # m=Make dummy value 0
#' dummy_rows(crime, select_columns = c("city", "year"),
#'     dummy_value = 0)
#' # Add a dummy indicator
#' dummy_rows(crime, select_columns = c("city", "year"),
#'     dummy_indicator = TRUE)
dummy_rows <- function(.data,
                       select_columns = NULL,
                       dummy_value = NA,
                       dummy_indicator = FALSE) {

  stopifnot(is.null(select_columns) || is.character(select_columns),
            select_columns != "",
            is.logical(dummy_indicator), length(dummy_indicator) == 1,
            length(dummy_value) == 1)

  if (is.atomic(.data) || ncol(.data) == 1) {
    stop("Cannot make dummy rows of a vector of one column data.frame/table.")
  }

  data_type <- check_type(.data)

  if (!data.table::is.data.table(.data)) {
    .data <- data.table::as.data.table(.data)
  }

  # Finds class of every column and keeps character, factor, and Date --------
  if (is.null(select_columns)) {
    char_cols <- sapply(.data, class)
    char_cols <- names(.data)[char_cols %in%
                               c("character", "factor", "Date")]
    if (length(char_cols) == 0) {
      stop("No character, factor, or Date columns found. Please use select_columns")
    }

  } else {
    char_cols <- select_columns
  }

  other_cols <- names(.data)[!names(.data) %in% char_cols]


  # Finds how many possible combinations of the variables there are.
  # This will be the number of rows in the new data
  total_length <- prod(sapply(.data[, char_cols, with = FALSE, drop = FALSE],
                              data.table::uniqueN))
  # Makes an empty data.table with right # of rows and columns. -------------
  temp_table <- data.table::data.table(matrix(nrow = total_length,
                                              ncol = ncol(.data)))
  names(temp_table) <- names(.data)

  # Fills in all possible combination rows ----------------------------------
    for (i in char_cols) {
        data.table::set(temp_table, j = i,
                      value = rep(unique(.data[[i]]), times =
                                  total_length /
                                  data.table::uniqueN(.data[[i]])))
     temp_table <- data.table::setorderv(temp_table, i)
    }

  # Adds the dummy variable columns (and indicator) -------------------------
  for (i in other_cols) {
    data.table::set(temp_table, j = other_cols,
                    value = rep(dummy_value, nrow(temp_table)))
  }

  if (dummy_indicator) {
    # Adding extra column
    data.table::alloc.col(temp_table, ncol(temp_table) + 1)
    data.table::alloc.col(.data, ncol(.data) + 1)

    data.table::set(.data, j = "dummy_indicator", value = 0L)
    data.table::set(temp_table, j = "dummy_indicator",
                    value = rep(1L, nrow(temp_table)))
  }

  # Removes rows that were in original data. --------------------------------
  data_temp_pasting <- do.call(paste0, .data[, char_cols, with = FALSE,
                                            drop = FALSE])
  temp_temp_pasting <- do.call(paste0, temp_table[, char_cols, with = FALSE,
                                            drop = FALSE])
  temp_table <- subset(temp_table, !temp_temp_pasting %in% data_temp_pasting)

  # Stacks new data on old data
  if (nrow(temp_table) > 0) {
  .data <- data.table::rbindlist(list(.data, temp_table), use.names = TRUE,
                                fill = TRUE)
  }

  .data <- fix_data_type(.data, data_type)
  return(.data)

}

