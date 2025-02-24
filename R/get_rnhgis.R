#' Retrieve NHGIS datasets with caching and lookup table generation.
#'
#' This function retrieves datasets from NHGIS (National Historical
#' Geographic Information System) using the `ipumsr` package, with
#' caching capabilities to avoid redundant downloads.  It also
#' generates a lookup table containing metadata about the dataset.
#'
#' @param ... Arguments to be passed to `ipumsr::ds_spec()`,
#'   specifying the datasets to retrieve.
#' @param lkp Logical. If `TRUE`, returns the lookup table; if `FALSE`
#'   (default), returns the dataset.
#' @param save_dir Directory where downloaded data and lookup tables
#'   are saved. Defaults to "data-raw/rnhgis_data/".
#'
#' @return A data frame (if `lkp = FALSE`) or a lookup table (if `lkp
#'   = TRUE`) containing the requested NHGIS data.
#'
#' @details This function first checks if a cached parquet file exists
#'   for the specified dataset.  If it does, and `lkp` is `FALSE`, the
#'   cached dataset is returned. If `lkp` is `TRUE`, the cached lookup
#'   table is returned.  If the cached file does not exist, the
#'   function downloads the data from NHGIS using the `ipumsr`
#'   package.  It requires an NHGIS API key to be set as an
#'   environment variable named "IPUMS_API_KEY".  The downloaded data
#'   and the generated lookup table are then saved as parquet files in
#'   the specified `save_dir`.
#'
#' The lookup table contains information about each variable in the
#' dataset, including its name, type, and attributes.
#'
#' @import ipumsr
#' @import nanoparquet
#' @import data.table
#' @import here
#'
#' @examples
#' \dontrun{
#' ## Example to get places pop from NHGIS
#' ds <- get_metadata_nhgis(type = "datasets") %>% setDT()
#' ds[grepl("2000", name) & grepl("SF1", name)]
#'
#' sf1 <- get_metadata_nhgis(dataset = "2000_SF1a")
#'
#' ## Find the variable name
#' sf1$data_tables %>% as.data.table() %>% .[grepl("Total Population", description)]
#' ## Find the geography level
#' sf1$geog_levels %>% as.data.table() %>% .[grepl("Place", description)] %>% .[4]
#'
#' dt_place_pop00 <- get_rnhgis_ds(
#'   name = "2000_SF1a",
#'   data_tables = "NP001A",
#'   geog_levels = "place"
#' )
#'
#' dt_place_pop00_lkp <- get_rnhgis_ds(
#'   name = "2000_SF1a",
#'   data_tables = "NP001A",
#'   geog_levels = "place",
#'   lkp = TRUE
#' )
#' }
#'
#' @export
get_rnhgis_ds <- function(..., lkp = FALSE,
                          save_dir = here::here("data-raw/rnhgis_ds/")) {

  args <- list(...)
  if (length(args) == 0) 
    stop("Error in `get_rnhgis_data()`: must provide at least one dataset")

  arg_names <- names(args)
  args_key_val_string <- paste0(arg_names,  unlist(args),
                                collapse = "_")

  ## Check for a parquet cached file 
  save_file_loc <- sprintf(
    "%s/%s.parquet", save_dir, paste0(args_key_val_string)
  )
  save_file_loc_lkp <- sprintf(
    "%s/%s_lkp.parquet", save_dir, paste0(args_key_val_string)
  )
  
  if (file.exists(save_file_loc) && !lkp) {
    return(nanoparquet::read_parquet(save_file_loc))
  } else if (file.exists(save_file_loc) && lkp) {
    return(nanoparquet::read_parquet(save_file_loc_lkp))
  }
  
  ## Check for an NHGIS key in the environment
  if (!"IPUMS_API_KEY" %in% names(Sys.getenv())) 
    stop("Error in `get_rnhgis_data()`: IPUMS_API_KEY not found in environment")

  mkdir_p(save_dir)
  save_dir_zips <- paste0(save_dir, "zip_files/")
  mkdir_p(save_dir_zips)

  ds <- ipumsr::ds_spec(...)

  nhgis_data_ext <- ipumsr::define_extract_nhgis(
    description = paste("Data request", Sys.time()),
    datasets = ds
  )

  nhgis_data_ext_submitted <- ipumsr::submit_extract(nhgis_data_ext)

  data_extract_complete <- ipumsr::wait_for_extract(nhgis_data_ext_submitted)

  if (ipumsr::is_extract_ready(data_extract_complete)) {
    dwnld_file_loc <- ipumsr::download_extract(
      data_extract_complete,
      download_dir = save_dir_zips,
      overwrite = TRUE,
      progress = TRUE
    )
  } else {
    stop("Error in `get_rnhgis_ds()`: Extract not ready")
  }

  if (nrow(ipumsr::ipums_list_files(dwnld_file_loc)) > 1) {
    stop("Error in `get_rnhgis_ds()`: Multiple  downloaded; only single files supported")
  }

  data <- ipumsr::read_nhgis(dwnld_file_loc)

  dt_lkp <- 
    cbind(
      data.table(var = names(data), type = sapply(data, class)), 
      data %>% lapply(attributes) %>% rbindlist(fill = TRUE)
    )
  
  
  nanoparquet::write_parquet(data, save_file_loc)
  nanoparquet::write_parquet(dt_lkp, save_file_loc_lkp)
  
  file.remove(dwnld_file_loc)

  if (lkp) {
    return(dt_lkp)
  } else {
    return(data)
  }
  
}


#' Retrieve NHGIS time series tables with caching and lookup table generation.
#'
#' This function retrieves time series tables from NHGIS (National
#' Historical Geographic Information System) using the `ipumsr`
#' package, with caching capabilities to avoid redundant downloads.
#' It also generates a lookup table containing metadata about the
#' dataset.
#'
#' @param ... Arguments to be passed to `ipumsr::tst_spec()`,
#'   specifying the time series tables to retrieve.
#' @param lkp Logical. If `TRUE`, returns the lookup table; if `FALSE`
#'   (default), returns the dataset.
#' @param save_dir Directory where downloaded data and lookup tables
#'   are saved. Defaults to "data-raw/rnhgis_tst/".
#'
#' @return A data frame (if `lkp = FALSE`) or a lookup table (if `lkp
#'   = TRUE`) containing the requested NHGIS time series data.
#'
#' @details This function first checks if a cached parquet file exists
#'   for the specified time series tables.  If it does, and `lkp` is
#'   `FALSE`, the cached dataset is returned. If `lkp` is `TRUE`, the
#'   cached lookup table is returned.  If the cached file does not
#'   exist, the function downloads the data from NHGIS using the
#'   `ipumsr` package.  It requires an NHGIS API key to be set as an
#'   environment variable named "IPUMS_API_KEY".  The downloaded data
#'   and the generated lookup table are then saved as parquet files in
#'   the specified `save_dir`.
#'
#' The lookup table contains information about each variable in the
#' dataset, including its name, type, and attributes.
#'
#' @import ipumsr
#' @import nanoparquet
#' @import data.table
#' @import here
#'
#' @examples
#' \dontrun{
#' ## Example to get places pop time series from NHGIS
#' tst <- get_metadata_nhgis(type = "time_series_tables") %>% setDT()
#'
#' ## Find which time series tables have a place `geog_level`
#' ## and get the total population variable
#' tst[sapply(geog_levels, function(x) any(grepl("place", x)))] %>%
#'   .[grepl("Total Population", description)]
#'
#' tst_spec(name = "AV0", geog_levels = "place")
#'
#' place_pop_tst <- get_rnhgis_tst(name = "AV0", geog_levels = "place")
#'
#' place_pop_tst_lkp <- get_rnhgis_tst(name = "AV0", geog_levels = "place", lkp = TRUE)
#' }
#'
#' @export
get_rnhgis_tst <- function(..., lkp = FALSE,
                           save_dir = here::here("data-raw/rnhgis_tst/")) {

  args <- list(...)
  if (length(args) == 0) 
    stop("Error in `get_rnhgis_data()`: must provide at least one dataset")

  arg_names <- names(args)
  args_key_val_string <- paste0(arg_names,  unlist(args),
                                collapse = "_")

  ## Check for a parquet cached file 
  save_file_loc <- sprintf(
    "%s/%s.parquet", save_dir, paste0(args_key_val_string)
  )
  save_file_loc_lkp <- sprintf(
    "%s/%s_lkp.parquet", save_dir, paste0(args_key_val_string)
  )

  if (file.exists(save_file_loc) && !lkp) {
    return(nanoparquet::read_parquet(save_file_loc))
  } else if (file.exists(save_file_loc) && lkp) {
    return(nanoparquet::read_parquet(save_file_loc_lkp))
  }
  
  ## Check for an NHGIS key in the environment
  if (!"IPUMS_API_KEY" %in% names(Sys.getenv())) 
    stop("Error in `get_rnhgis_data()`: IPUMS_API_KEY not found in environment")

  mkdir_p(save_dir)
  save_dir_zips <- paste0(save_dir, "zip_files/")
  mkdir_p(save_dir_zips)

  tst <- ipumsr::tst_spec(...)

  nhgis_data_ext <- ipumsr::define_extract_nhgis(
    description = paste("Data request", Sys.time()),
    time_series_tables = tst
  )

  nhgis_data_ext_submitted <- ipumsr::submit_extract(nhgis_data_ext)

  data_extract_complete <- ipumsr::wait_for_extract(nhgis_data_ext_submitted)

  if (ipumsr::is_extract_ready(data_extract_complete)) {
    dwnld_file_loc <- ipumsr::download_extract(
      data_extract_complete,
      download_dir = save_dir_zips,
      overwrite = TRUE,
      progress = TRUE
    )
  } else {
    stop("Error in `get_rnhgis_ds()`: Extract not ready")
  }

  if (nrow(ipumsr::ipums_list_files(dwnld_file_loc)) > 1) {
    stop("Error in `get_rnhgis_ds()`: Multiple  downloaded; only single files supported")
  }

  data <- ipumsr::read_nhgis(dwnld_file_loc)

  dt_lkp <- 
    cbind(
      data.table(var = names(data), type = sapply(data, class)), 
      data %>% lapply(attributes) %>% rbindlist(fill = TRUE)
    )
  
  
  nanoparquet::write_parquet(data, save_file_loc)
  nanoparquet::write_parquet(dt_lkp, save_file_loc_lkp)
  
  file.remove(dwnld_file_loc)

  if (lkp) {
    return(dt_lkp)
  } else {
    return(data)
  }
  
}
