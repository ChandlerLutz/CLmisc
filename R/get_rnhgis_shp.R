#' Download and cache NHGIS shapefiles.
#'
#' This function downloads shapefiles from the NHGIS (National Historical
#' Geographic Information System) using the `ipumsr` package.  It caches
#' downloaded shapefiles as RDS files to avoid redundant downloads.  It requires
#' an IPUMS API key to be set as an environment variable named
#' `IPUMS_API_KEY`.
#'
#' @param shp A character vector of length one specifying the name of the
#'   shapefile to download.  This name corresponds to the shapefile codes used
#'   by NHGIS (e.g., "us_state_2020_tl2020").  See the `ipumsr` documentation for
#'   how to find these codes.
#' @param save_dir The directory where the downloaded and processed shapefiles
#'   will be saved as RDS files.  Defaults to `"data-raw/rnhgis_shp/"` using
#'   `here::here()`.  This directory will be created if it doesn't exist.
#'
#' @returns An `sf` object containing the downloaded shapefile.
#'
#' @examples
#' \dontrun{
#' # Download and cache the 2020 US counties shapefile.
#' states <- get_rnhgis_shp("us_state_2020_tl2020")
#'
#' # Subsequent calls with the same 'shp' argument will load the cached
#' # shapefile.
#' states_cached <- get_rnhgis_shp("counties")
#' identical(states, states_cached) # TRUE
#' }
#'
#' @importFrom ipumsr define_extract_nhgis submit_extract wait_for_extract
#' @importFrom ipumsr is_extract_ready download_extract read_ipums_sf
#'
#' @export
get_rnhgis_shp <- function(shp, save_dir = here::here("data-raw/rnhgis_shp/")) {

  if (length(shp) != 1) 
    stop("Error in `get_rnhgis_shp()`: `shp` must be a single string with the name of the shapefile to download")

  ## Check for an NHGIS key in the environment
  if (!"IPUMS_API_KEY" %in% names(Sys.getenv())) 
    stop("Error in `get_rnhgis_shp()`: IPUMS_API_KEY not found in environment")

  mkdir_p(save_dir)
  save_dir_zips <- paste0(save_dir, "zip_files/")
  mkdir_p(save_dir_zips)

  save_file_loc <- here::here(save_dir, paste0(shp, ".rds"))

  if (file.exists(save_file_loc)) 
    return(readRDS(save_file_loc))

  nhgis_shp_ext <- ipumsr::define_extract_nhgis(
    description = paste("Shapefile request", Sys.time()),
    shapefiles = shp
  )

  nhgis_shp_ext_submitted <- ipumsr::submit_extract(nhgis_shp_ext)

  shp_extract_complete <- ipumsr::wait_for_extract(nhgis_shp_ext_submitted)

  if (is_extract_ready(shp_extract_complete)) {
    dwnld_file_loc <- ipumsr::download_extract(
      shp_extract_complete,
      download_dir = save_dir_zips,
      overwrite = TRUE,
      progress = TRUE
    )
  } else {
    stop("Error in `get_rnhgis_shp()`: Extract not ready")
  }

  sf <- ipumsr::read_ipums_sf(dwnld_file_loc)

  saveRDS(sf, save_file_loc)

  file.remove(dwnld_file_loc)

  return(sf)
}
