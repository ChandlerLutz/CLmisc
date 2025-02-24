#' Retrieve and cache data from the tigris package.
#'
#' This function retrieves data from the `tigris` package, caches the
#' results as an RDS file, and returns the data.  It avoids redundant
#' downloads by checking for the existence of a cached file before
#' attempting to download.
#'
#' @param tigris_fun A function from the `tigris` package.  This
#'   should be passed as a bare function name (e.g., `zctas`, not
#'   `"zctas"`).
#' @param save_dir The directory where the cached RDS files should be
#'   saved. Defaults to `here::here("data-raw/tigris/")`.
#' @param ... Additional arguments to be passed to the `tigris_fun`.
#'   These arguments will be incorporated into the name of the cached
#'   file, allowing for different cached versions based on different
#'   function arguments.
#'
#' @return An sf object containing the data retrieved from the specified `tigris_fun`.
#'
#' @details This function simplifies the process of working with the
#'   `tigris` package by automatically caching downloaded data.  It
#'   constructs a file name based on the function called and its
#'   arguments, ensuring that different datasets or different queries
#'   to the same dataset are stored separately. This prevents
#'   unnecessary downloads and speeds up subsequent access to the same
#'   data.
#'
#' The function performs several checks to ensure that the provided
#' `tigris_fun` is a valid function from the `tigris` package and that
#' the returned object is an sf object with a geometry column.
#'
#' @examples
#' \dontrun{
#' # Get ZCTAs for California and cache the result.
#' ca_zctas <- get_rtigris(zctas, state = "CA")
#'
#' # Get ZCTAs for California with a different year and cache the result separately.
#' ca_zctas_2020 <- get_rtigris(zctas, state = "CA", year = 2020)
#'
#' # Get counties for North Carolina.
#' nc_counties <- get_rtigris(counties, state = "NC")
#' }
#'
#' @import tigris
#' @export
get_rtigris <- function(tigris_fun, save_dir = here::here("data-raw/rtigris/"), ...) {

  tigris_fun_name <- deparse(substitute(tigris_fun)) %>%
    sub("tigris::", "", .)

  if (tigris_fun_name %notin% getNamespaceExports("tigris"))
    stop("Error in `get_r_tigris()`: `tigris_fun` must be a function from the tigris package")

  mkdir_p(save_dir)

  args <- list(...)
  if (length(args) > 0) {
    arg_names <- names(args)
    args_key_val_string <- paste0(arg_names,  unlist(args),
                                  collapse = "_")
  } else {
    arg_names <- NA_character_
    args_key_val_string <- ""
  }

  save_file_location <- sprintf(
    "%s/%s.rds", save_dir, paste0(tigris_fun_name, "_", args_key_val_string)
  )

  if (file.exists(here::here(save_file_location))) 
    return(readRDS(here::here(save_file_location)))

  out <- tigris_fun(...)

  if ("geometry" %notin% names(out)) 
    stop("Error in `get_r_tigris()`: `tigris_fun` must have a `geometry` column")
  if ("sfc" %notin% class(out$geometry))
    stop("Error in `get_r_tigris()`: `tigris_fun` must have a `geometry` column of class `sfc`")

  saveRDS(out, here::here(save_file_location))

  return(out)

}
