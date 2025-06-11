# This environment will store the cached modules.
# It's not exported, so it's internal to 'myutils'.
# parent = emptyenv() means it doesn't inherit from anything,
# which is good for a simple key-value store.
.module_cache <- new.env(parent = emptyenv())

#' Load an R Script into a Managed Environment with Caching
#'
#' Sources an R script into a new environment. The resolved, absolute file path
#' is used as the unique identifier for caching. Subsequent calls for the same
#' file path during the same R session will return the cached environment,
#' avoiding re-sourcing.
#'
#' @param file_path Character string: The path to the R script to load.
#'        Relative paths are resolved to an absolute path.
#' @param parent_env The parent environment for the new environment created for
#'        the module's code. Defaults to \code{.GlobalEnv}.
#' @param refresh Logical: If \code{TRUE}, forces the module to be reloaded
#'        and re-cached, even if it's already in the cache. Defaults to \code{FALSE}.
#'
#' @return An environment containing the objects created by sourcing \code{file_path}.
#' @export
#' @examples
#' \dontrun{
#'   # Ensure your package is loaded if testing interactively, e.g., devtools::load_all()
#'   # or library(myutils) if installed.
#'
#'   # Create a dummy R file for demonstration
#'   temp_r_file <- tempfile(fileext = ".R")
#'   # Ensure cleanup even if errors occur
#'   on.exit(unlink(temp_r_file, force = TRUE), add = TRUE)
#'   writeLines(c("message('Sourcing temp_r_file...')",
#'                "secret_value <- 42",
#'                "get_secret <- function() secret_value",
#'                "add_to_secret <- function(x) secret_value + x"), temp_r_file)
#'
#'   # --- First load ---
#'   # (Will print 'Sourcing temp_r_file...')
#'   mod1 <- load_module(temp_r_file)
#'   print(paste("Value from mod1:", mod1$get_secret()))
#'   print(paste("Calculation from mod1:", mod1$add_to_secret(8)))
#'
#'   # --- Second load (should be from cache) ---
#'   # (Should NOT print 'Sourcing temp_r_file...' again)
#'   mod2 <- load_module(temp_r_file) # Same file path, so uses cache
#'   stopifnot(identical(mod1, mod2)) # Should be the exact same environment object
#'   print("mod1 and mod2 are identical (from cache).")
#'
#'   # --- Force refresh ---
#'   # (Will print 'Sourcing temp_r_file...' again)
#'   mod3 <- load_module(temp_r_file, refresh = TRUE)
#'   stopifnot(!identical(mod1, mod3)) # A new environment is created
#'   print(paste("Value from mod3 (refreshed):", mod3$get_secret()))
#'   print("mod1 and mod3 are NOT identical (refreshed).")
#'
#'   # --- Test with a path that needs normalization (conceptual) ---
#'   # Create a file in a temporary subdirectory
#'   temp_dir <- file.path(tempdir(), "my_temp_module_dir")
#'   if (!dir.exists(temp_dir)) dir.create(temp_dir)
#'   on.exit(unlink(temp_dir, recursive = TRUE, force = TRUE), add = TRUE)
#'
#'   file_in_subdir <- file.path(temp_dir, "another_mod.R")
#'   writeLines("message('Sourcing another_mod.R...'); module_var <- 'hello'", file_in_subdir)
#'
#'   # Load using a relative path (if current dir allows) or modified path
#'   # This part of example assumes you might have paths like "./file.R" vs "file.R"
#'   # For simplicity, let's use the direct path and one that might be constructed
#'   mod_a <- load_module(file_in_subdir)
#'
#'   # Construct a path that might look different but resolves to the same place
#'   # e.g., by adding redundant "./" if in that directory
#'   # For this example, we'll just use the same path again to show caching
#'   mod_b <- load_module(paste0(temp_dir, "/another_mod.R")) # Ensure same path format
#'   # A more robust test for normalization would involve relative paths like "."
#'   # if (file.exists("./temp_test_dir/another_mod.R")){
#'   #    mod_c <- load_module("./temp_test_dir/another_mod.R")
#'   #    stopifnot(identical(mod_a, mod_c))
#'   # }
#'   stopifnot(identical(mod_a, mod_b)) # Should be cached if paths normalize identically
#'   print("Normalized paths to the same file correctly use the cache.")
#' }
load_module <- function(file_path,
                        parent_env = .GlobalEnv,
                        refresh = FALSE) {

  if (!is.character(file_path) || length(file_path) != 1) {
    stop("'file_path' must be a single character string.", call. = FALSE)
  }

  if (!file.exists(file_path)) {
    stop(paste("File does not exist at the provided path:", file_path), call. = FALSE)
  }

  # Normalize the file path to get its canonical, absolute form.
  # This ensures that different string representations of the same file
  # (e.g., relative vs. absolute, or paths with redundant separators)
  # resolve to the same unique ID for caching.
  # Since we've checked file.exists, mustWork = TRUE is safe here.
  module_cache_id <- normalizePath(file_path, mustWork = TRUE)

  if (!is.environment(parent_env)) {
    stop("'parent_env' must be an environment.", call. = FALSE)
  }
  if (!is.logical(refresh) || length(refresh) != 1) {
    stop("'refresh' must be a single logical value.", call. = FALSE)
  }

  # --- Cache Logic ---
  # Access the internal cache (.module_cache) from the package's namespace.
  # This assumes .module_cache is defined in your package (e.g., in R/zzz.R)
  # and is therefore accessible in the lexical scope of this function.
  if (!refresh && exists(module_cache_id, envir = .module_cache, inherits = FALSE)) {
    # message(paste("Returning cached module:", module_cache_id)) # For debugging
    return(.module_cache[[module_cache_id]])
  }

  # --- Load or Reload Module ---
  # message(paste("Loading module:", module_cache_id)) # For debugging
  loaded_env <- new.env(parent = parent_env)

  tryCatch({
    # Use the normalized path (module_cache_id) for sourcing for maximum consistency.
    source(module_cache_id, local = loaded_env, chdir = TRUE)
  }, error = function(e) {
    # If sourcing fails during a refresh operation and an old version was in cache,
    # it's good practice to remove the (now potentially stale) old entry.
    # Or, more simply, just don't add the failed 'loaded_env' to cache.
    # The current logic only adds to cache on success.
    # If refresh was TRUE and it failed, the old item (if any) needs to be explicitly
    # removed to ensure the next non-refresh call doesn't get a stale version.
    if (refresh && exists(module_cache_id, envir = .module_cache, inherits = FALSE)) {
      rm(list = module_cache_id, envir = .module_cache)
    }
    stop(paste0("Error sourcing file '", module_cache_id, "'.\nOriginal error: ",
                conditionMessage(e)), call. = FALSE)
  })

  # Store the newly loaded environment in the cache
  .module_cache[[module_cache_id]] <- loaded_env

  return(loaded_env)
}
