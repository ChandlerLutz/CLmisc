
#' Load Specified Objects from an R Script File
#'
#' @description
#' Sources an R script into an isolated temporary environment and then copies
#' specified objects (typically functions) from that environment into a
#' target environment. This allows for selective loading of code without
#' polluting the target environment or running unintended code from the script.
#' Since this function uses automatic loading, only use it when you are confident
#' the script does not contain any side effects that could alter the state of
#' the target environment.
#'
#' @param file A character string: the path to the R script file (must have a
#'   `.R` or `.r` extension).
#' @param fs A character vector (optional): the names of the objects to load
#'   from the `file`. If `NULL` (the default), all objects created by sourcing
#'   the `file` will be loaded.
#' @param mode A character string: the mode of the objects to be retrieved
#'   (e.g., "function", "numeric", "list"). Passed to `get()`.
#'   Defaults to `"function"`.
#' @param envir The environment to which the objects should be assigned.
#'   Defaults to `parent.frame()`, which is the environment where
#'   `load_functions` was called.
#'
#' @return Invisibly returns a character vector of the names of the objects
#'   that were successfully loaded into the `envir`.
#'
#' @details
#' The function first validates the `file` argument to ensure it's a single,
#' existing R script. It then creates a new, empty (base) environment and sources the
#' `file` into this temporary environment using `source(file, local = temp_env)`.
#' This isolation prevents the sourced script from affecting any other environments
#' directly.
#'
#' If `fs` is provided, the function checks if all specified objects in `fs` exist
#' in the temporary environment. If any are missing, it stops with an error
#' listing the missing objects.
#'
#' If `fs` is `NULL`, all objects found in the temporary environment (including
#' those starting with a dot, as `ls(all.names = TRUE)` is used) are selected for loading.
#'
#' Each selected object is then retrieved from the temporary environment using
#' `get()` with the specified `mode` and `inherits = FALSE`, and assigned to the
#' target `envir` using `assign()`.
#'
#' @examples
#' \dontrun{
#' # Create a dummy R script file
#' temp_script_file <- tempfile(fileext = ".R")
#' writeLines(c(
#'   "my_func_1 <- function(x) x + 1",
#'   "my_data <- data.frame(a = 1:3, b = letters[1:3])",
#'   ".hidden_func <- function() 'secret'",
#'   "another_func <- function(y) y * 2"
#' ), temp_script_file)
#'
#' # --- Example 1: Load all objects ---
#' # (Note: my_func_1, my_data, .hidden_func, another_func will be loaded)
#' loaded_all <- load_functions(temp_script_file)
#' print(loaded_all)
#' print(ls()) # See the loaded objects in the current environment
#' print(my_func_1(10))
#' print(my_data)
#' # .hidden_func() # This would also be available
#'
#' # Clean up for next example
#' rm(my_func_1, my_data, .hidden_func, another_func, loaded_all)
#'
#' # --- Example 2: Load specific functions ---
#' loaded_specific <- load_functions(temp_script_file, fs = c("my_func_1", "another_func"))
#' print(loaded_specific)
#' print(ls()) # Only my_func_1 and another_func should be loaded
#' print(my_func_1(5))
#' print(another_func(5))
#' # try(print(my_data)) # This would error as my_data was not loaded
#'
#' # Clean up
#' rm(my_func_1, another_func, loaded_specific)
#'
#' # --- Example 3: Load a specific data object by changing mode ---
#' loaded_data <- load_functions(temp_script_file, fs = "my_data", mode = "any")
#' print(loaded_data)
#' print(my_data)
#'
#' # Clean up
#' rm(my_data, loaded_data)
#'
#' # --- Example 4: Attempt to load a non-existent function ---
#' tryCatch(
#'   load_functions(temp_script_file, fs = c("my_func_1", "non_existent_func")),
#'   error = function(e) print(e$message)
#' )
#'
#' # --- Example 5: Load into a different environment ---
#' target_env <- new.env()
#' load_functions(temp_script_file, fs = "my_func_1", envir = target_env)
#' print(ls(envir = target_env)) # Should show "my_func_1"
#' print(target_env$my_func_1(100))
#'
#' # Clean up the dummy file
#' file.remove(temp_script_file)
#' }
#'
#' @seealso \code{\link[base]{source}}, \code{\link[base]{get}},
#'   \code{\link[base]{assign}}, \code{\link[base]{new.env}},
#'   \code{\link[base]{ls}}, \code{\link[base]{parent.frame}}
#'
#' @keywords utilities environment programming
#' @export
load_functions <- function(file, fs = NULL, mode = "function", envir = parent.frame()) {
  stopifnot(
    is.character(file), length(file) == 1, file.exists(file),
    grepl("\\.R$", file)
  )

  env <- new.env(parent = baseenv())
  source(file, local = env)

  all_objects <- ls(envir = env, all.names = TRUE)

  if (is.null(fs)) {
    fs <- all_objects
  } else {
    missing <- setdiff(fs, all_objects)
    if (length(missing) > 0) {
      stop(sprintf("The following objects were not found in '%s': %s",
                   file, paste(missing, collapse = ", ")), call. = FALSE)
    }
  }

  for (fname in fs) {
    obj <- get(fname, envir = env, mode = mode, inherits = FALSE)
    assign(fname, obj, envir = envir)
  }

  invisible(fs)
}
