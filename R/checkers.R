#' Throw an informative type error on a user-provided input
#'
#' Follows the
#' @param object Object with incorrect type
#' @param arg_name Name of the corresponding argument to `.object`
#' @param expected_type The type that the user should provide instead
#' @param call The calling environment to be reflected in the error message
#'
#' @return This function is called for its side effect of throwing an error. It
#'   should never return.
#' @importFrom rlang abort
throw_type_error <- function(object,
                             arg_name,
                             expected_type,
                             call = rlang::caller_env()) {
  abort(
    message = c("{.arg {.arg_name} has type {.obj_type_friendly {object}}",
      "i" = "Must be of type {.emph {expected_type}}"
    ),
    call = call,
    class = "RtGam_type_error"
  )
  invisible(NULL)
}
