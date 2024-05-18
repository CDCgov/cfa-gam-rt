#' Check required inputs provided
check_required_inputs_provided <- function(cases,
                                           reference_date,
                                           group,
                                           call = caller_env()) {
  rlang::check_required(cases, "cases")
  rlang::check_required(reference_date, "reference_date")
  rlang::check_required(group, "group")
  invisible()
}

check_no_missingness <- function(x, arg = "x", call = rlang::caller_env()) {
  is_missing <- rlang::are_na(x)

  if (any(is_missing)) {
    cli::cli_abort(
      c("{.arg {arg}} has missing values",
        "i" = "Missing values are not supported in {.arg {arg}}",
        "!" = "Missing element(s): {.val {which(is_missing)}}"
      ),
      call = call,
      class = "RtGam_invalid_input"
    )
  }
}

#' Check all elements are 0 or positive
check_elements_non_neg <- function(x, arg = "x", call = rlang::caller_env()) {
  # Greater than or equal to 0 or is NA
  is_non_neg <- (x >= 0) | is.na(x)
  if (!all(is_non_neg)) {
    cli::cli_abort(
      c("{.arg {arg}} has negative elements",
        "!" = "All elements must be 0 or greater",
        "i" = "Elements {.val {which(!is_non_neg)}} are negative"
      ),
      class = "RtGam_invalid_input",
      call = call
    )
  }
  invisible()
}

#' Check vector sums to one
check_sums_to_one <- function(x,
                              arg = "x",
                              call = rlang::caller_env(),
                              tol = 1e-8) {
  diff <- abs(sum(x) - 1)
  if (diff > tol) {
    cli::cli_abort(
      c("{.arg {arg}} does not sum to one",
        "i" = "Uses a tolerance of {.val {tol}}",
        "!" = "Difference of {.val {diff}} is greater than {.val {tol}}"
      ),
      class = "RtGam_invalid_input",
      call = call
    )
  }
  invisible()
}

#' Basic type checks for input validation
#'
#' @param x Object with type checking
#' @param arg Name of the argument supplying the object
#' @param call Calling environment to be passed to the type checker
#' @name type_checker
#'
#' @return NULL, invisibly
NULL

#' @rdname type_checker
check_date <- function(x, arg = "x", call = rlang::caller_env()) {
  if ((!rlang::is_integerish(x)) || (class(x) != "Date")) {
    throw_type_error(
      object = x,
      arg_name = arg,
      expected_type = "Date",
      call = call
    )
  }
  invisible()
}

#' @rdname type_checker
check_vector <- function(x, arg = "x", call = rlang::caller_env()) {
  # Lists are bare vectors, but we want truly vanilla vectors
  if (!rlang::is_bare_vector(x) || class(x) == "list") {
    throw_type_error(
      object = x,
      arg_name = arg,
      expected_type = "vector",
      call = call
    )
  }
  invisible()
}

#' @rdname type_checker
check_integer <- function(x, arg = "x", call = rlang::caller_env()) {
  if (!rlang::is_bare_integerish(x)) {
    throw_type_error(
      object = x,
      arg_name = arg,
      expected_type = "integer",
      call = call
    )
  }
  invisible()
}

check_character <- function(x, arg = "x", call = rlang::caller_env()) {
  if (!rlang::is_bare_character(x)) {
    throw_type_error(
      object = x,
      arg_name = arg,
      expected_type = "character",
      call = call
    )
  }
  invisible()
}

#' Throw an informative type error on a user-provided input
#'
#' Follows the guidance from [rlang::abort()] on applying a call and a class
#' in the error message. Used as a base in type-checkers to throw a properly
#' formatted error.
#'
#' @param object Object with incorrect type
#' @param arg_name Name of the argument corresponding to `object`
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
  cli::cli_abort(
    c("{.arg {arg_name}} is {.obj_type_friendly {object}}",
      "i" = "Must be of type {.emph {expected_type}}"
    ),
    call = call,
    class = "RtGam_type_error"
  )
  invisible(NULL)
}
