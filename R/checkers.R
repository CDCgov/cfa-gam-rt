check_vector_length <- function(
    n,
    name,
    min,
    max,
    call = rlang::caller_env()) {
  if (!rlang::is_na(min)) {
    if (n < min) {
      cli::cli_abort(
        c("{.arg {name}} requires a minimum length of {.val {min}}",
          "i" = "{.arg {name}} is of length {.val {n}}"
        ),
        class = "RtGam_invalid_input",
        call = call
      )
    }
  }
  if (!rlang::is_na(max)) {
    if (n > max) {
      cli::cli_abort(
        c("{.arg {name}} requires a maximum length of {.val {max}}",
          "i" = "{.arg {name}} is of length {.val {n}}"
        ),
        class = "RtGam_invalid_input",
        call = call
      )
    }
  }
  invisible()
}

check_vectors_equal_length <- function(
    cases,
    reference_date,
    group,
    call = rlang::caller_env()) {
  if (rlang::is_null(group)) {
    args <- c("cases", "reference_date")
    lengths <- c(length(cases), length(reference_date))
    all_equal <- length(cases) == length(reference_date)
  } else {
    args <- c("cases", "reference_date", "group")
    lengths <- c(length(cases), length(reference_date), length(group))
    all_equal <- all(
      length(cases) == length(reference_date),
      length(cases) == length(group)
    )
  }

  if (!all_equal) {
    cli::cli_abort(
      c(
        "{.arg {args}} must be the same length",
        "i" = "{.arg {args}} are of lengths {.val {lengths}}"
      ),
      class = "RtGam_invalid_input",
      call = call
    )
  }
  invisible()
}

check_dates_unique <- function(
    reference_date,
    group,
    call = rlang::caller_env()) {
  # Two cases:
  ## (1) There are no groups -- need to check that all dates are unique
  ## (2) There **are** groups -- check that dates unique _within each group_

  # Case (1): No groups
  if (rlang::is_null(group)) {
    if (length(unique(reference_date)) != length(reference_date)) {
      duplicate_table <- table(reference_date)
      duplicates <- names(which(duplicate_table > 1))
      cli::cli_abort(
        c("{.arg reference_date} has duplicate values",
          "!" = "Dates can only occur once. Did you mean to provide groups?",
          "i" = "Duplicate dates: {.val {duplicates}}"
        ),
        class = "RtGam_invalid_input",
        call = call
      )
    }
    # Case (2): Groups
  } else {
    groups <- unique(group)
    for (g in groups) {
      dates_in_group <- reference_date[which(group == g)]
      if (length(unique(dates_in_group)) != length(dates_in_group)) {
        duplicate_table <- table(dates_in_group)
        duplicates <- names(which(duplicate_table > 1))
        cli::cli_abort(
          c(
            "{.arg reference_date} has duplicates in {.arg group} {.val {g}}",
            "!" = "Dates can only occur once per group",
            "i" = "Duplicates in {.arg group} {.val {g}}: {.val {duplicates}}"
          ),
          class = "RtGam_invalid_input",
          call = call
        )
      }
    }
  }
  invisible()
}

check_required_inputs_provided <- function(
    cases,
    reference_date,
    group,
    k,
    m,
    backend,
    call = rlang::caller_env()) {
  rlang::check_required(cases, "cases", call = call)
  rlang::check_required(reference_date, "reference_date", call = call)
  rlang::check_required(group, "group", call = call)
  rlang::check_required(k, "k", call = call)
  rlang::check_required(m, "m", call = call)

  invisible()
}

check_no_missingness <- function(
    x,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()) {
  is_missing <- rlang::are_na(x)

  if (any(is_missing)) {
    cli::cli_abort(
      c("{.arg {arg}} has missing values",
        "i" = "Missing values are not supported in {.arg {arg}}",
        "!" = "Missing element(s) index: {.val {which(is_missing)}}"
      ),
      call = call,
      class = "RtGam_invalid_input"
    )
  }
}

check_elements_below_max <- function(
    x,
    arg = rlang::caller_arg(x),
    max,
    call = rlang::caller_env()) {
  # Greater than or equal to 0 or is NA
  is_below_max <- (x <= max) | is.na(x)
  if (!all(is_below_max)) {
    cli::cli_abort(
      c("{.arg {arg}} has elements larger than {.val {max}}",
        "!" = "All elements must be {.val {max}} or less",
        "i" = "Elements {.val {which(!is_below_max)}} are larger"
      ),
      class = "RtGam_invalid_input",
      call = call
    )
  }
  invisible()
}


check_elements_above_min <- function(
    x,
    arg = rlang::caller_arg(x),
    min,
    call = rlang::caller_env()) {
  # Greater than or equal to 0 or is NA
  is_above_min <- (x >= min) | is.na(x)
  if (!all(is_above_min)) {
    cli::cli_abort(
      c("{.arg {arg}} has elements smaller than {.val {min}}",
        "!" = "All elements must be {.val {min}} or greater",
        "i" = "Elements {.val {which(!is_above_min)}} are smaller"
      ),
      class = "RtGam_invalid_input",
      call = call
    )
  }
  invisible()
}

check_sums_to_one <- function(
    x,
    arg = rlang::caller_arg(x),
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
#' @noRd
NULL

#' @rdname type_checker
#' @noRd
check_date <- function(
    x,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()) {
  if (rlang::is_character(x)) {
    casted <- rlang::try_fetch(
      as.Date(x),
      error = function(con) {
        cli::cli_abort(
          c(
            "{.arg {arg}} {.val {x}} could not be automatically cast to date.",
            "Try explicitly converting with {.fn as.Date}"
          ),
          call = call,
          class = "failed_to_cast_date"
        )
      },
      call = call
    )
    cli::cli_alert(c(
      "Casting {.arg {arg}} {.obj_type_friendly {x}} ",
      "{.arg {head(x)}} to {.obj_type_friendly {casted}} ",
      "{.val {stringify_date(head(casted))}}"
    ))
    x <- casted
  }
  if ((!rlang::is_integerish(x)) || (!inherits(x, "Date"))) {
    throw_type_error(
      object = x,
      arg_name = arg,
      expected_type = "Date",
      call = call
    )
  }
  invisible(x)
}

#' @rdname type_checker
#' @noRd
check_vector <- function(
    x,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()) {
  # Lists are bare vectors, but we want truly vanilla vectors
  if (!rlang::is_bare_vector(x) || inherits(x, "list")) {
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
#' @noRd
check_integer <- function(
    x,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()) {
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

check_character <- function(
    x,
    arg = rlang::caller_arg(x),
    call = rlang::caller_env()) {
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
#' @noRd
throw_type_error <- function(
    object,
    arg_name = rlang::caller_arg(object),
    expected_type,
    call = rlang::caller_env()) {
  cli::cli_abort(
    c("{.arg {arg_name}} is {.obj_type_friendly {object}}",
      "i" = "Must be of type {.emph {expected_type}}"
    ),
    call = call,
    class = "RtGam_type_error"
  )
}
