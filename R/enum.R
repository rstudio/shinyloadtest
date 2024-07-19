enum_value <- function(x, enum_id, all_val) {
  structure(
    x,
    enum_id = enum_id,
    all_val = all_val,
    class = "shinyloadtest_enum_value"
  )
}

#' @export
`==.shinyloadtest_enum_value` <- function(x, y) {
  if (!inherits(y, "shinyloadtest_enum_value")) {
    return(FALSE)
  }
  identical(x, y) && (attr(x, "enum_id") == attr(y, "enum_id"))
}

enum_counter <- local({
  n <- 0L
  function() (n <<- n + 1L)
})

enum <- function(...) {
  val_sym <- rlang::ensyms(...)
  val_str <- vapply(val_sym, as.character, character(1))
  structure(
    stats::setNames(lapply(val_str, enum_value, enum_counter(), val_str), val_str),
    class = "shinyloadtest_enum"
  )
}

#' @export
`$.shinyloadtest_enum` <- function(x, i) {
  if (!(i %in% names(x))) cli::cli_abort("Unknown enum value")
  NextMethod()
}
#' @export
`[[.shinyloadtest_enum` <- `$.shinyloadtest_enum`
#' @export
`==.shinyloadtest_enum` <- function(x, y) {
  identical(x, y)
}

enum_case <- function(field, ...) {
  stopifnot(inherits(field, "shinyloadtest_enum_value"))
  cases <- rlang::enquos(..., .named = TRUE, .homonyms = "error")
  all_val <- attr(field, "all_val")

  unknown_values <- base::setdiff(names(cases), all_val)
  if (length(unknown_values)) {
    cli::cli_abort(paste("Unknown enum value", paste(unknown_values, collapse = ", ")), call = NULL)
  }

  missing_values <- base::setdiff(all_val, names(cases))
  if (length(missing_values)) {
    cli::cli_abort(paste("Missing enum value", paste(missing_values, collapse = ", ")), call = NULL)
  }

  rlang::eval_tidy(cases[[field]])
}

# The idea here is we have an object, Frobs, representing a set of named, distinct, values:
#
# Frobs <- enum(FOO, BAR, BAZ)
#
# Values are equal to themselves:
#
# Frobs$FOO == Frobs$FOO
# [1] TRUE
#
# Values are not equal to values from other enums:
#
# Blobs <- enum(FOO, BAR, BAZ)
#
# Frobs$FOO == Blobs$FOO
# [1] FALSE
#
# Partial matching of a value, or referring to an unknown value, produces an error:
#
# Frobs$FO
# Error in `$.shinyloadtest_enum`(Frobs, FO) : Unknown enum value
#
# A conditional construct, `enum_case`, is provided to dispatch
# on a value and to perform exhaustiveness checking of the other values from the enum
# at runtime:
#
# some_field <- Frobs$FOO
# enum_case(some_field,
#   FOO = "foo",
#   BAR = "bar"
# )
#
# Error: Missing enum value BAZ
#
# An error message is also produced if a field is mentioned that doesn't exist
# in the enum:
#
# some_val <- Frobs$FOO
# enum_case(some_val, FOO = 1, BAR = 2, BAZ = 3, QUX = 3)
# enum_case(some_field,
#   FOO = "foo",
#   BAR = "bar",
#   BAZ = "baz",
#   QUX = "qux"
# )
#
# Error: Unknown enum value QUX
#
# Since the exhaustiveness checking is at runtime, the use of enums doesn't provide
# any safety guarantees that couldn't be provided by e.g. assertions. However,
# if use of an enum-like construct is desirable, then using this mechanism
# provides a degree of run-time error checking without explicit assertions.
