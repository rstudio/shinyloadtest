enum_value <- function(x, enum_id) {
  structure(x, enum_id = enum_id, class = "shinyloadtest_enum_value")
}

`==.shinyloadtest_enum_value` <- function(x, y) {
  if (class(y) != "shinyloadtest_enum_value") return(FALSE)
  identical(x, y) && (attr(x, "enum_id") == attr(y, "enum_id"))
}

enum_counter <- 0

enum <- function(...) {
  val_sym <- rlang::ensyms(...)
  val_str <- vapply(val_sym, as.character, character(1))
  enum_counter <<- enum_counter + 1
  structure(
    setNames(lapply(val_str, enum_value, enum_counter), val_str),
    class = "shinyloadtest_enum"
  )
}

`$.shinyloadtest_enum` <- function(x, i) {
  if (!(i %in% names(x))) stop("Unknown enum value")
  NextMethod()
}

enum_case <- function(field, ...) {
  stopifnot(class(field) == "shinyloadtest_enum_value")
  cases <- rlang::enexprs(..., .named = TRUE, .homonyms = "error")
  all_val <- attr(field, "all_val")

  unknown_values <- base::setdiff(names(cases), all_val)
  if (length(unknown_values)) stop(paste("Unknown enum value", unknown_values, collapse = ", "))

  missing_values <- base::setdiff(attr(field, "all_val"), names(cases))
  if (length(missing_values)) stop(paste("Missing enum value", missing_values, collapse = ", "))

  rlang::eval_tidy(cases[[attr(field, "val")]])
}

# The idea here is we have a class representing a set of named, distinct, uppercase fields:
#
# Frobs <- enum(FOO, BAR, BAZ)
#
# Partial matching of a field, or referring to an unknown field, results in an error:
#
# Frobs$FO
# Error in `$.shinyloadtest.enum`(types, FO) : Unknown field 'FO' of enum
#
# Frobs$LOL
# Error in `$.shinyloadtest.enum`(types, LOL) : Unknown field 'LOL' of enum
#
# A conditional construct, `enum_case`, is provided to dispatch
# on a field and to perform exhaustiveness checking at runtime:
#
# some_field <- Frobs$FOO
# enum_case(some_field,
#   FOO = "foo",
#   BAR = "bar"
# )
#
# Error in enum_case(some_field, FOO = "foo", BAR = "bar") :
#   missing enum value BAZ
#
# An error message is also produced if a field is mentioned that doesn't exist
# in the enum:
#
# some_field <- Frobs$FOO
# enum_case(some_field,
#   FOO = "foo",
#   BAR = "bar",
#   BAZ = "baz",
#   QUX = "qux"
# )
#
# Error in enum_case(some_field, FOO = "foo", BAR = "bar", BAZ = "baz",  :
#  unknown enum value QUX
#
# Since the exhaustiveness checking is at runtime, the use of enums doesn't provide
# any safety guarantees that couldn't be provided by e.g. assertions. However,
# if use of an enum-like construct is desirable, then using this mechanism
# provides a degree of run-time error checking without explicit assertions.
