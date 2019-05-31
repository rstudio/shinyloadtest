enum_field <- function(name) {
  env <- as.environment(list(
    name = name,
    parent = NULL,
    set_parent = function(parent) (env$parent <- parent)
  ))
  structure(env, class = "shinyloadtest.enum_field")
}

`==.shinyloadtest.enum_field` <- function(f1, f2) {
  if (!(class(f2) == "shinyloadtest.enum_field")) return(FALSE)
  stopifnot(!is.null(f1$parent) && !is.null(f2$parent))
  (f1$parent == f2$parent) && (f1$name == f2$name)
}

enum <- function(...) {
  # TODO Use rlang for ... here
  fields <- as.character(eval(substitute(alist(...))))
  stopifnot(all(grepl("^[[:upper:]]+$", fields)))
  stopifnot(base::setequal(fields, unique(fields)))
  enum_fields <- lapply(fields, function(f) enum_field(f))
  self <- structure(list(fields = enum_fields), class = "shinyloadtest.enum")
  for (field in enum_fields) field$set_parent(self)
  self
}

enum_fieldnames <- function(enum) {
  vapply(enum[["fields"]], function(field) field$name, character(1))
}

`==.shinyloadtest.enum` <- identical

`$.shinyloadtest.enum` <- function(x, fld) {
  for (f in x[["fields"]]) if (f$name == fld) return(f)
  stop(paste0("Unknown field '", fld, "' of enum"))
}

enum_case <- function(field, ...) {
  stopifnot(class(field) == "shinyloadtest.enum_field")
  cases <- rlang::enexprs(..., .named = TRUE, .homonyms = "error")
  unknown_fields <- base::setdiff(names(cases), enum_fieldnames(field$parent))
  if (length(unknown_fields)) stop(paste("Unknown field names", unknown_fields))
  missing_fields <- base::setdiff(enum_fieldnames(field$parent), names(cases))
  if (length(missing_fields)) stop(paste("Missing field names", missing_fields))
  rlang::eval_tidy(cases[[field$name]])
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
# some_field <- frobs$FOO
# enum_case(some_field,
#   FOO = "foo",
#   BAR = "bar"
# )
#
# Error in enum_case(some_field, FOO = "foo", BAR = "bar") :
#   Missing field names BAZ
#
# An error message is also produced if a field is mentioned that doesn't exist
# in the enum:
#
# some_field <- frobs$FOO
# enum_case(some_field,
#   FOO = "foo",
#   BAR = "bar",
#   BAZ = "baz",
#   QUX = "qux"
# )
#
# Error in enum_case(some_field, FOO = "foo", BAR = "bar", BAZ = "baz",  :
#  Unknown field names QUX
#
# Since the exhaustiveness checking is at runtime, the use of enums doesn't provide
# any safety guarantees that couldn't be provided by e.g. assertions. However,
# if use of an enum-like construct is desirable, then using this mechanism
# provides a degree of run-time error checking without explicit assertions.
