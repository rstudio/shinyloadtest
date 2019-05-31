seteq <- function(v1, v2) {
  stopifnot(is.vector(v1), is.vector(v2), typeof(v1) == typeof(v2))
  if (length(v1) != length(v2)) return(FALSE)
  for (x in v1) if (!(x %in% v2)) return(FALSE)
  TRUE
}

enum <- function(...) {
  fields <- as.character(eval(substitute(alist(...))))
  stopifnot(all(grepl("^[[:upper:]]+$", fields)))
  stopifnot(seteq(fields, unique(fields)))
  structure(list(fields = fields), class = "shinyloadtest.enum")
}

`$.shinyloadtest.enum` <- function(x, fld) {
  m <- match(fld, x[["fields"]])
  if (is.na(m)) stop(paste0("Unknown field '", fld, "' of enum"))
  m
}

types <- enum(FOO, BAR, BAZ)
types$FOO
types$LOL # Error in `$.shinyloadtest.enum`(types, LOL) : Unknown field 'LOL' of enum

enum_case <- function(enum, ...) {
  stopifnot(class(enum) == "shinyloadtest.enum")
  # TODO check exhaustiveness, eval matching field handler
}

# enum_case(types,
#   FOO = "foo",
#   BAR = "bar"
# )
#
# Error: field BAZ of enum 'types' is not handled

