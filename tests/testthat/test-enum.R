test_that("Enum values are equal only to themselves", {
  e1 <- enum(X, Y, Z)
  e2 <- enum(X, Y, Z)
  expect_equal(e1$X, e1$X)
  expect_false(e1$X == e2$X)
  expect_false(e1$X == "X")
})

test_that("Enums values cannot be partially matched or missing", {
  e1 <- enum(FOO, BAR)
  expect_error(e1$FO)
  expect_error(e1$BAZ)
})

test_that("enum_case dispatches to the appropriate branch", {
  e1 <- enum(FOO, BAR)
  expect_equal(123, enum_case(e1$FOO,
    FOO = 123,
    BAR = 321
  ))
})

test_that("enum_case does not evaluate other branches", {
  e1 <- enum(FOO, BAR)
  x <- 0
  fun <- function(val) {
    enum_case(val,
      FOO = 123,
      BAR = (x <<- x + 1)
    )
  }
  expect_equal(123, fun(e1$FOO))
  expect_equal(x, 0)
  expect_equal(1, fun(e1$BAR))
  expect_equal(x, 1)
})

test_that("enum_case complains about unmatched values", {
  e1 <- enum(FOO, BAR)
  expect_error({
    enum_case(val,
      FOO = 123,
    )
  })
})

test_that("enum_case complains about unknown values", {
  e1 <- enum(FOO, BAR)
  expect_error({
    enum_case(val,
      FOO = 123,
      BAR = 321,
      BAZ = 99
    )
  })
})
