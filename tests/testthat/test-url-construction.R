test_that("basic URL without path is created", {
  url <- "http://example.com/"
  expect_equal(url, URLBuilder$new(url)$build())
})

test_that("for basic URL with path, trailing slash is preserved", {
  url <- "http://example.com/foobar/"
  expect_equal(url, URLBuilder$new(url)$build())
})

test_that("basic URL with non-standard port is created", {
  url <- "http://example.com:8383/"
  expect_equal(url, URLBuilder$new(url)$build())
})

test_that("setting raw path works", {
  url <- "http://example.com/foo/bar"
  builder <- URLBuilder$new(url)
  expect_equal("http://example.com/foop/barp", builder$setPath("foop/barp")$build())
})

test_that("appending path works", {
  url <- "http://example.com/foo/bar"
  builder <- URLBuilder$new(url)
  expect_equal("http://example.com/foo/bar/foop/barp", builder$appendPath("foop/barp")$build())
})

test_that("handles URL scheme properly", {
  url_none <- URLBuilder$new("example.com/foo/bar")$build()
  url_http <- URLBuilder$new("http://example.com/foo/bar")$build()
  url_https <- URLBuilder$new("https://example.com/foo/bar")$build()

  expect_match(url_none, "^http:")
  expect_match(url_http, "^http:")
  expect_match(url_https, "^https:")
})

test_that("basic query string round-tripping", {
  url_string <- "http://foo.com/?pizzazz=true&flair=false"
  url <- URLBuilder$new(url_string)
  expect_equal(url_string, url$build())
})

test_that("URLs with query strings can be modified", {
  url_string_before <- "http://foo.com/?pizzazz=true&flair=false"
  url_string_after <- "http://foo.com/foo?pizzazz=true&flair=false"
  url <- URLBuilder$new(url_string_before)$appendPath("foo")
  expect_equal(url_string_after, url$build())
})
