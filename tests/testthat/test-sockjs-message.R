test_that("SockJS init messages are identified", {
  # Pre sub-app version of SSP
  expect_true(isSockjsInitMessage("[\"0|o|\"]"))
  # Newer, sub-app enabled version of SSP or RSC
  expect_true(isSockjsInitMessage("[\"0#0|o|\"]"))
  # SSP with app URL
  expect_true(isSockjsInitMessage("[\"0#0|o|Foo%20Bar%20Baz%C2%AE\"]"))
})

test_that("non-SockJS init messages are not identified as such", {
  expect_false(isSockjsInitMessage("[\"0|m|{\\\"method\\\":\\\"init\\\",\\\"data\\\":{\\\"days\\\":7,\\\".clientdata_output_box_width\\\":1499,\\\".clientdata_output_box_height\\\":400,\\\".clientdata_output_title_hidden\\\":false,\\\".clientdata_output_userPanel_hidden\\\":false,\\\".clientdata_output_box_hidden\\\":false,\\\".clientdata_pixelratio\\\":1,\\\".clientdata_url_protocol\\\":\\\"http:\\\",\\\".clientdata_url_hostname\\\":\\\"rstudio.local\\\",\\\".clientdata_url_port\\\":\\\"8787\\\",\\\".clientdata_url_pathname\\\":\\\"/s/d5de824806c835a7c9c83/p/c3277dbb/\\\",\\\".clientdata_url_search\\\":\\\"\\\",\\\".clientdata_url_hash_initial\\\":\\\"\\\",\\\".clientdata_singletons\\\":\\\"\\\",\\\".clientdata_allowDataUriScheme\\\":true}}\"]"))
  expect_false(isSockjsInitMessage("[\"11#0|m|{\\\"method\\\":\\\"update\\\",\\\"data\\\":{\\\"shinyalert\\\":true}}\"]"))
  expect_false(isSockjsInitMessage("[\"5#1|m|{\\\"method\\\":\\\"update\\\",\\\"data\\\":{\\\"mything\\\":\\\"0#0|o|this is some data\\\"}}\"]"))
})
