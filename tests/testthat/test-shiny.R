test_that("placeholder works", {
  expect_true(foo())
})

library(shinytest)
test_that("hello_world_app() works", {
  skip_on_cran()
  # images cannot be compared across platforms
  expect_pass(testApp("apps/hello_world", compareImages = FALSE))
})
