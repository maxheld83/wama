library(shinytest)
test_that("greeting_app() works", {
  app <- shinytest::ShinyDriver$new(greeting_app())
  app$setInputs(name = "Max")
  expect_equal(app$getValue("greeting"), "Hi Max")

  app$click("reset")
  expect_equal(app$getValue("greeting"), "")
})
