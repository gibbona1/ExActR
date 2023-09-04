library(shinytest2)

test_that("{shinytest2} recording: basic", {
  app <- AppDriver$new(variant = platform_variant(), name = "basic", seed = 123, 
      height = 563, width = 979)
  app$set_inputs(sel_crs = "4326")
  app$expect_screenshot()
})
