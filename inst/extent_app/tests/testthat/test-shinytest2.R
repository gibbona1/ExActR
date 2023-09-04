library(shinytest2)

test_that("{shinytest2} recording: basic", {
  app <- AppDriver$new(variant = platform_variant(), name = "basic", seed = 123, 
      height = 563, width = 979)
  app$set_inputs(sel_crs = "4326")
  app$expect_screenshot()
})


test_that("{shinytest2} recording: extent tables", {
  app <- AppDriver$new(name = "extent tables", seed = 123, height = 563, width = 979)
  app$set_inputs(sel_crs = "4326")
  app$upload_file(sf1 = "../../../../Hazelwood/hazelwood_CLC2000.zip")
  app$upload_file(sf2 = "../../../../Hazelwood/hazelwood_CLC2006.zip")
  app$set_inputs(map2_sel_col = "CODE_06")
  app$set_inputs(map1_sel_col = "CODE_00")
  app$click("gen_extent")
  app$expect_values()
})
