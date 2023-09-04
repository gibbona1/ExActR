library(shinytest2)

test_that("{shinytest2} recording: basic", {
  app <- AppDriver$new(variant = platform_variant(), name = "basic", seed = 123, 
                       height = 563, width = 979)
  app$set_inputs(sel_crs = "4326")
  app$expect_screenshot()
})


test_that("{shinytest2} recording: extent_tables", {
  app <- AppDriver$new(name = "extent_tables", seed = 123, height = 563, width = 979)
  app$set_inputs(sel_crs = "4326")
  app$upload_file(sf1 = "../../ext_data/Hazelwood/hazelwood_CLC2000.zip")
  app$upload_file(sf2 = "../../ext_data/Hazelwood/hazelwood_CLC2006.zip")
  app$set_inputs(map2_sel_col = "CODE_06")
  app$set_inputs(map1_sel_col = "CODE_00")
  app$click("gen_extent")
  app$wait_for_value(output = "extentTable_2", timeout = 5000)
  app$expect_values()
})



test_that("{shinytest2} recording: plots", {
  app <- AppDriver$new(variant = platform_variant(), name = "plots", seed = 456, 
                       height = 563, width = 979)
  app$set_inputs(sel_crs = "4326")
  app$upload_file(sf1 = "../../ext_data/Hazelwood/hazelwood_CLC2000.zip")
  app$upload_file(sf2 = "../../ext_data/Hazelwood/hazelwood_CLC2006.zip")
  app$set_inputs(map2_sel_col = "CODE_06")
  app$set_inputs(map1_sel_col = "CODE_00")
  app$click("gen_extent")
  app$set_inputs(mainTabs = "Composition Plots")
  app$wait_for_value(output = "plotStack", timeout = 5000)
  app$expect_screenshot()
})


test_that("{shinytest2} recording: download", {
  app <- AppDriver$new(variant = platform_variant(), name = "download", seed = 111, 
                       height = 563, width = 979)
  app$set_inputs(sel_crs = "4326")
  app$upload_file(sf1 = "../../ext_data/Hazelwood/hazelwood_CLC2000.zip")
  app$upload_file(sf2 = "../../ext_data/Hazelwood/hazelwood_CLC2006.zip")
  app$set_inputs(map2_sel_col = "CODE_06")
  app$set_inputs(map1_sel_col = "CODE_00")
  app$click("gen_extent")
  app$set_inputs(mainTabs = "Composition Plots")
  app$wait_for_idle()
  app$expect_download("download_plotComp")
  app$wait_for_idle()
  app$expect_download("download_plotStack")
  app$wait_for_idle()
  app$expect_download("download_plotMap1")
  app$wait_for_idle()
  app$expect_download("download_plotMap2")
})