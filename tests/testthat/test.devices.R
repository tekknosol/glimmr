context("Devices")
library(glimmr)

data("gasmet", "meta_gasmet")
meta_gasmet$temp[3] <- NA

test_that("Flux calculation stops", {
  expect_error(preprocess_gasmet(gasmet,meta_gasmet), "missing temperature values in meta file. Flux calculation stoped.")
})

rm(meta_gasmet)
data("meta_gasmet")
meta_gasmet1 <- meta_gasmet
meta_gasmet1$wndw <- NA
meta_gasmet$offset <- NA
test_that("Window & Offset", {
  expect_warning(process_gasmet(gasmet, meta_gasmet1, pre = T), "No window provided in meta file. Set to 10.")
  expect_warning(process_gasmet(gasmet, meta_gasmet, pre = T), "No offset provided in meta file. Set to 0.")
})

rm(meta_gasmet)
data("meta_gasmet")
test_that("No NA in preprocessed data",{
  expect_false(TRUE %in% is.na(process_gasmet(gasmet, meta_gasmet, pre = T)))
})
