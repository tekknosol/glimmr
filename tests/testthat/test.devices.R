context("Devices")
library(tregata)

data("gasmet", "meta")
meta1 <- meta
meta1$pmbar[3] <- NA
meta$temp[3] <- NA

test_that("Flux calculation stops", {
  expect_error(preprocess_gasmet(gasmet,meta), "missing preassure or temperature values in meta file. Flux calculation stoped.")
  expect_error(preprocess_gasmet(gasmet,meta1), "missing preassure or temperature values in meta file. Flux calculation stoped.")
})

rm(meta)
data("meta")
meta1 <- meta
meta1$wndw <- NA
meta$offset <- NA
test_that("Window & Offset", {
  expect_warning(preprocess_gasmet(gasmet, meta1), "No window provided in meta file. Set to 10.")
  expect_warning(preprocess_gasmet(gasmet, meta), "No offset provided in meta file. Set to 0.")
})
