context("Devices")
library(glimmr)
library(dplyr)

test_that("LosGatos", {
  Flux_a1_1 <- process_losgatos(losgatos, meta_losgatos) %>% filter(site == "A1", rep == 1)
  expect_equal(Flux_a1_1 %>% filter(gas == "CO2") %>% pull("F_LM"), 20.12128, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CO2") %>% pull("F_RLM"), 20.22194, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CH4") %>% pull("F_LM"), 0.007639924, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CH4") %>% pull("F_RLM"), 0.007568566, tolerance=1e-3)
})

test_that("GASMET", {
  Flux_a1_1 <- process_gasmet(gasmet, meta_gasmet) %>% filter(site == "YTMeteo", rep == 1)
  expect_equal(Flux_a1_1 %>% filter(gas == "CO2") %>% pull("F_LM"), 25.90705, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CO2") %>% pull("F_RLM"), 25.90705, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CH4") %>% pull("F_LM"), 0.0150685, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CH4") %>% pull("F_RLM"), 0.0150685, tolerance=1e-3)
})
