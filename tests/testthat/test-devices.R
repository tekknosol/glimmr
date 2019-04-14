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

test_that("Chamber", {
  lg <- analyzer(
    #define datastructure
    time_stamp = "Time",
    conc_columns = c(CH4 = "[CH4]_ppm", CO2 = "[CO2]_ppm"),
    preassure = "GasP_torr",
    preassure_factor = 1.33322,
    temperature = "AmbT_C",
    trimmer = trim_time,
    # define metadata structure
    spot = "spot",
    day = "day",
    start = "start",
    end = "end",
    #define chamber dimensions
    V = 0.01461,
    A = 0.098
  )
  Flux_a1_1 <- process_chamber(losgatos, meta_losgatos, lg) %>% filter(site == "A1", rep == 1)
  expect_equal(Flux_a1_1 %>% filter(gas == "CO2") %>% pull("F_LM"), 20.12128, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CO2") %>% pull("F_RLM"), 20.22194, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CH4") %>% pull("F_LM"), 0.007639924, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CH4") %>% pull("F_RLM"), 0.007568566, tolerance=1e-3)
})
