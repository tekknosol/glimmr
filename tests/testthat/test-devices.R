library(glimmr)
library(dplyr)

# test fluxcalculation with losgatos
test_that("LosGatos", {
  Flux_a1_1 <- process_losgatos(losgatos, meta_losgatos) %>% filter(site == "plotA", rep == 1)
  expect_equal(Flux_a1_1 %>% filter(gas == "CO2") %>% pull("F_LM"), 20.12128, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CO2") %>% pull("F_RLM"), 20.22194, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CH4") %>% pull("F_LM"), 0.007639924, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CH4") %>% pull("F_RLM"), 0.007568566, tolerance=1e-3)

  # expect_output(str(as.data.frame(read_losgatos("../../data-raw/2018-05-23/"))), "1682 obs")
})

# test fluxcalculation with gasmet
test_that("GASMET", {
  Flux_a1_1 <- process_gasmet(gasmet, meta_gasmet) %>% filter(site == "plotB", rep == 1)
  expect_equal(Flux_a1_1 %>% filter(gas == "CO2") %>% pull("F_LM"), 25.90705, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CO2") %>% pull("F_RLM"), 25.90705, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CH4") %>% pull("F_LM"), 0.0150685, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CH4") %>% pull("F_RLM"), 0.0150685, tolerance=1e-3)

  # expect_output(str(as.data.frame(read_gasmet("../../data-raw/gasmet.csv"))), "399 obs")
  expect_error(read_gasmet("dummy.csv"), "File doesn't seem to be a supported GASMET file")
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
  expect_true(is.lyzr(lg))
  expect_true(is.lyzr(validate(lg)))
  Flux_a1_1 <- process_chamber(losgatos, meta_losgatos, lg) %>% filter(site == "plotA", rep == 1)
  expect_equal(Flux_a1_1 %>% filter(gas == "CO2") %>% pull("F_LM"), 20.12128, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CO2") %>% pull("F_RLM"), 20.22194, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CH4") %>% pull("F_LM"), 0.007639924, tolerance=1e-3)
  expect_equal(Flux_a1_1 %>% filter(gas == "CH4") %>% pull("F_RLM"), 0.007568566, tolerance=1e-3)
})
