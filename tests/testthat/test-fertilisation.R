testthat::test_that("fertilisation maps to Tool2 management fields", {
  programme <- spacing_fertilisation(
    at_sowing_kg_n_ha = 50,
    applications = list(
      spacing_fertiliser_application(
        100, trigger = "DaysAfterSowing", days_after_sowing = 35
      )
    )
  )
  x <- as.list(programme)
  testthat::expect_equal(x$nitrogen_mineral_kg_ha, 50)
  testthat::expect_equal(x$split_fertiliser[[1]]$fert_type, "UreaN")
  testthat::expect_equal(x$split_fertiliser[[1]]$days_after_sowing, 35L)
})

testthat::test_that("unsupported fertilisers are rejected", {
  testthat::expect_error(
    spacing_fertiliser_application(50, fertiliser = "Unknown"),
    "Unsupported"
  )
})
