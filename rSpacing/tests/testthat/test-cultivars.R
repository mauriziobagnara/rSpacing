testthat::test_that("verified cultivars are accepted", {
  testthat::expect_identical(
    spacing_validate_cultivar("Soybean", "Generic_MG1"),
    "Generic_MG1"
  )
})

testthat::test_that("unknown cultivars fail before APSIM", {
  testthat::expect_error(
    spacing_validate_cultivar("Soybean", "Baxters_Black"),
    "not in the verified"
  )
})
