test_that("real Tool2 mock run succeeds", {
  skip_if(Sys.getenv("RSPACING_INTEGRATION") != "true")
  repository <- Sys.getenv("RSPACING_REPOSITORY")
  skip_if(repository == "")
  cfg <- spacing_config(repository)
  req <- new_spacing_request(45.4, 9.2, "Maize", mock = TRUE,
                             scenario_id = "ci-mock")
  run <- run_spacing(cfg, req, echo = FALSE)
  expect_equal(run$status, 0L)
  expect_true(length(run$outputs$apsimx) >= 1L)
  expect_true(length(run$outputs$database) >= 1L)
})
