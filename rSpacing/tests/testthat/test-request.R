test_that("request validation catches invalid coordinates", {
  expect_error(new_spacing_request(95, 9.2, "Maize"), class="spacing_validation_error")
  expect_error(new_spacing_request(45.4, 200, "Maize"), class="spacing_validation_error")
})

test_that("request stores Tool2 arguments", {
  x <- new_spacing_request(45.4, 9.2, "Maize", mock=TRUE)
  expect_s3_class(x, "spacing_request")
  expect_true(x$mock)
})
