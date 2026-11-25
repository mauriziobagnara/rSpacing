test_that("coordinates are extracted from requests, runs, and results", {
  req <- new_spacing_request(42.71924, 12.11249, "Maize", mock = TRUE,
                             scenario_id = "orvieto")
  a <- spacing_coordinates(req)
  expect_equal(a$latitude, 42.71924)
  expect_equal(a$longitude, 12.11249)
  expect_equal(a$cell_id, "CELL_427_121")

  run <- structure(list(request = unclass(req), run_id = "run-1", status = 0L),
                   class = "spacing_run")
  b <- spacing_coordinates(run)
  expect_equal(b$status, 0L)

  result <- structure(list(run = run), class = "spacing_result")
  c <- spacing_coordinates(result)
  expect_equal(c$scenario_id, "orvieto")
})

test_that("location data frames and lists are supported", {
  d <- data.frame(site = c("A", "B"), lat = c(45.4, 42.7), lon = c(9.2, 12.1),
                  rotation = c("Maize", "Maize"))
  x <- spacing_coordinates(d)
  expect_equal(nrow(x), 2L)
  expect_equal(x$label, c("A", "B"))

  req1 <- new_spacing_request(45.4, 9.2, "Maize", mock = TRUE, scenario_id = "milan")
  req2 <- new_spacing_request(42.7, 12.1, "Maize", mock = TRUE, scenario_id = "orvieto")
  y <- spacing_coordinates(list(Milan = req1, Orvieto = req2))
  expect_equal(y$label, c("Milan", "Orvieto"))
})

test_that("static maps are ggplot objects", {
  req <- new_spacing_request(42.71924, 12.11249, "Maize", mock = TRUE,
                             scenario_id = "orvieto")
  p <- plot_spacing_location(req, type = "static", basemap = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("invalid coordinates fail clearly", {
  expect_error(
    spacing_coordinates(data.frame(latitude = 200, longitude = 12)),
    class = "spacing_location_error"
  )
})

test_that("coordinates of a scenario and of a scenario run", {
  sc <- new_spacing_scenario("loc", list(spacing_crop("Maize", "B_130", c("1-apr", "10-may"), 7.5, 50, 750)),
                             latitude = 44.9, longitude = 10.63, start_year = 2020, end_year = 2021)
  xy <- spacing_coordinates(sc)
  expect_equal(c(xy$latitude, xy$longitude), c(44.9, 10.63))
  expect_equal(xy$rotation, "Maize")
  run <- structure(list(request = list(scenario = "loc", request_file = "x.json"), status = 0L,
                        run_id = "r1", result = list(inputs = list(latitude = 44.9, longitude = 10.63,
                          rotation = list(Crops = data.frame(CropName = c("Soybean", "Wheat")))))),
                   class = "spacing_run")
  xy <- spacing_coordinates(run)
  expect_equal(xy$scenario_id, "loc")
  expect_equal(xy$rotation, "Soybean-Wheat")
  expect_equal(xy$latitude, 44.9)
})
