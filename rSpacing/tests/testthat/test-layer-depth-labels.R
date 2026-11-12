test_that("dynamic layer labels replace static dictionary depths", {
  source_text <- paste(readLines(system.file("R", "variable-dictionary.R", package = "rSpacing"), warn = FALSE), collapse = "\n")
  skip_if(!nzchar(source_text), "Installed source file is unavailable")
  expect_match(source_text, "description_base <- gsub", fixed = TRUE)
  expect_match(source_text, "title_base <- gsub", fixed = TRUE)
  expect_false(grepl("Profondità effettiva APSIMx", source_text, fixed = TRUE))
})
