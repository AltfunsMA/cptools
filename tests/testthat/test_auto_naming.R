context("Auto-naming tests")


solar <- c("photovoltaic", "solar cell")
wind <- c("wind turbine", "wind farm")

nm <- c("solar", "wind")

named_list <- lnm(solar, wind)
named_vector <- cnm("solar", "wind")

test_that("lists and vectors are named with their constituting elements", {
  expect_equal(names(named_list), nm)
  expect_equal(names(named_vector), nm)
  expect_is(named_vector, "character")
  expect_is(named_list, "list")
})

