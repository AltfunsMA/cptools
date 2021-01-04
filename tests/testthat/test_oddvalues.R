context("Odd value identification tests")


sf_sample <- readRDS("~/Projects/cptools/tests/testthat/sample_sf.rds") %>% 
  mutate(ST_NAME = ifelse(is.na(ST_NAME), "Madhya Pradesh", ST_NAME))

# Open a file to send messages to
f <- "messages.Rout"
zz <- file(f, open = "wt")
# Divert messages to that file
sink(zz)

dist_refs <- odd_val_col(sf_sample, "dist")
ac_refs <- odd_val_col(sf_sample, "ac")


x <- map_dfc(mtcars, ~recode(.x, c(`0` = -4)))
neg <- odd_val_col(x, oddValues = "negative")


sink()
close(zz)
file.remove(f)


test_that("Returns correct number of values", {
  expect_equal(nrow(dist_refs), 5)
  expect_equal(nrow(ac_refs), 20)
  expect_equal(nrow(neg), 18)
  
})

test_that("Returns error messages", {
    expect_error(odd_val_col(NA), "This function checks")
})