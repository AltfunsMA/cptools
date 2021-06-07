context("Filtering tests")

# Open a file to send messages to
f <- "messages.Rout"
zz <- file(f, open = "wt")
# Divert messages to that file
sink(zz)

# despite cptools no longer requiring sf package; these tests are based on sf
# objects and many tidyverse internals need new methods supplied by sf to work
# properly. Loading it here is the best workaround I have come up with so far.
library(sf) 

# Load samples
sf_sample <- readRDS("~/Projects/cptools/tests/testthat/sample_sf.rds") %>% 
  dplyr::mutate(ST_NAME = dplyr::case_when(
    ST_NAME == "Andhra Pradesh" & AC_NO < 50 ~ 'Delhi',
    ST_NAME == "Andhra Pradesh" & dplyr::between(AC_NO, 50, 100) ~ 'Daman and Diu',
    ST_NAME == "Andhra Pradesh" & dplyr::between(AC_NO, 100, 150) ~ 'Chandigarh',
    ST_NAME == "Andhra Pradesh" & dplyr::between(AC_NO, 150, 200) ~ 'Manipur',
    TRUE ~ as.character(ST_NAME))
    )

sink()
close(zz)
# file.remove(f)

total_rows <- nrow(sf_sample)

nvals <- sum(is.na(sf_sample$ST_NAME))

n <- total_rows - nvals
                   
test_that("Warnings about NAs appear, and that NAs are indeed removed", {
  expect_warning(cptools:::filter_st_name(sf_sample, "not_in_cpw1"), " contains NA values.")
  expect_equal(suppressWarnings(nrow(cptools:::filter_st_name(sf_sample, "not_in_cpw1"))), n-50)
})
  
sf_sample <- dplyr::filter(sf_sample, !is.na(ST_NAME))

n <- sf_sample %>% 
  dplyr::filter(!is.na(ST_NAME)) %>% 
  nrow()

test_that("Filters and by implication state selection works", {
  expect_equal(nrow(cptools:::filter_st_name(sf_sample, "not_in_cpw1")), n-50)
  expect_equal(nrow(cptools:::filter_st_name(sf_sample, "Gujarat")), (n-178))
  expect_equal(nrow(exclude_states(sf_sample, "no_vidhan")), n-50-51)
  expect_equal(nrow(exclude_states(sf_sample, "not_in_cpw1")), n-50)
  expect_equal(nrow(include_states(sf_sample, "Gujarat")), (178))
  expect_equal(nrow(include_states(sf_sample, "no_vidhan")), 101)
  expect_equal(nrow(filter_cpw1_states(sf_sample)), n-50)
  expect_error(include_states(sf_sample))
  expect_error(exclude_states(sf_sample))
})
