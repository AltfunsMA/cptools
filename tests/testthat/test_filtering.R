context("Filtering tests")

# Open a file to send messages to
f <- "messages.Rout"
zz <- file(f, open = "wt")
# Divert messages to that file
sink(zz)

# Load samples
sf_sample <- readRDS("~/Projects/cptools/tests/testthat/sample_sf.rds") %>% 
  mutate(ST_NAME = case_when(
    ST_NAME == "Andhra Pradesh" & AC_NO < 50 ~ 'Delhi',
    ST_NAME == "Andhra Pradesh" & between(AC_NO, 50, 100) ~ 'Daman and Diu',
    ST_NAME == "Andhra Pradesh" & between(AC_NO, 100, 150) ~ 'Chandigarh',
    ST_NAME == "Andhra Pradesh" & between(AC_NO, 150, 200) ~ 'Manipur',
    TRUE ~ as.character(ST_NAME))
    )

sink()
close(zz)
file.remove(f)

total <- nrow(sf_sample)
nas_vals <- sum(is.na(sf_sample$ST_NAME))
n <- total - nas_vals

sf_sample %>% rm_list_cols() %>% count(ST_NAME)

nrow(filter_st_name(sf_sample, "not_in_cpw1"))

test_that("Filters and by implication state selection works", {
  expect_equal(nrow(filter_st_name(sf_sample, "not_in_cpw1")), n-50)
  expect_equal(nrow(filter_st_name(sf_sample, "Gujarat")), (n-178))
  expect_equal(nrow(exclude_states(sf_sample, "no_vidhan")), n-50-51)
  expect_equal(nrow(exclude_states(sf_sample, "not_in_cpw1")), n-50)
  expect_equal(nrow(include_states(sf_sample, "Gujarat")), (178))
  expect_equal(nrow(include_states(sf_sample, "no_vidhan")), 101)
  expect_equal(nrow(filter_cpw1_states(sf_sample)), n-50)
  expect_warning(filter_st_name(sf_sample))
})