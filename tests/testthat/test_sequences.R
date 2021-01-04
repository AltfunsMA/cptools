# R version 3.6.3 (2020-02-29)
# author: Alfonso Martínez Arranz
# 2020-04-14 11:05:26 
# test file for sequences

context("Sequence tests")

# Open a file to send messages to
f <- "messages.Rout"
zz <- file(f, open = "wt")
# Divert messages to that file
sink(zz)

# Load samples
sample_sf <- readRDS("~/Projects/cptools/tests/testthat/sample_sf.rds")

warned <- sample_sf %>% 
  mutate(year = ifelse(AC_NO > 50, "2000", "2005"))

standard <- nrow(is_acno_complete(sample_sf))

newcols <- nrow(is_acno_complete(sample_sf, 
                                 checkCols = c("ST_NAME", "PC_ID")))

sink()
close(zz)
file.remove(f)

test_that("is_acno_complete works on tibble/sf object", {
  expect_equal(standard, 18)
  expect_equal(newcols, NULL)
})


test_that("Year warning appears", {
  expect_warning(is_acno_complete(warned, 
                                  checkCols = c("ST_NAME", "PC_ID")))

})
