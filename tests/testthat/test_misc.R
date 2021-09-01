# testing the contents of the misc.R file

# Open a file to send messages to
f <- "messages.Rout"
zz <- file(f, open = "wt")
# Divert messages to that file
sink(zz)


sf_sample <- readRDS("~/Projects/cptools/tests/testthat/sample_sf.rds") %>% 
  dplyr::mutate(ST_NAME = ifelse(is.na(ST_NAME), "Orissa", ST_NAME)) %>% 
  rm_list_cols()


sf_sample2 <- sf_sample

sf_sample3 <- merge(sf_sample, sf_sample2, by = "ST_NAME")


sample_string <- c("one","two")

sink()
close(zz)
file.remove(f)


test_that(".x and .y suffixes are coalesced", {
  expect_equal(names(replace_xy(sf_sample3)), names(sf_sample))
})

test_that("bounding regexes works under various assumptions", {
  expect_error(bound_rx(c(TRUE, FALSE, TRUE)), "Cannot build regex")
  expect_error(bound_rx(NA), "Cannot build regex")
  expect_equal(bound_rx(sample_string), "(\\bone\\b|\\btwo\\b)")
  expect_equal(bound_rx(sample_string, "", ""), "(one|two)")
  expect_warning(bound_rx("(?<something)tosearch"), "lookarounds")

})

test_that("split_into_chunks yields the correct number of outputs", {
  expect_equal(length(split_into_chunks(mtcars, 10)), 4)
  expect_equal(length(split_into_chunks(iris$Sepal.Length, 3)), 50)
})


# TODO: test the rename states and other functions


