context("Duplication tests")


# Open a file to send messages to
f <- "messages.Rout"
zz <- file(f, open = "wt")
# Divert messages to that file
sink(zz)

# Load samples

# despite cptools no longer requiring sf package; these tests are based on sf
# objects and many tidyverse internals need new methods supplied by sf to work
# properly. Loading it here is the best workaround I have come up with so far.
library(sf) 

path <- "~/Projects/cptools/tests/testthat/"

sample_sf <- readRDS(paste0(path, "sample_sf.rds"))

dobles_in_map <- readRDS(paste0(path,"sample_dobles.rds"))

# Process for testing
all_in_map <- duplicheck(sample_sf)

half <- duplicheck(sample_sf, fromLast = FALSE)

half_fromLast <- duplicheck(sample_sf, fromLast = TRUE)

iris1 <- duplicheck(iris, checkCols = c("Sepal.Length", "Species"))

iris_half <- duplicheck(iris, checkCols = c("Sepal.Length", "Species"), 
           fromLast = TRUE)                

# Hard to make-up a dataset that will serve this purpose
repxy <- sample_sf %>%
  dplyr::mutate_if(is.factor, as.character) %>% 
  dplyr::left_join(dobles_in_map, 
            by = c("ST_NAME", "AC_NO", "DIST_NAME", "PC_NAME")) %>%
  replace_xy() %>%
  dplyr::mutate(ST_NAME = dplyr::case_when(
    is.na(ST_NAME) ~ NEW_ST_NAME,
    ST_NAME != NEW_ST_NAME ~ NEW_ST_NAME,
    TRUE ~ ST_NAME
  )) %>%
  dplyr::select(-NEW_ST_NAME)
                    

sink()
close(zz)
file.remove(f)

test_that("duplicheck works on CP tibble/sf object", {
  expect_equal(nrow(all_in_map), 6)
  expect_equal(nrow(half), 3)
  expect_equal(nrow(half_fromLast), 3)
})

test_that("duplicheck works on standard datasets", {
  expect_equal(nrow(iris1), 186)
  expect_equal(nrow(iris_half), 93)
  })


test_that("replace_xy merges on sf object", {
  expect_equal(length(names(repxy)), 9)
  expect_is(repxy, "sf")
  
})