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

# despite cptools no longer requiring sf package; these tests are based on sf
# objects and many tidyverse internals need new methods supplied by sf to work
# properly. Loading it here is the best workaround I have come up with so far.
library(sf) 

# Load samples
sample_sf <- readRDS("~/Projects/cptools/tests/testthat/sample_sf.rds") %>% 
  filter(!is.na(ST_NAME))  # These NAs should yield a warning that is tested in filtering...

sample_df <- sample_sf %>% 
  st_set_geometry(NULL) %>%
  rename(group_name = ST_NAME, broken_seq = AC_NO, non_broken_seq = PC_ID)
  

warned <- sample_sf %>% 
  mutate(year = ifelse(AC_NO > 50, "2000", "2005"))

standard <- nrow(find_acno_incomplete(sample_sf))

newcols <- nrow(find_acno_incomplete(sample_sf, 
                                 checkCols = c("ST_NAME", "PC_ID")))

seqstandard <- nrow(find_seq_incomplete(sample_df,
                    checkCols = c("group_name", "broken_seq")))

seqsafe <- nrow(find_seq_incomplete(sample_df,
                    checkCols = c("group_name", "non_broken_seq")))

sink()
close(zz)
file.remove(f)

test_that("Missing argument error shows", {
  expect_error(find_seq_incomplete(standard))
  
})


test_that("find_acno_incomplete works on tibble/sf object", {
  expect_equal(standard, 18)
  expect_equal(newcols, NULL)
})



test_that("find_seq_incomplete works on a normal tibble",
          {
    expect_equal(seqstandard, 18)            
    expect_equal(seqsafe, NULL)        
          })


test_that("Year warning appears", {
  expect_warning(find_acno_incomplete(warned, 
                                  checkCols = c("ST_NAME", "PC_ID")))

})
