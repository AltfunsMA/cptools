suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(cptools))
sample_sf <- readRDS("sample_sf.rds")

test_check("cptools")
