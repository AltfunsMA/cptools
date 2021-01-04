context("Save and read file tests")

f <- "messages.Rout"
zz <- file(f, open = "wt")
# Divert messages to that file
sink(zz)

wcsv(mtcars)

rcsv(mtcars)

wcsv(mtcars, "Other/")


sink()
close(zz)
file.remove(f)

test_that("CSV files exist", {
          expect_is(mtcars, "data.frame")
          expect_true(dir.exists("Processed/Other"))
          expect_message(rcsv(mtcars), "More than")
          expect_message(rcsv(no_file), "Object not")
          }
          )


unlink("Processed", recursive = TRUE)

test_that("Files have been removed", {
          expect_false(dir.exists("Processed"))
  }
          )
