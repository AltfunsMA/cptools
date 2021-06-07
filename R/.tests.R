

# To-do: trasnform dataset into more portable one
AC_map1978 <- st_read("../check_data/sample_map_df.gpkg")


sample_sf <- AC_map1978 %>% 
  filter(is.na(ST_NAME) | ST_NAME %in% c("Andhra Pradesh", 
                                         "Gujarat"))

sample_dobles <- read.csv("~/Projects/check_data/sample_map_fixes.csv",
                          col_types = cols()) %>% 
  filter(ST_NAME %in% c("Andhra Pradesh", 
                        "Gujarat"))

saveRDS(sample_sf, file = "tests/testthat/sample_sf.rds")

saveRDS(sample_dobles, file = "tests/testthat/sample_dobles.rds")

odd_val_col(AC_map1978)

pryr::mem_used()

sample_sf %>% 
 filter(!is.na(ST_NAME)) %>% 
odd_val_col("ac")

find_acno_incomplete(sf_for_tests)

#to-do: create tests for sequences
complete <- find_acno_incomplete(AC_map1978)

stopifnot(nrow(complete) == 363)

#to-do: create tests for maps?
AC_map1978 %>% 
  filter(is.na(ST_NAME)) %>% 
  create_india_map()

 
AC_map1978 %>% 
  filter(ST_NAME %in% c("Himachal Pradesh", "Jammu & Kashmir"), 
         dplyr::between(AC_NO, 20, 40)) %>% 
  create_india_map(district_display = "labels", district_label_colour = "blue")

