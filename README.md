# cptools
A package for processing Indian elections and [Consumer Pyramids](https://consumerpyramidsdx.cmie.com/) data. It currently uses [tidyverse](www.tidyverse.org) functions and will soon be updated to take advantage of the ultra-fast yet user-friendly [tidytable](https://github.com/markfairbanks/tidytable/) rendering

## Why use it
cptools provides a few simple convenience functions for operating on dataframes containing Indian administrative divisions:

- `include_states()` and `exclude_states()` automatically find the column storing Indian states and Union Territories and subset the dataframe accordingly
- `rename_states()` ensures States and Union Territories have modern spellings and homogenises the various names of "Delhi" and of "Jammu and Kashmir".
- `duplicheck()` and `find_acno_incomplete()` respectively help check for duplicates and for missing Assembly Constituency numbers. They have more generic equivalents. Duplicheck is very similar to `janitor::get_dupes()`, although I only discovered that later!
- `odd_val_col()` summarises 'odd values' from large dataframes. The default definition of 'odd values' is taken from missing value markers in Consumer Pyramids but can be altered.

This package also contains a few add-ins to facilitate workflow, most notably an improved `View()` function that prevents Rstudio from crashing or slowing down as it sometimes happens when viewing large `sf` dataframes.

cptools is necessary to run the replication materials in [its sister repository](https://github.com/AltfunsMA/india-research-public): 

>Martínez Arranz, A., R. Thomson et al., 2021, "The Uneven Expansion of Electricity Supply in India", Energy Research and Social Science, Volume 78, August 2021, 102126. DOI: https://doi.org/10.1016/j.erss.2021.102126

You may use any code in this repository subject to the MIT license. Please acknowledge it in any written material with a citation to the above article.


## How to use it

Install with: `devtools::install_github("https://github.com/AltfunsMA/cptools")`










