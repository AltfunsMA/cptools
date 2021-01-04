# Table functions for report generation in HTML
# R 3.6.1


var2fivenum <- function(var) {
  
  skim_to_wide(var, na.rm = TRUE) %>%
    dplyr::select(mean, sd, p0, p25, p50, p75, p100, hist)
  
}

my_kable <- function(df, unit = "", caption = NULL, col.names = NA) {
  
  if(unit != "") {unit <- paste0(" (*", unit, "*)")}
  
  kable(df, digits = 3, format.args = list(big.mark = ",", drop0trailing = TRUE, scientific = FALSE, linesep = ""), caption = paste(caption, unit), col.names = col.names, align = "c", format = "html", escape = FALSE) %>% 
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE, position = "center", font_size = 11, protect_latex = TRUE)
  
}

resumir_una <- function(var, unit = "", caption = NULL) {
  
  if(is.null(caption)) {caption <- deparse(substitute(var))}
  
  var2fivenum(var) %>% 
    my_kable(caption = caption, unit = unit)
  
}

severalSummary <- function(pattern = "^$", unit = "", caption = NULL, ...) {
  
  x <- enquos(...)
  
  matched_df <- CPACUR %>% dplyr::select(matches(pattern), !!! x)
  
  if(is.null(caption)) {caption <- paste0("Summary statistics for variables with the pattern", pattern)}
  
  cbind("Variable" = colnames(matched_df), 
        map_dfr(matched_df, var2fivenum)) %>% 
    my_kable(caption = caption, unit = unit)
  
}

sample_table <- function(df, caption = "Sample of the original dataset", scroll = "none") {
  
  r <- nrow(df)
  l <- length(df)
  
  df[1:min(20, r), 1:min(20, l)] %>%                           
    my_kable(caption = caption) %>% 
    kable_styling(font_size = 10) %>% 
    {switch(scroll,
            none = {.},
            width = {scroll_box(., width = "400px")},
            height = {scroll_box(., height = "200px")},
            both = {scroll_box(., width = "400px", height = "200px")},
            full = {scroll_box(., width = "100%", height = "200px")}
    )}
  
}

long2wide_kable <- function(df, col.names = NA, caption = NULL) {
  
  df <- df %>% mutate_if(is.factor, as.character)
  
  rep_main_col <- length(df) + 1
  
  if (!is.na(col.names)) {
    col.names <- rep(col.names, 2)
  }
  
  if (nrow(df) %% 2 != 0) { # For odd numbered nrows
    
    # Create empty row for end of second column
    oddrow <- as.character()
    length(oddrow) <- length(df)
    names(oddrow) <- colnames(df)
    oddrow <- replace_na(oddrow, "")
    
    x <- df[1:(ceiling(nrow(df)/2)), ] #
    y <- rbind(df[(ceiling(nrow(df)/2)+1):nrow(df), ],
               oddrow)
    
  }
  
  else { # For even numbered ones
    x <- df[1:round(nrow(df)/2), ]
    y <- df[(round(nrow(df)/2)+1):nrow(df), ]
  }
  
  z <-  cbind(x,y)
  
  my_kable(z, col.names = col.names, caption = caption) %>% 
    kable_styling(font_size = 10) %>% 
    column_spec(c(1, rep_main_col), bold = TRUE)
  
}