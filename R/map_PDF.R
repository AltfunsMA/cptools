#' Saving multiple plots into PDF from a list
#'
#' @param plot_list A list of plots handled by gridExtra 
#' @param multiple Should the images be printed in multiple PDF files?
#' @param nrow Number of rows for gridExtra::marrangeGrob
#' @param ncol Number of columns for gridExtra::marrangeGrob
#'
#' @return One or several PDF file with numnber of map per page or one PDF per map 
#' in separate folder
#' @export
#'
#' @examples
map_PDF <- function(plot_list, folder = NULL, multiple = FALSE, nrow = 1, ncol = 1) {
  
  ifelse(!dir.exists("Maps_output"), dir.create("Maps_output"), FALSE)
  
  
  if (multiple) {
    
    for (i in plot_list) {
      
      if(is.null(folder)) folder <-  deparse(substitute(plot_list))
      
      
      title <- as.character(i[["labels"]][["title"]])
      alt_title <- as.character((i[["labels"]][[1]]))
      
      
      if(length(title) < 2) {file_title <- title} 
      else {file_title <- alt_title}
      
      
      filename <-  stringr::str_replace_all(file_title, pattern = " ", 
                                            replacement = "_")
      
      fullpath = paste0(file.path("Maps_output", folder, filename), ".pdf")
      
      cat("Saving output to", fullpath, " \n")
      
      
      ifelse(!dir.exists(file.path("Maps_output/", folder)),
             dir.create(file.path("Maps_output/", folder)), FALSE)
      
      tryCatch(ggplot2::ggsave(filename = fullpath, plot = i),
               error = function(cond) {
                 message(cond)
                 write_lines(cond, str_replace(fullpath, "\\.pdf$", ".txt"))
                 })
      
    }
    
  }
  
  else {
    
    grob_list <- gridExtra::marrangeGrob(plot_list, nrow = nrow, ncol = ncol)
    
    filename <- paste0("Maps_output/", deparse(substitute(plot_list)), ".pdf")
    
    ggplot2::ggsave(filename = filename, grob_list)
    
  }
  
}