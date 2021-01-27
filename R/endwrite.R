
#' Add the relevant sign at the end of the line and move to the next line
#'
#' @details Wrapper around a function that adds code at the end of the currently 
#' active line and moves the cursor to the next line

endwrite <- function(sign) {
  
  ctx <- rstudioapi::getSourceEditorContext()
  
  current_line <-
    as.numeric(ctx[["selection"]][[1]][["range"]][["start"]][["row"]])
  
  source_id <- ctx$id
  
  end_of_line <- rstudioapi::as.document_position(c(current_line, Inf))
  
  nextline <- rstudioapi::as.document_position(c(current_line + 1, Inf))
  

  rstudioapi::insertText(location = end_of_line, text = sign, id = source_id)
  
  rstudioapi::setCursorPosition(position = nextline, id = source_id)
  
  rstudioapi::executeCommand("reformatCode")

  rstudioapi::insertText("")
  
} 

  


#' Add a magrittr::pipe at the end of the line and move to the next line
#'
#' @inherit endwrite details
#' @export
endpipe <- function() {
  
  endwrite(" %>% \n")
  
}

#' Add a plus sign ('+') at the end of the line and move to the next line (for ggplot)
#' 
#' @inherit endwrite details
#' @export
endplus <- function() {
  
  endwrite(" + \n")
  
}