#' Insert dashes from cursor position to up to 80 characters
#'
#' @return dashes inside RStudio
#' 
set_new_chapter <- function(){
  # set limit to which position dashes should be included
  nchars <- 80
  
  # grab current document information
  context <- rstudioapi::getActiveDocumentContext()
  # extract horizontal courser position in document
  context_col <- context$selection[[1]]$range$end["column"]
  
  # if a line has less than 81 characters, insert hyphens at the current line
  # up to 80 characters
  if (nchars > context_col) {
    rstudioapi::insertText(strrep("-", nchars - context_col))
  }
}


description_prompt <- function() {
  
m <- "Please provide a description. Enter a space or press cancel to leave blank."  
  
  rstudioapi::showPrompt(title = "Description in header",
                         message = m)
}

generate_header <- function(description) {
  
  elements <- c(R.version.string, 
                "author: Alfonso Martínez Arranz", 
                paste0("last header generated at: ", as.character(Sys.time())),
                description)
  
  elements <- elements[!grepl("^[[:space:]]+$", elements)]
  
  comments <- paste("#", elements, collapse = "\n")
  
  list("comments" = comments, "length" = length(elements))
  
}

create_R_with_header <- function() {

  description <- description_prompt()
  
  header <- generate_header(description)
  
  rstudioapi::documentNew(text = paste(header[["comments"]], "\n\n"), 
                          position = c(header[["length"]] + 2, 1))
                          
}


update_header <- function() {
  
  description <- description_prompt() 
  
  header <- generate_header(description)
  
  rstudioapi::modifyRange(location = rstudioapi::document_range(rstudioapi::document_position(1,1), 
                                                                rstudioapi::document_position(header[["length"]], 80)), 
                         text = header[["comments"]],
                         id = rstudioapi::getSourceEditorContext()$id)
  
}

insert_header <- function() {
# to-do  
  
  description <- description_prompt() 
  
  header <- generate_header(description)
  
  
}
