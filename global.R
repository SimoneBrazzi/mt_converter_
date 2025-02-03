library(tidyverse)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(gt)
library(thematic)
library(scales)
library(writexl)
library(rsconnect)
library(usethis)
library(rmarkdown)
library(knitr)
library(reticulate)
library(xml2)
library(XML)

library(xml2)
library(tidyverse)


convert_tibble_to_xml_mt540 <- function(tbl) {
  # Create root document
  doc <- xml_new_root("Document", 
                      `xmlns` = "urn:iso:std:iso:20022:tech:xsd:sese.023.001.09",
                      `xmlns:xsi` = "http://www.w3.org/2001/XMLSchema-instance")
  
  # Function to add nodes recursively
  add_nodes <- function(path, value) {
    elements <- str_split(path, "\\.", simplify = TRUE)
    
    # Traverse and add nodes using reduce()
    last_node <- reduce(elements, 
                        .init = doc, 
                        .f = ~ {
                          existing <- xml_find_first(.x, .y)
                          if (inherits(existing, "xml_missing")) {
                            xml_add_child(.x, .y)
                          } else {
                            existing
                          }
                        })
    
    # Set text value for the last node
    xml_set_text(last_node, value)
  }
  
  # Apply function for each row using walk2
  walk2(tbl$tree, tbl$value, add_nodes)
  
  return(doc)
}