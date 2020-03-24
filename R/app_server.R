#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  lst <- callModule(mod_inputs_server, "inputs_ui_1")
  
  callModule(mod_generate_server, "generate_ui_1", data_lst = lst)
}
