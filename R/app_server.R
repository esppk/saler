#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  db <- mongo(collection = "saler", db = "saler", 
              url = "mongodb+srv://saler:5al3r@cluster-opuen.mongodb.net/test?retryWrites=true&w=majority", 
              verbose = FALSE)

  callModule(mod_initiate_server, "initiate_ui_1", db = db)
  callModule(mod_inputs_server, "inputs_ui_1", db = db)
  callModule(mod_generate_server, "generate_ui_1", db = db)
}
