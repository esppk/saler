#' initiate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @param db
#' 
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_initiate_ui <- function(id){
  ns <- NS(id)
  bs4TabItem(
    tabName = "file_inputs",
    fluidRow(
      col_4(
        bs4Card(
          width = 12,
          fileInput(ns("file_xl"), "Upload original file"),
          actionButton(ns("ok"), "Overide Database")
        )
      ),
      col_8(
        DT::DTOutput(ns("preview"))
      )
    )
  )
}
    
#' initiate Server Function
#' 
#' @import mongolite
#'
#' @noRd 
mod_initiate_server <- function(input, output, session, db){
  ns <- session$ns
  

  
  data <- reactive({
     
    req(input$file_xl)
    
    ext <- tools::file_ext(input$file_xl$name)
    
    switch(ext,
           
           xlsx = readxl::read_excel(input$file_xl$datapath),
           csv = readr::read_csv(input$file_xl$datapath),
           validate("Invaldi file; please upload either csv or xlsx.")
    )
  })
  
  output$preview <- renderDT({
    
    head(data())
  })
  
  
  observeEvent(input$ok, {
    
    db$drop()
    db$insert(data())
    
    sendSweetAlert(
      session = session,
      title = "Success !!",
      text = "All in order",
      type = "success"
    )
  })
}
    
## To be copied in the UI
# mod_initiate_ui("initiate_ui_1")
    
## To be copied in the server
# callModule(mod_initiate_server, "initiate_ui_1")
 
