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
          actionButton(ns("append"), "Append Database"),
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
#' @import mongolite dplyr
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
  
  observeEvent(input$append, {
   
    tryCatch({
      data() %>% 
        mutate_at(vars(-firm), as.numeric) %>% 
        mutate_at(vars(-firm), ~ if_else(is.na(.x), 0, .x)) %>% 
        db$insert(.)
      
      sendSweetAlert(
        session = session,
        title = "Success !!",
        text = "All in order",
        type = "success")
      
    },
    error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Error !!",
        text = "Error parsing the colnames; Please make sure your have column `firm`",
        type = "error"
      )
    })
  })
  
  observeEvent(input$ok, {
    
    db$drop()
    
    tryCatch({
      data() %>% 
        mutate_at(vars(-firm), as.numeric) %>% 
        mutate_at(vars(-firm), ~ if_else(is.na(.x), 0, .x)) %>% 
        db$insert(.)
      
      sendSweetAlert(
        session = session,
        title = "Success !!",
        text = "All in order",
        type = "success")
      
    },
    error = function(e) {
      sendSweetAlert(
        session = session,
        title = "Error !!",
        text = "Error parsing the colnames; Please make sure your have column `firm`",
        type = "error"
      )
    }) 
    

  })
}
    
## To be copied in the UI
# mod_initiate_ui("initiate_ui_1")
    
## To be copied in the server
# callModule(mod_initiate_server, "initiate_ui_1")
 
