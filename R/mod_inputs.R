#' inputs UI Function
#'
#' @description input comps
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' 
#'
#' @importFrom shiny NS tagList 
mod_inputs_ui <- function(id){
  ns <- NS(id)
  bs4TabItem(
    tabName = "data_inputs",
    fluidRow(
      col_4(
        bs4Card(
          title = "Input sales updates",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          selectizeInput(ns("firm"), "Select Firm", firms),
          textInput(ns("fsales"), "Sales by firm"),
          textInput(ns("feq"), "Equity by firm"),
          textInput(ns("farea"), "Area by firm"),
          shinyWidgets::materialSwitch(ns("if_adj"), label = "Ajusted",
                                       status = "danger"),
          uiOutput(ns("adj_sales")),
          fileInput(ns("file_xl"), "Upload original file"),
          actionButton(ns("ok"), "OK")
        )
      ),
      col_8(
        DT::DTOutput(ns("dt"))
      )
    )
  )
}
    
#' inputs Server Function
#'
#' @noRd 
#' 
#' @import dplyr shinyWidgets
#' 
#' @importFrom DT renderDT
mod_inputs_server <- function(input, output, session){
  ns <- session$ns
  
  record <- reactiveValues()
  
  output$adj_sales <- renderUI({
    
    if(input$if_adj == TRUE) {
      
      tagList(
        textInput(ns("gsales"), "Sales by firm"),
        textInput(ns("geq"), "Equity by firm"),
        textInput(ns("garea"), "Area by firm")
      )
    }
  })
  
  data <- reactive({
    req(input$file_xl)
    
    ext <- tools::file_ext(input$file_xl$name)
    
    switch(ext,
           
           xlsx = readxl::read_excel(input$file_xl$datapath),
           csv = readr::read_csv(input$file_xl$datapath),
           validate("Invaldi file; please upload either csv or xlsx.")
    )
  })
  
  output$dt <- renderDT({
    
    head(data())
  })
  

    observeEvent(
      input$ok, {
      
      record$sale <- input$fsales
      record$eq <- input$feq
      record$area <- input$farea
      
      sendSweetAlert(
        session = session,
        title = NULL,
        text = tags$span(
          tags$h3("Please Comfirm your inputs",
                  style = "color: steelblue;"),
          tags$br(),
          tags$br(),
          "Sales: ",
          record$sale,
          tags$br(),
          "Equity: ",
          record$eq,
          tags$br(),
          "Area: ",
          record$area,
          tags$br(),
          icon("thumbs-up")
        ),
        html = TRUE
      )
    })
    
  
  return (list(
    updates = reactive({ record }),
    data = reactive({ data() })
  ))

}
    
## To be copied in the UI
# mod_inputs_ui("inputs_ui_1")
    
## To be copied in the server
# callModule(mod_inputs_server, "inputs_ui_1")
 
