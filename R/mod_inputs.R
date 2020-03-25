#' inputs UI Function
#'
#' @description input comps
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @param db
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
          switchInput(ns("if_cum"), value = FALSE, onLabel = "Cumulative", offLabel = "Single Month"),
          uiOutput(ns("byfirms")),
          shinyWidgets::materialSwitch(ns("if_adj"), label = "Ajusted",
                                       status = "danger"),
          uiOutput(ns("adj_sales")),
          actionButton(ns("ok"), "OK")
        )
      ),
      col_8(
        actionButton("refresh", "Refresh Table", icon = icon("sync")),
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
mod_inputs_server <- function(input, output, session, db){
  ns <- session$ns
  
  record <- reactiveValues()
  
  output$byfirms <- renderUI({
    
    if(input$if_cum == TRUE){
      
      tagList(
        textInput(ns("cfsales"), "Cum Sales by firm"),
        textInput(ns("cfeq"), "Cum Equity by firm"),
        textInput(ns("cfarea"), "Cum Area by firm")
      )
    } else {
      
      tagList(
        textInput(ns("fsales"), "Sales by firm"),
        textInput(ns("feq"), "Equity by firm"),
        textInput(ns("farea"), "Area by firm")
      )
      
    }
  })
  
  output$adj_sales <- renderUI({
    
    if(input$if_adj == TRUE) {
      
      tagList(
        textInput(ns("gsales"), "Adjusted Sales"),
        textInput(ns("geq"), "Adjusted Equity"),
        textInput(ns("garea"), "Adjusted Area")
      )
    }
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
  
  
  
  output$dt <- renderDT({
    
    input$refresh
    db$find()
  })
  

  # return (list(
  #   updates = reactive({ record }),
  #   data = reactive({ data() })
  # ))

}
    
## To be copied in the UI
# mod_inputs_ui("inputs_ui_1")
    
## To be copied in the server
# callModule(mod_inputs_server, "inputs_ui_1")
 
