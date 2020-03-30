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
        actionButton(ns("refresh"), "Refresh Table", icon = icon("sync")),
        DT::DTOutput(ns("dt"))
      )
    )
  )
}
    
#' inputs Server Function
#'
#' @noRd 
#' 
#' @import dplyr shinyWidgets stringr
#' 
#' @importFrom DT renderDT
mod_inputs_server <- function(input, output, session, db){
  ns <- session$ns
  
  record <- reactiveValues()
  
  output$byfirms <- renderUI({
    
    if(input$if_cum == TRUE){
      
      tagList(
        textInput(ns("cfsales"), "Cum Sales by firm", value = 0),
        textInput(ns("cfeq"), "Cum Equity by firm", value = 0),
        textInput(ns("cfarea"), "Cum Area by firm", value = 0)
      )
    } else {
      
      tagList(
        textInput(ns("fsales"), "Sales by firm", value = 0),
        textInput(ns("feq"), "Equity by firm", value = 0),
        textInput(ns("farea"), "Area by firm", value = 0)
      )
      
    }
  })
  
  output$adj_sales <- renderUI({
    
    if(input$if_adj == TRUE) {
      
      tagList(
        switchInput(ns("adj_cum"), value = FALSE, onLabel = "Cumulative", offLabel = "Single Month"),
        textInput(ns("gsales"), "Adjusted Sales", value = 0),
        textInput(ns("geq"), "Adjusted Equity", value = 0),
        textInput(ns("garea"), "Adjusted Area", value = 0)
      )
    }
  })
  


  observeEvent(
    input$ok, {
    
    
    
    filter_firm <- str_interp('{"firm":"${input$firm}"}')
    
    if(input$if_cum != TRUE) {
      
      record$sale <- input$fsales
      record$eq <- input$feq
      record$area <- input$farea
      
      update_sale <- str_interp('{"$set":{"sale_firm": ${input$fsales}}}')
      db$update(filter_firm, update_sale)
      
      update_eq <- str_interp('{"$set":{"eq_firm": ${input$feq}}}')
      db$update(filter_firm, update_eq)
      
      update_area <- str_interp('{"$set":{"area_firm": ${input$farea}}}')
      db$update(filter_firm, update_area)
    
    } else {
      
      record$sale <- input$cfsales
      record$eq <- input$cfeq
      record$area <- input$cfarea
      
      
      update_sale <- str_interp('{"$set":{"cum_sale": ${input$cfsales}}}')
      db$update(filter_firm, update_sale)
      
      update_eq <- str_interp('{"$set":{"cum_eq": ${input$cfeq}}}')
      db$update(filter_firm, update_eq)
      
      update_area <- str_interp('{"$set":{"cum_area": ${input$cfarea}}}')
      db$update(filter_firm, update_area)
    }
    
    
    if (input$if_adj){
      
      if (input$adj_cum){
        
        update_sale <- str_interp('{"$set":{"cum_adj_sale": ${input$gsales}}}')
        db$update(filter_firm, update_sale)
        
        update_eq <- str_interp('{"$set":{"cum_adj_eq": ${input$geq}}}')
        db$update(filter_firm, update_eq)
        
        update_area <- str_interp('{"$set":{"cum_adj_area": ${input$garea}}}')
        db$update(filter_firm, update_area)
      } else {
        
        update_sale <- str_interp('{"$set":{"adj_sale": ${input$gsales}}}')
        db$update(filter_firm, update_sale)
        
        update_eq <- str_interp('{"$set":{"adj_eq": ${input$geq}}}')
        db$update(filter_firm, update_eq)
        
        update_area <- str_interp('{"$set":{"adj_area": ${input$garea}}}')
        db$update(filter_firm, update_area)
      }
      
    }
    
    
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
    # updateTextInput(session, "feq", value = "")
    lapply(list("fsales", "feq", "farea", "cfsales", "cfeq", "cfarea"), 
           function(.x) updateTextInput(session, .x, value = 0))
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
 
