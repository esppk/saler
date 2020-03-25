#' generate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @param db
#' 
#' @noRd 
#'
#' @import bs4Dash ggplot2 magick shinycssloaders dplyr
#' @importFrom shiny NS tagList 
mod_generate_ui <- function(id){
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "rank_gen",
    fluidRow(
      col_6(
        bs4Card(
          title = NULL,
          width = 12,
          solidHeader = TRUE,
          actionButton(ns("sync"), "Sync DB and Calculte!", icon = icon("sync-alt")),
          tags$br(),
          selectInput(ns("rank"), "Select Ranking to Generate: ", choices = c("sales", "equity", "area")),
          actionButton(ns("gen"), "generate", class = "btn-success"),
          downloadButton(ns("download"), "Download CSV")
        )
      ),
      col_6(
        withSpinner(imageOutput(ns("pic")))
      )
    )
  )
}
    
#' generate Server Function
#'
#' @noRd 
mod_generate_server <- function(input, output, session, db){
  ns <- session$ns
 
  
  observeEvent(input$sync,
               
     df <<- db$find() %>% 
       mutate(cur_sales = if_else(sale_firm == 0, cur_preds, sale_firm),
              cur_eq = eq_firm,
              cur_area = area_firm,
              total_sales = if_else(cum_sale == 0, cur_sales + past_sales, cum_sale),
              total_eq = case_when(
                cum_eq != 0 ~ cum_eq,
                cur_eq != 0 ~ past_eq + cur_eq,
                TRUE ~ total_sales * eq_ratio),
              total_area = case_when(
                cum_area != 0 ~ cum_area,
                cur_area != 0 ~ cur_area + past_area,
                TRUE ~  total_sales/price)
       ) %>% 
       select(firm, starts_with("total"), starts_with("cur"))
  )
    
    
  output$download <- downloadHandler(
    filename = function() {
      
      paste0("df", ".csv")
      
    },
    content = function(file) {
      readr::write_excel_csv(df, file)
  })

  
  output$pic <- renderImage({
    
    req(input$gen) 
    # bigdata <- image_read('https://jeroen.github.io/images/bigdata.jpg')
    
    # gg <- image_graph(width = 400, height = 400, res = 96)
    # # ggplot(aes(total_area), data = df) + geom_histogram()
    header <- image_read("header.png")
    

    isolate({
      if(input$rank == "sales") {
        
        df %>% 
          select(firm, total_sales) %>% 
          arrange(desc(total_sales)) %>% 
          slice(1:100) %>% 
          bind_cols(rank = 1:100) %>% 
          transmute(`排名` = rank, `公司名称` = firm, `全口径销售` = total_sales) -> tbl
        
        
      } else if(input$rank == "equity") {
        
        df %>% 
          select(firm, total_eq) %>% 
          arrange(desc(total_eq)) %>% 
          slice(1:100) %>% 
          bind_cols(rank = 1:100) %>% 
          transmute(`排名` = rank, `公司名称` = firm, `总权益销售` = total_eq) -> tbl
        
        
      } else {
        
        df %>% 
          select(firm, total_area) %>% 
          arrange(desc(total_area)) %>% 
          slice(1:100) %>% 
          bind_cols(rank = 1:100) %>% 
          transmute(`排名` = rank, `公司名称` = firm, `总销售面积` = total_area) -> tbl
        
      }
      
      
    })
    
    
    
    Sys.setlocale(category = "LC_ALL", locale = "chs")
    # png("tbl.png", width = 500, 2300, res = 100)
    ggpubr::ggtexttable(tbl, rows = NULL)
    # dev.off()
    
    
    # ggplot(aes(total), data = data_lst$data() ) + geom_histogram()
    ggsave("tbl.png", height = 70, limitsize = FALSE, units = "cm")
    
    img <- image_trim(image_read("tbl.png"))
    
    
    imgs <- image_append(image_scale(c(header, img), "400x"), stack = TRUE)
    
    image_write(imgs, "imgs.png")
    
    list(
        src = "imgs.png",
        contentType = 'image/png',
        width = 400,
        height = 1400,
        alt = "This is alternate text")


  }, deleteFile = TRUE)
  
}
    
## To be copied in the UI
# mod_generate_ui("generate_ui_1")
    
## To be copied in the server
# callModule(mod_generate_server, "generate_ui_1")
 
