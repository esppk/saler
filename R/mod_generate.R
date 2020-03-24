#' generate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @param data_lst
#' 
#' @noRd 
#'
#' @import bs4Dash ggplot2 magick shinycssloaders
#' @importFrom shiny NS tagList 
mod_generate_ui <- function(id){
  ns <- NS(id)
  
  bs4TabItem(
    tabName = "rank_gen",
    h1("hello GEN"),
    actionButton(ns("ok"), "GEN"),
    withSpinner(imageOutput(ns("pic"))),
    textOutput(ns("text"))
  )
}
    
#' generate Server Function
#'
#' @noRd 
mod_generate_server <- function(input, output, session, data_lst){
  ns <- session$ns
 
  output$pic <- renderImage({
    
    req(input$ok)
    
    
    # bigdata <- image_read('https://jeroen.github.io/images/bigdata.jpg')
    
    # gg <- image_graph(width = 400, height = 400, res = 96)
    # # ggplot(aes(total_area), data = df) + geom_histogram()
    header <- image_read("header.png")
    
    # png("tbl.png", width = 500, 2300, res = 100)
    ggpubr::ggtexttable(data_lst$data(), rows = NULL)
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
 
