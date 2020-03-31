#' generate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @param db
#' 
#' 
#' @noRd 
#'
#' @import bs4Dash ggplot2 magick shinycssloaders dplyr shinyWidgets ggpubr
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
          tags$br(),
          switchInput(ns("current"), value = TRUE,
                      label = "Choose Date",
                      offLabel = "Last Month", onLabel = "Current"),

          selectInput(ns("rank"), "Select Ranking to Generate: ", choices = c("sales", "equity", "area")),
          actionButton(ns("gen"), "generate", class = "btn-success"),
          downloadButton(ns("downpic"), "Download IMAGE"),
          downloadButton(ns("download"), "Download CSV"),
          downloadButton(ns("downorigin"), "Download Original Data")
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
 
  df <- tibble()
  
  observeEvent(input$sync, {
    
    
  df <<- db$find() %>% mutate_at(vars(-firm), as.numeric) %>% 
     
     mutate(cur_sales = case_when(
                  adj_sale != 0 ~ adj_sale,  
                  sale_firm != 0 ~ sale_firm,
                  TRUE ~ cur_preds),
            cur_eq = if_else(adj_eq == 0, eq_firm, adj_eq),
            cur_area = if_else(adj_area == 0, area_firm, adj_area),
            
            total_sales = case_when(
                    cum_adj_sale != 0 ~ cum_adj_sale,
                    cum_sale != 0 ~ cum_sale,
                    TRUE ~ cur_sales + past_sales),
            total_eq = case_when(
              cum_adj_eq != 0 ~ cum_adj_eq,
              cum_eq != 0 ~ cum_eq,
              cur_eq != 0 ~ past_eq + cur_eq,
              TRUE ~ total_sales * eq_ratio),
            total_area = case_when(
              cum_adj_area != 0 ~ cum_adj_area,
              cum_area != 0 ~ cum_area,
              cur_area != 0 ~ cur_area + past_area,
              TRUE ~  total_sales/price),
            source = case_when(
              (cum_adj_eq + cum_adj_sale + cum_adj_area + adj_eq + adj_sale + adj_area) > 0 ~ "指数调整",
              (cum_eq + cum_sale + cum_area + eq_firm + sale_firm + area_firm) > 0 ~ "公司提供",
              TRUE ~ "指数预测")
     ) %>% 
     select(firm, starts_with("total"), starts_with("cur"), source) %>% 
     left_join(pydf, by = "firm") %>% 
     mutate_at(vars(starts_with("total")), ~ round(.x, 1))
    
    
    sendSweetAlert(
      session = session,
      title = "Success !!",
      text = "All Set!",
      type = "success"
    )
    
  })
    
    
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

    isolate({
      if(input$rank == "sales") {
        
        str(head(df))
        df %>% 
          arrange(desc(total_sales), py) %>% 
          slice(1:100) %>% 
          bind_cols(rank = 1:100) %>% 
          transmute(`排名` = rank, `公司名称` = firm, `累计全口径销售（亿元）` = round(total_sales, 1)) -> tbl
        
        rank_name <- "销售金额"
        
        
      } else if(input$rank == "equity") {
        
        df %>% 
          arrange(desc(total_eq), py) %>% 
          slice(1:100) %>% 
          bind_cols(rank = 1:100) %>% 
          transmute(`排名` = rank, `公司名称` = firm, `累计权益销售金额（亿元）` = round(total_eq, 1)) -> tbl
        
        rank_name <- "权益销售金额"
        
      } else {
        
        df %>% 
          arrange(desc(total_area), py) %>% 
          slice(1:100) %>% 
          bind_cols(rank = 1:100) %>% 
          transmute(`排名` = rank, `公司名称` = firm, `累计销售面积（万平方米）` = round(total_area, 1)) -> tbl
       
        
        rank_name <- "销售面积" 
      }

      
    })
    
    
    
    image_read("./header.jpg") -> header
    footer <- image_read("./footer.jpg")
    
    showtext::showtext.auto()
    showtext::showtext.opts(dpi = 300)
    
    isolate({
      
      if(input$current == TRUE){
        month <- lubridate::month(Sys.Date())
      } else {
        month <- lubridate::month(Sys.Date())-1
      }
      
    })
    
    
    showtext::showtext_auto() 
    tibble(x = 0, y = 0, t = str_glue("1-{month}月中国房地产企业{rank_name}TOP100")) %>% 
      ggplot(aes(x, y)) + 
      geom_text(aes(label = t), col = "#605742") + theme_void() + 
      theme(text = element_text(face = "bold"))
    ggsave("name.png")
    
    image_read("name.png") %>% 
      image_trim() %>% 
      image_transparent(color = "white") %>% 
      image_scale("500x") %>% 
      image_composite(
        header, composite_image = ., offset = "+50+220"
      ) %>% image_write("header.png")
    
    tibble(x = 0, y = 0, t = "统计来源：观点指数") %>% 
      ggplot(aes(x, y)) + 
      geom_text(aes(label = t), col = "#605742") + theme_void()
    ggsave("source.png")
    
    (lubridate::ymd(str_glue("2020-{month + 1}-01"))-1) %>% str_split("-", simplify = TRUE) -> dates
    
    showtext::showtext_auto() 
    tibble(x = 0, y = 0, 
           t = str_glue("统计日期：2020年1月1日-{dates[2] %>% str_remove('^0')}月{dates[3]}日")) %>% 
      ggplot(aes(x, y)) + 
      geom_text(aes(label = t), col = "#605742") + theme_void()
    ggsave("dates.png")
    
    image_read("source.png") %>% 
      image_trim() %>% 
      image_transparent(color = "white") %>% 
      image_scale("x14") %>% 
      image_composite(
        footer, composite_image = ., offset = "+350+55"
      ) -> temp
    
    image_read("dates.png") %>% 
      image_trim() %>% 
      image_transparent(color = "white") %>% 
      image_scale("x14") %>% 
      image_composite(
        temp, composite_image = ., offset = "+350+30"
      ) %>% image_write("footer.png")
    
  
    #Sys.setlocale(category = "LC_ALL", locale = "chs")
    header <- image_read("header.png")
    footer <- image_read("footer.png")
    
    
    showtext::showtext.opts(dpi = 300)
    showtext::showtext_auto()
    ggtexttable(tbl, rows = NULL, 
                        theme = ttheme(base_colour = "gray50",
                          colnames.style = colnames_style(size = 8, color = "white",
                            fill = "#f3a06e", linewidth = 0.3),
                          padding = unit(c(8, 3), "mm"), base_size = 14, 
                          tbody.style = tbody_style(size = 8,
                                              fill = c("#f4d5b1", "#eeeded"),
                                              color = "gray10", face = "plain",
                                              linewidth = 0.3)
                        )) + theme(text = element_text(family = "source-han-sans-cn"))
    # dev.off()
    
    # ggplot(aes(total), data = data_lst$data() ) + geom_histogram()
    ggsave("tbl.png", height = 70, limitsize = FALSE, units = "cm", dpi = 300)
    
    img <- image_trim(image_read("tbl.png"))
    
    mask <- image_read("mask.png")
    img <- image_flatten(c(img, mask), "Multiply")
    
    imgs <- image_append(image_scale(c(header, img, footer), "600x"), stack = TRUE)
    
    image_write(imgs, "imgs.png")
    
    list(
        src = "imgs.png",
        contentType = 'image/png',
        width = 600,
        height = 5400,
        alt = "This is alternate text")


  }, deleteFile = FALSE)
  
  output$downpic <- downloadHandler(
    filename = function() {
      
      "ranking.png"
    },
    content = function(con) {
      
      imgs <- image_read("imgs.png")
      
      image_write(imgs, con)
    }
  )
  
  output$downorigin <- downloadHandler(
    
    filename = function() {
      
      paste0("df", ".csv")
      
    },
    content = function(file) {
      readr::write_excel_csv(db$find(), file)
      
    })
}




    
## To be copied in the UI
# mod_generate_ui("generate_ui_1")
    
## To be copied in the server
# callModule(mod_generate_server, "generate_ui_1")
 
