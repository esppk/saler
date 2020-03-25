#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny bs4Dash 
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    bs4DashPage(
      title = "Sales consolidation",
      navbar = bs4DashNavbar(),
      sidebar = bs4DashSidebar(
        inputId = "nav_sidebar",
        bs4SidebarMenu(
          bs4SidebarHeader("Progress"),
          bs4SidebarMenuSubItem(
            "Init File", 
            tabName = "file_inputs", 
            icon = "heart"),
          bs4SidebarMenuSubItem(
              "inputs_menu", 
              tabName = "data_inputs", 
              icon = "circle-thin"),
          bs4SidebarMenuSubItem(
              "Generate Ranking", 
              tabName = "rank_gen", 
              icon = "beer")
          )
      ),
      body = bs4DashBody(
        bs4TabItems(
          mod_initiate_ui("initiate_ui_1"),
          mod_inputs_ui("inputs_ui_1"),
          mod_generate_ui("generate_ui_1")
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'saler'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

