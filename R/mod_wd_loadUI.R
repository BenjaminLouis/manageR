#' @title mod_wd_loadUI and mod_wd_load
#' @description A shiny module that load and display a path to a working directory
#'
#' @param id shiny id
#' 
#' @importFrom shiny NS tagList fluidRow column verbatimTextOutput
#' @importFrom shinyFiles shinyDirButton
#'
#' @export
#'
#' @examples
#' library(shiny)
#' library(shinyFiles)
#' if (interactive()){
#' ui <- fluidPage(
#'   mod_wd_loadUI("wd")
#' )
#' 
#' server <- function(input, output, session) {
#'   path <- callModule(mod_wd_load,"wd")
#' }
#' 
#' shinyApp(ui, server)
#' }
#' 
mod_wd_loadUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 8, verbatimTextOutput(ns("path"), placeholder = TRUE)),
      column(width = 4, shinyDirButton(id = ns("dir"), label = "Browse...", title = "Directory of output file", buttonType = "primary"))
    )
  )
}



#' mod_wd_load server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @importFrom shiny renderText req reactive
#' @importFrom shinyFiles getVolumes shinyDirChoose parseDirPath
#'
#' @export
#' @rdname mod_wd_loadUI
mod_wd_load <- function(input, output, session) {
  #shinyDirChoose(input, "dir", roots = c(wd = getwd())) 
  volumes <- getVolumes()
  shinyDirChoose(input, "dir", roots = volumes, session = session) #getVolumes()) <-- doesn't work
  output$path <- renderText({
    req(input$dir)
    #parseDirPath(c(wd = getwd()), input$dir)
    parseDirPath(volumes, input$dir)
  })
  reactive({
    req(input$dir)
    #parseDirPath(c(wd = getwd()), input$dir)
    parseDirPath(volumes, input$dir)
  })
}