if (!require(shiny)) install.packages('shiny'); library(shiny)
if (!require(shinyFiles)) install.packages('shinyFiles'); library(shinyFiles)
if (!require(readr)) install.packages('readr'); library(readr)
if (!require(shinydashboard)) install.packages('shinydashboard'); library(shinydashboard)
if (!require(DT)) install.packages('DT'); library(DT)
if (!require(dplyr)) install.packages('dplyr'); library(dplyr)
if (!require(lubridate)) install.packages('lubridate'); library(lubridate)
if (!require(sassr)) install.packages('sassr'); library(sassr)
#if (!require(backports)) install.packages('backports'); library(backports)
#if (!require(shinyjs)) install.packages('shinyjs'); library(shinyjs)
source("utils.R")
source("editTableDT.R") #Inspired from https://github.com/cardiomoon/editData
source("editBills.R")

wdLoadUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 8, verbatimTextOutput(ns("path"), placeholder = TRUE)),
      column(width = 4, shinyDirButton(id = ns("dir"), label = "Browse...", title = "Directory of output file", buttonType = "primary"))
    )
  )
}

wdLoad <- function(input, output, session) {
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

## loadingOptions

loadingOptionsUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
             radioButtons(inputId = ns("sep"), label = "Separation", selected = ";",
                          choices = c(Virgule = ",", 'Point Virgule' = ";", Tabulation = "\t"))
             
      ),
      column(width = 6,
             checkboxInput(inputId = ns("header"), label = "Header ?", value = TRUE)
      )
    )
  )
}

loadingOptions <- function(input, output, session, path, filename, coltypes = cols(.default = col_character())) {
  reactive({
    req(path())
    do.call(read_delim, args = list(file = paste0(path(), "/", filename), col_names = input$header,
                                   col_types = coltypes,
                                   delim = input$sep))
  })
}

## showData

showDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    dataTableOutput(ns("data"))
  )
}

showData <- function(input, output, session, data) {
  output$data <- renderDataTable(data())
}

## addData

# addDataUI <- function(id) {
#   ns <- NS(id)
#   tagList(
#     uiOutput(ns("filldata"))
#   )
# }
# 
# addData <- function(input, output, session, data, ncol = 4) {
#   
#   ns <- session$ns
#   
#   rv <- reactiveValues(tofill = list(), nrow = list(), ids = list())
#   observe({
#     req(data())
#     rv$tofill <- names(data())
#     rv$nrow <- ceiling(length(rv$tofill)/ncol)
#     rv$ids <- paste0("col", 1:length(rv$tofill))
#   })
#   
#   output$filldata <- renderUI({
#     req(data())
#     #ll <- replicate(nrow, fluidRow())
#     ll <- lapply(1:rv$nrow, function(i) {
#       row <- fluidRow()
#       for (j in 1:ncol) {
#         if (j + ncol*(i - 1) <= length(rv$tofill)) {
#           row <- tagAppendChild(row, column(width = floor(12/ncol), textInput(inputId = ns(rv$ids[j + ncol*(i - 1)]), label = rv$tofill[j + ncol*(i - 1)])))
#         }
#       }
#       return(row)
#     })
#     tagList(
#       ll,
#       br(),
#       fluidRow(
#         column(width = 3, actionButton(inputId = ns("adddata"), label = "Save entry"), offset = 7),
#         column(width = 2, actionButton(inputId = ns("cleardata"), label = "Clear page"))
#       )
#     )
#   })
#   
#   eventReactive(input$adddata, {
#     req(data())
#     newd <- as.data.frame(setnames(input[rv$ids], rv$tofill))
#     newd <- rbind(data(), newd)
#   })
#   
#   observeEvent(input$cleardata, {
#     lapply(rv$ids, function(i) {
#       updateTextInput(session, inputId = i, value = "")
#     })
#   })
# }
# 


