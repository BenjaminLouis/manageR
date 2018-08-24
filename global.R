if (!require(shiny)) install.packages('shiny'); library(shiny)
if (!require(shinyjs)) install.packages('shinyjs'); library(shinyjs)
if (!require(shinyFiles)) install.packages('shinyFiles'); library(shinyFiles)
if (!require(tidyverse)) install.packages('tidyverse'); library(tidyverse)
if (!require(DT)) install.packages('DT'); library(DT)
if (!require(data.table)) install.packages('data.table'); library(data.table)
if (!require(shinydashboard)) install.packages('shinydashboard'); library(shinydashboard)

wdLoadUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    shinyDirButton(id = ns("dir"), label = "Browse...", title = "Choose a working directory", buttonType = "primary"),     
    verbatimTextOutput(ns("path"), placeholder = TRUE)
  )
}

wdLoad <- function(input, output, session) {
  shinyDirChoose(input, "dir", roots = c(wd = getwd()))
  output$path <- renderText({
    parseDirPath(c(wd = getwd()), input$dir)
  })
  reactive({parseDirPath(c(wd = getwd()), input$dir)})
}
