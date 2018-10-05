#' @title mod_edit_billsUI and mod_edit_bills
#' @description A shiny module to manage quote and bill data. This shiny module is 
#' quiet big and in the future it should be divided in smaller module
#'
#' @param id shiny id
#' @param docmode either \code{"quote"} to mange quote document or \code{"bill"} (default) to manage bill document
#'
#' @importFrom shiny NS actionButton icon br
#' @importFrom DT dataTableOutput
#' 
#' @export
#'
#' @examples
#' ## No example yet
mod_edit_billsUI <- function(id, docmode = "bill") {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      inline(actionButton(ns("add"), paste("New", docmode), icon = icon("plus", lib = "glyphicon")), va = "middle"),
      inline(actionButton(ns("delRow"), paste("Delete", docmode), icon = icon("remove", lib = "glyphicon")), va = "middle"),
      inline(actionButton(ns("edit"), "Edit status", icon = icon("wrench", lib = "glyphicon")), va = "middle"),
      inline(actionButton(ns("savedata"), label = "Save change", icon = icon("floppy-save", lib = "glyphicon")), va = "middle"),
      inline(actionButton(ns("downloadpdf"), label = "Download PDF", icon = icon("download-alt", lib = "glyphicon")), va = "middle")
    ),
    br(),
    dataTableOutput(ns("origTable"))
  )
}


#' mod_edit_bills server function
#'
#' @param input internal
#' @param output internal
#' @param session internal
#' @param data reactive data with bill or quote information
#' @param servicesdata reactive data with services information
#' @param clientsdata reactive data with clients information
#' @param quotesdata only if \code{mode = "bill"}. Reactive data with quotes information
#' @param billingaddressesdata reactive data with billind addresses information
#' @param path reactive string value. The path to the directory where the file will be exported
#' @param filename string value. The name of the file when exported
#' @param mode either \code{"quote"} to mange quote document or \code{"bill"} (default) to manage bill document
#' @param settingsdata reactive data with settings information
#' 
#' @importFrom shiny reactiveValues reactive observeEvent showModal modalDialog modalButton uiOutput tagList actionButton icon renderUI numericInput hr selectInput dateInput checkboxInput textInput h4 updateNumericInput updateSelectInput updateDateInput updateCheckboxInput updateTextInput radioButtons removeModal selectizeInput textAreaInput observe updateSelectizeInput strong textOutput isolate h5 updateTextAreaInput
#' @importFrom dplyr filter pull bind_cols bind_rows select mutate
#' @importFrom DT renderDataTable datatable
#' @importFrom lubridate ymd
#' @importFrom readr write_delim
#' @importFrom rmarkdown render
#' @importFrom sass sass sass_import
#' @importFrom shiny tableOutput renderTable
#' @importFrom shinyFiles shinyDirButton getVolumes shinyDirChoose parseDirPath
#' @importFrom shinyjs hide
#' @importFrom tibble tibble as_tibble add_row
#' @importFrom xml2 as_list
#' @importFrom purrr map
#' 
#' @export
#' @rdname mod_edit_billsUI
mod_edit_bills <- function(input, output, session, data = reactive(NULL), servicesdata = reactive(NULL), clientsdata = reactive(NULL), quotesdata = NULL, billingaddressesdata = NULL, path, filename, mode = "bill", settingsdata = reactive(NULL)) {
  
  #####################################################
  ##############  INITIALISATION   ####################
  #####################################################
  
  rv <- reactiveValues(no = 1, update = 0)#, newdf = newdf)
  newdf <- NA # pour éviter un erreur avec l'utilisation de '<<-' qui est nécessaire
  newservices <- NA # pour éviter un erreur avec l'utilisation de '<<-' qui est nécessaire
  df <- reactive({
    if (rv$update != 0) {
      df <- newdf
    }
    else {
      df <- data()
    }
    df
  })
  dfserv <- reactive({
    if (rv$update != 0) {
      dfserv <- newservices
    }
    else {
      dfserv <- servicesdata()
    }
    dfserv
  })
  observeEvent(data(), {
     rv$update <- 0
     # Pour stocker les presta dans un tableau au fur et à mesure qu'on en ajoute
     rv$datapresta <- tibble(
       Designation = vector(mode = "character"),
       Quantity = vector(mode = "double"),
       Unit = vector(mode = "character"),
       Unit_price = vector(mode = "double")
     )
  })
  
  #################################################
  ##############  ADD BUTTON   ####################
  #################################################
  
  # Add New button
  # --------------
  observeEvent(input$add, {
    addData()
  })
  
  # Modal dialog to add data
  # -------------------------
  addData <- reactive({
    input$add
    ns <- session$ns
    if (mode == "quote") {
      showModal(
        modalDialog(
          title = "New quote", 
          uiOutput(ns("displayaddquote")),
          footer = tagList(
            actionButton(ns("reset_page"),"Reset", icon = icon("refresh", lib = "glyphicon")),
            actionButton(ns("update"), "Add new", icon = icon("ok", lib = "glyphicon")),
            modalButton("Close", icon = icon("eject", lib = "glyphicon"))
          ), 
          easyClose = TRUE, 
          size = "l"
        )
      )
    }
    if (mode == "bill") {
      showModal(
        modalDialog(
          title = "New bill",
          uiOutput(ns("whichdisplayaddbill")),
          footer = tagList(
            modalButton("Close", icon = icon("eject", lib = "glyphicon"))
          ), 
          easyClose = TRUE, 
          size = "l"
        )
      )
    }
  })
  
  # UI TO add QUOTE data
  # ---------------------
  output$displayaddquote <- renderUI({
    ns <- session$ns
    mydf <- df()
    incr <- nrow(mydf) + 1
    mylist <- list()
    mylist[[1]] <- fluidRow(column(width = 6,
                                   textInput(ns("id_est"), label = "ID_Quote", value = paste0("D", format(Sys.Date(), "%y%m"), "-", paste0(rep(0, 4 - nchar(incr)), collapse = ""), incr))),
                            column(width = 6,
                                   dateInput(inputId = ns("date"), label = "Date", format = "dd-mm-yyyy"))
    )
    mylist[[2]] <- h4("Client selection")
    mylist[[3]] <- fluidRow(column(width = 6,
                                   selectizeInput(inputId = ns("dclient"), label = "ID Client", choices = c("", clientsdata()$ID_Client)),
                                   selectizeInput(inputId = ns("dclient2"), label = "Name", choices = c("", paste(clientsdata()$Firstname, clientsdata()$Name, sep = " ")))
    ),
    column(width = 6,
           br(),
           verbatimTextOutput(ns("client_info"))
    )
    )
    mylist[[4]] <- h4("Description des prestations")
    mylist[[5]] <- tableOutput(ns("allservice"))
    mylist[[6]] <- br()
    mylist[[7]] <- fluidRow(
      column(width = 6, textAreaInput(inputId = ns("design"), label = "Designation", resize = "none", value = "")),
      column(width = 2, numericInput(inputId = ns("qtity"), label = "Quantity", value = 0)),
      column(width = 2, textInput(inputId = ns("unit"), label = "Unit", value = "")),
      column(width = 2, numericInput(inputId = ns("price"), label = "Price", value = 0))
    )
    mylist[[8]] <- actionButton(inputId = ns("add_service"), label = "Add service")
    mylist[[9]] <- br()
    mylist[[10]] <- numericInput(inputId = ns("discount"), label = "Discount (%)", value = 0, min = 0, max = 100, step = 1)
    do.call(tagList, mylist)
  })
  ##### A COMMENTER ######
  observe({
    req(input$dclient2)
    dd <- clientsdata()[paste(clientsdata()$Firstname, clientsdata()$Name, sep = " ") == input$dclient2,]
    updateSelectizeInput(session, 'dclient', choices = c("", clientsdata()$ID_Client), selected = dd$ID_Client)
  })
  observe({
    req(input$dclient)
    dd2 <- clientsdata()[clientsdata()$ID_Client == input$dclient,]
    updateSelectizeInput(session, 'dclient2', choices = c("", paste(clientsdata()$Firstname, clientsdata()$Name, sep = " ")), selected = paste(dd2$Firstname, dd2$Name, sep = " "))
  })
  # On montre le résultat d'affichage de l'adresse du client sur le devis
  output$client_info <- renderText({
    with(clientsdata()[clientsdata()$ID_Client == input$dclient, ],
         paste0(Firstname, " ", Name, "\n",
                ifelse(!is.na(Company) | !is.na(Department), paste0(Company, " ", Department, "\n"), ""),
                ifelse(!is.na(Address1), Address1, ""), "\n",
                ifelse(!is.na(Address2), paste0(Address2, "\n"), ""),
                ifelse(!is.na(Postal_code), Postal_code, ""), " ", ifelse(!is.na(City), City, ""), "\n",
                ifelse(!is.na(Office_line), paste0(Office_line, "\n"), ""),
                ifelse(!is.na(e_mail), paste0(e_mail, "\n"), "")))
  })
  # DEFINITION PRESTATIONS
  dttemp <- reactive({    
    tibble(
      Designation = input$design,
      Quantity = input$qtity,
      Unit = input$unit,
      Unit_price = input$price
    )})
  # Action du bouton pour ajouter des lignes de presta
  observeEvent(input$add_service, {
    rv$datapresta <- rbind(rv$datapresta, dttemp())
    output$allservice <- renderTable(rv$datapresta)
    #show("allservice")
    #rv$datapresta[,'Quantity'] <- gsub("[.]", ",", as.character(unlist(rv$datapresta[,'Quantity'])))
    #rv$datapresta[,'Unit_price'] <- gsub("[.]", ",", as.character(unlist(rv$datapresta[,'Unit_price'])))
    updateTextAreaInput(session, "design", value = "")
    updateNumericInput(session, "qtity", value = 0)
    updateTextInput(session, "unit", value = "")
    updateNumericInput(session, "price", value = 0)
  })
  # Resetting 'devis' creation page
  observeEvent(input$reset_page, {
    rv$datapresta <- tibble(
      Designation = vector(mode = "character"),
      Quantity = vector(mode = "double"),
      Unit = vector(mode = "character"),
      Unit_price = vector(mode = "double")
    )
    hide("allservice")
    #reset("create_devis_page")
  })
  
  
  # UI to choose which type of bill adder to display
  # -------------------------------------------------
  output$whichdisplayaddbill <- renderUI({
    ns <- session$ns
    mylist <- list()
    mylist[[1]] <- h4("Selection of associated quote")
    if (nrow(quotesdata()) > 0) {
      mylist[[2]] <- fluidRow(
        column(width = 6, selectizeInput(inputId = ns("which_est"), label = NULL, choices = c("", quotesdata()$ID_Quote))),
        column(width = 6, actionButton(inputId = ns("gobill"), label = "Go !"))
      )
      mylist[[3]] <- actionButton(inputId = ns("noquote"), label = "No associated quote")
    } else {
      mylist[[2]] <- actionButton(inputId = ns("noquote"), label = "No associated quote")
    }
    do.call(tagList, mylist)
  })
  
  # Modal to add new bill from a quote
  # ----------------------------------
  observeEvent(input$gobill, {
    ns <- session$ns
    showModal(
      modalDialog(
        title = "New bill", 
        uiOutput(ns("bill_with_quote")),
        footer = tagList(
          actionButton(ns("update"), "Add new", icon = icon("ok", lib = "glyphicon")),
          modalButton("Close", icon = icon("eject", lib = "glyphicon"))
        ), 
        easyClose = TRUE, 
        size = "l"
      )
    )
  })
  
  # Modal to add new bill with no associated quote
  # ----------------------------------------------
  observeEvent(input$noquote, {
    showModal(modalDialog(
      title = "New bill", "Please add an associated quote first",
      easyClose = TRUE, footer = modalButton("OK"), size = "l"
    ))
  })
  
  
  # UI to add new bill from a quote
  # -------------------------------
  output$bill_with_quote <- renderUI({
    ns <- session$ns
    mydf <- df()
    incr <- nrow(mydf) + 1
    mylist <- list()
    mylist[[1]] <- h4("Bill information")
    mylist[[2]] <- fluidRow(column(width = 6,
                                   textInput(ns("id_bill"), label = "ID_Bill", value = paste0("F", format(Sys.Date(), "%y%m"), "-", paste0(rep(0, 4 - nchar(incr)), collapse = ""), incr))),
                            column(width = 6,
                                   dateInput(inputId = ns("date"), label = "Date", format = "dd-mm-yyyy"))
    )
    mylist[[3]] <- strong("Client : ", textOutput(ns("which_client"), inline = TRUE))
    mylist[[4]] <- br()
    which_client <- quotesdata() %>%
      filter(ID_Quote == input$which_est) %>%
      pull(ID_Client)
    which_workplace <- clientsdata() %>%
      filter(ID_Client == which_client) %>%
      pull(ID_Workplace)
    which_addr <- billingaddressesdata() %>% 
      filter(ID_Workplace == which_workplace) %>%
      pull(ID_Address)
    mylist[[5]] <- fluidRow(column(width = 6,
                                   selectizeInput(inputId = ns("id_billing_address"), label = "Billing Address", choices = c("", which_addr))),
                            column(width = 6,
                                   br(),
                                   verbatimTextOutput(ns("billing_info")))
    )
    mylist[[6]] <- br()
    mylist[[7]] <- numericInput(inputId = ns("deposit"), label = "Deposit", value = 0, min = 0, max = NA, step = 1)
    do.call(tagList, mylist)
  })
  # A COMMENTER
  observeEvent(input$which_est, {
    req(input$which_est)
    output$billing_info <- renderText({
      with(billingaddressesdata()[billingaddressesdata()$ID_Address == input$id_billing_address, ],
           paste0(ifelse(!is.na(Company) | !is.na(Department), paste0(Company, " ", Department, "\n"), ""),
                  ifelse(!is.na(Address1), Address1, ""), "\n",
                  ifelse(!is.na(Address2), paste0(Address2, "\n"), ""),
                  ifelse(!is.na(Postal_code), Postal_code, ""), " ", ifelse(!is.na(City), City, ""), "\n",
                  ifelse(!is.na(Country), paste0(Country, "\n"), "")))
    })
    which_client <- quotesdata() %>%
      filter(ID_Quote == input$which_est) %>%
      pull(ID_Client)
    output$which_client <- renderText(which_client)
    which_workplace <- clientsdata() %>%
      filter(ID_Client == which_client) %>%
      pull(ID_Workplace)
    which_addr <- billingaddressesdata() %>% 
      filter(ID_Workplace == which_workplace) %>%
      pull(ID_Address)
    updateSelectizeInput(session, inputId = "id_billing_address", choices = c("", which_addr))
  })
  
  
  # Update (add new) button
  # -----------------------
  observeEvent(input$update, {
    x <- as_tibble(df())
    dd <- as_tibble(dfserv())
    x1 <- add_row(x)
    rv$no <- nrow(x1)
    ids <- rv$no
    if (mode == "quote") {
      presta <- as_tibble(isolate(rv$datapresta))
      amount <- sum(presta$Quantity*presta$Unit_price)
      x1$ID_Quote[ids] <- input$id_est
      x1$ID_Client[ids] <- input$dclient
      x1$Date[ids] <- as.character(format(ymd(input$date), "%d-%m-%Y"))
      x1$Amount[ids] <- amount
      x1$Discount[ids] <- input$discount
      x1$Net_payable[ids] <- amount*(1 - (input$discount/100))
      x1$Status[ids] <- "In_progress"
      x1$Status_comment[ids] <- ""
      z <- bind_cols(tibble(ID_Quote = rep(x1$ID_Quote[ids], nrow(rv$datapresta)),
                                    ID_Bill = rep("", nrow(rv$datapresta)),
                                    N_Service = 1:nrow(rv$datapresta)),
                     presta)
      z <- bind_rows(dd, z)
    }
    if (mode == "bill") {
      dd[dd$ID_Quote == input$which_est, "ID_Bill"] <- input$id_bill
      z <- dd
      serv <- filter(dd, ID_Quote == input$which_est)
      amount <- sum(serv$Quantity*serv$Unit_price)
      x1$ID_Bill[ids] <- input$id_bill
      x1$ID_Client[ids] <- pull(filter(quotesdata(), ID_Quote == input$which_est), ID_Client)
      x1$ID_Address[ids] <- input$id_billing_address
      x1$Date[ids] <-  as.character(format(ymd(input$date), "%d-%m-%Y")) 
      x1$Amount[ids] <- amount
      x1$Discount[ids] <- pull(filter(quotesdata(), ID_Quote == input$which_est), Discount)
      x1$Deposit[ids] <- input$deposit
      x1$Net_payable[ids] <- amount*(1 - (x1$Discount[ids]/100)) - input$deposit
      x1$Status[ids] <- "In_progress"
      x1$Payment[ids] <- ""
    }
    newdf <<- x1
    newservices <<- z
    rv$update <- rv$update + 1
  })
  
  ####################################################
  ##############  DELETE BUTTON   ####################
  ####################################################
  
  # Delete button
  # -------------
  observeEvent(input$delRow, {
    ids <- input$origTable_rows_selected
    if (length(ids) > 0) {
      x <- as_tibble(df())
      z <- as_tibble(dfserv())
      if (mode == "quote") {
        idest <- x$ID_Quote[ids]
        newservices <<- z %>%
          filter(ID_Quote != idest)
      }
      x <- x[-ids, ]
      newdf <<- x
      rv$update <- rv$update + 1
    }
    else {
      showModal(modalDialog(
        title = "Delete Row", "Please Select Row(s) To Delete. Press 'Esc' or Press 'OK' button",
        easyClose = TRUE, footer = modalButton("OK"), size = "l"
      ))
    }
  })
  
  ####################################################
  ################  EDIT BUTTON   ####################
  ####################################################
  
  # Edit Data button
  # ----------------
  observeEvent(input$edit, {
    ids <- input$origTable_rows_selected
    if (length(ids) == 1) {
      rv$no <- ids
    } else if (rv$no > nrow(df())) {
      rv$no <- 1
    }
    editData()
  })
  
  # Modal dialog to edit data
  # -------------------------
  editData <- reactive({
    input$edit
    ns <- session$ns
    showModal(
      modalDialog(
        title = paste0("Edit ", mode), 
        uiOutput(ns("displayedit")),
        footer = tagList(
          actionButton(ns("update_status"), "Save", icon = icon("ok", lib = "glyphicon")),
          modalButton("Close", icon = icon("eject", lib = "glyphicon"))
        ), 
        easyClose = TRUE, 
        size = "l"
      )
    )
  })
  
  # UI displaying data to edit in modal dialog
  # -----------------------------------------
  output$displayedit <- renderUI({
    ns <- session$ns
    ids <- rv$no
    if (length(ids) == 1) {
      mydf <- df()
      mylist <- list()
      mylist[[1]] <- inline(actionButton(inputId = ns("home"), label = NULL, icon = icon("backward", lib = "glyphicon")), m = 3)
      mylist[[2]] <- inline(actionButton(inputId = ns("left"), label = NULL, icon = icon("chevron-left", lib = "glyphicon")), m = 3)
      mylist[[3]] <- inline(numericInput(inputId = ns("rowno"), label = NULL, value = rv$no, min = 1, max = nrow(mydf), step = 1, width = 50 + 10 * log10(nrow(mydf))), m = 3)
      mylist[[4]] <- inline(actionButton(inputId = ns("right"), label = NULL, icon = icon("chevron-right",lib = "glyphicon")), m = 3)
      mylist[[5]] <- inline(actionButton(inputId = ns("end"), label = NULL, icon = icon("forward",lib = "glyphicon")), m = 3)
      mylist[[6]] <- hr()
      mydf <- as_tibble(mydf[rv$no,])
      mylist[[7]] <- inline(textInput(ns("status"), label = "Status", value = mydf$Status))
      if (mode == "quote") {
        mylist[[8]] <- inline(textInput(ns("comment"), label = "Comment", value = mydf$Status_comment))
      }
      if (mode == "bill") {
        mylist[[8]] <- inline(textInput(ns("payment"), label = "Payment method", value = mydf$Payment))
      }
      do.call(tagList, mylist)
    }
    else {
      h4("You can edit data after select one row in datatable.")
    }
  })
  
  # Update Status button
  # --------------------
  observeEvent(input$update_status, {
    x <- as_tibble(df())
    ids <- rv$no
    x$Status[ids] <- input$status
    rv$update <- rv$update + 1
    if (mode == "quote") {
      x$Status_comment[ids] <- input$comment
    }
    if (mode == "bill") {
      x$payment[ids] <- input$Payment
    }
    newdf <<- x
    rv$update <- rv$update + 1
  })
  
  # Backward button
  # ----------------
  observeEvent(input$home, {
    rv$no <- 1
  })
  
  # Forward button
  # ----------------
  observeEvent(input$end, {
    rv$no <- nrow(df())
  })
  
  # Left chevron button
  # ----------------
  observeEvent(input$left, {
    value <- ifelse(rv$no > 1, rv$no - 1, 1)
    rv$no <- value
  })
  
  # Right chevron button
  # ----------------
  observeEvent(input$right, {
    value <- ifelse(rv$no < nrow(df()), rv$no + 1, nrow(df()))
    rv$no <- value
  })
  
  # Row number selection
  # ----------------
  observeEvent(input$rowno, {
    maxno <- nrow(df())
    if (input$rowno > maxno) {
      updateNumericInput(session, "rowno", value = maxno)
      rv$no <- maxno
    }
    else {
      rv$no <- input$rowno
    }
  })
  
  # Action when row number changed
  # ------------------------------
  observeEvent(rv$no, {
    mydf <- df()
    if (!is.null(mydf)) {
      mydf <- as_tibble(mydf[rv$no, ])
      updateTextInput(session, "status", value = mydf$Status)
      if (mode == "quote") {
        updateTextInput(session, "comment", value = mydf$Status_comment)
      }
      if (mode == "bill") {
        updateTextInput(session, "payment", value = mydf$Payment)
      }
    }
  })
  
  
  ###########################################################
  ################  SAVE CHANGE BUTTON   ####################
  ###########################################################
  
  # Modal dialog to save change button
  # ----------------------------------
  observeEvent(input$savedata, {
    ns <- session$ns
    showModal(
      modalDialog(
        title = "Which delimiter should be used ?",
        radioButtons(
          inputId = ns("sep"), label = "Separation", selected = ";",
          choices = c(Comma = ",", "Semi colon" = ";", Tabulation = "\t")
        ),
        footer = tagList(
          actionButton(ns("save"), "Save", icon = icon("save", lib = "glyphicon")),
          modalButton("Close", icon = icon("eject", lib = "glyphicon"))
        ),
        easyClose = TRUE,
        size = "l"
      ))
  })
  
  # Save Button
  # -----------
  observeEvent(input$save, {
    ns <- session$ns
    write_delim(x = df(), path = normalizePath(file.path(path(), filename[1])), delim = input$sep)
    #write_delim(x = as.data.frame(dfserv()), path = normalizePath(file.path(path(), filename[2])), delim = input$sep)
    write_delim(x = dfserv(), path = normalizePath(file.path(path(), filename[2])), delim = input$sep)
    removeModal()
  })
  
  #############################################################
  ################  DOWNLOAD PDF BUTTON   ####################
  ############################################################
  
  # Modal dialog to download pdf button
  # -----------------------------------
  observeEvent(input$downloadpdf, {
    ns <- session$ns
    ids <- input$origTable_rows_selected
    if (length(ids) == 1) {
      rv$no <- ids
    } else {
      rv$no <- 1
    }
    showModal(
      modalDialog(
        title = "Printing options",
        uiOutput(ns("print_option")),
        footer = tagList(
          actionButton(inputId = ns("printpdf"), label = "Download", icon = icon("download-alt", lib = "glyphicon")),
          modalButton("Close", icon = icon("eject", lib = "glyphicon"))
        ),
        easyClose = TRUE,
        size = "l"
      ))
  })
  
  # UI for print options
  # -------------------
  output$print_option <- renderUI({
    ns <- session$ns
    ids <- rv$no
    mydf <- df()
    mylist <- list()
    mylist[[1]] <- h4("Document to print")
    mylist[[2]] <- inline(actionButton(inputId = ns("home"), label = NULL, icon = icon("backward", lib = "glyphicon")), m = 3)
    mylist[[3]] <- inline(actionButton(inputId = ns("left"), label = NULL, icon = icon("chevron-left", lib = "glyphicon")), m = 3)
    mylist[[4]] <- inline(numericInput(inputId = ns("rowno"), label = NULL, value = rv$no, min = 1, max = nrow(mydf), step = 1, width = 50 + 10 * log10(nrow(mydf))), m = 3)
    mylist[[5]] <- inline(actionButton(inputId = ns("right"), label = NULL, icon = icon("chevron-right",lib = "glyphicon")), m = 3)
    mylist[[6]] <- inline(actionButton(inputId = ns("end"), label = NULL, icon = icon("forward",lib = "glyphicon")), m = 3)
    if (mode == "quote") {
      iddoc <- pull(mydf[ids,], ID_Quote)
      thedoc <- h5(paste("Quote :", iddoc))
    }
    if (mode == "bill") {
      iddoc <- pull(mydf[ids,], ID_Bill)
      thedoc <- h5(paste("Bill :", iddoc))
    }
    idclient <- pull(mydf[ids,], ID_Client)
    thename <- pull(filter(clientsdata(), ID_Client == idclient), Name)
    thefirstname <- pull(filter(clientsdata(), ID_Client == idclient), Firstname)
    mylist[[7]] <- fluidRow(column(width = 4, thedoc),
                            column(width = 8, h5(paste("Client :", idclient, "(",thefirstname, thename,")"))))
    mylist[[8]] <- tableOutput(ns("theserv"))
    mylist[[9]] <- br()
    mylist[[10]] <- tableOutput(ns("totdat"))
    if (mode == "bill") {
      mylist[[11]] <- textAreaInput(ns("comment"), label = "Comment :")
    }
    addno <- length(mylist)
    mylist[[addno + 1]] <- h4("Output options")
    mylist[[addno + 2]] <- h5("Name of output file")
    mylist[[addno + 3]] <- textInput(inputId = ns("output_name"), label = NULL, value = iddoc)
    mylist[[addno + 4]] <- h5("Directory of output file")
    mylist[[addno + 5]] <- fluidRow(
      column(width = 8, verbatimTextOutput(ns("pathoutput"), placeholder = TRUE)),
      column(width = 4, shinyDirButton(id = ns("diroutput"), label = "Browse...", title = "Directory of output file", buttonType = "primary")))
    do.call(tagList, mylist)
  })
  
  # Table output
  # ------------
  observeEvent(rv$no, {#input$downloadpdf, {
    ns <- session$ns
    ids <- rv$no
    mydf <- df()
    servdf <- dfserv()
    idclient <- reactive(pull(mydf[ids,], ID_Client))
    if (mode == "quote") {
      iddoc <- reactive(pull(mydf[ids,], ID_Quote))
      amount <- reactive(round(pull(mydf[ids,], Amount), 2))
      discount <- reactive(pull(mydf[ids,], Discount))
      net <- reactive(round(pull(mydf[ids,], Net_payable), 2))
      theserv <- reactive({
        servdf %>%
        filter(ID_Quote == iddoc()) %>%
        select(Designation, Quantity, Unit, Unit_price) %>%
        mutate(Unit_price = round(Unit_price, 2)) %>%
        mutate(Total = round(Quantity * Unit_price, 2))
      })
      totdat <- reactive({
        tibble(
        x = c("Amount", "Discount", "Net payable"),
        y = c(amount(), paste(discount(), "%"), net())
      )
      })
    }
    if (mode == "bill") {
      idaddress <- reactive(pull(mydf, ID_Address))
      iddoc <- reactive(pull(mydf[ids,], ID_Bill))
      amount <- reactive(round(pull(mydf[ids,], Amount), 2))
      discount <- reactive(pull(mydf[ids,], Discount))
      deposit <- reactive(round(pull(mydf[ids,], Deposit), 2))
      net <- reactive(round(pull(mydf[ids,], Net_payable), 2))
      theserv <- reactive({
        servdf %>%
        filter(ID_Bill == iddoc()) %>%
        select(Designation, Quantity, Unit, Unit_price) %>%
        mutate(Unit_price = round(Unit_price, 2)) %>%
        mutate(Total = round(Quantity * Unit_price, 2))
      })
      totdat <- reactive({
        tibble(
        x = c("Amount", "Discount", "Deposit", "Net payable"),
        y = c(amount(), paste(discount(), "%"), deposit(), net())
      )
      })
    }
    output$theserv <- renderTable(theserv())
    output$totdat <- renderTable(totdat(), colnames = FALSE)
  })
  
  # Directory choice
  # ----------------
  volumes <- getVolumes()
  shinyDirChoose(input, "diroutput", roots = volumes, session = session)
  filepath <- reactive({
    req(input$diroutput)
    parseDirPath(volumes, input$diroutput)
  })
  output$pathoutput <- renderText({
    req(filepath())
    filepath()
  })
  
  # Download button
  # ---------------
  observeEvent(input$printpdf, {
    ns <- session$ns
    ids <- rv$no
    mydf <- df()
    
    # Copy the report file to a temporary directory before processing it, in
    # case we don't have write permissions to the current working dir (which
    # can happen when deployed).
    sets <- settingsdata()
    setslist <- as_list(sets)$settings
    logoname <- setslist$logo$file
    tempReport <- normalizePath(file.path(tempdir(), "template.Rmd"), mustWork = FALSE, winslash = "/")
    tempSCSS <- normalizePath(file.path(tempdir(), "template_style.scss"), mustWork = FALSE, winslash = "/")
    tempCSS <- normalizePath(file.path(tempdir(), "template_style.css"), mustWork = FALSE, winslash = "/")
    tempVar <- normalizePath(file.path(tempdir(), "_variables.scss"), mustWork = FALSE, winslash = "/")
    tempImage <- normalizePath(file.path(tempdir(), logoname), mustWork = FALSE, winslash = "/")
    file.copy(system.file("www/template.Rmd", package = "manageR"), tempReport, overwrite = TRUE)
    file.copy(system.file("www/template_style.scss", package = "manageR"), tempSCSS, overwrite = TRUE)
    file.copy(system.file(paste0("www/", logoname), package = "manageR"), tempImage, overwrite = TRUE)
    
    # SCSS compilation
    if (mode == "quote") {
      doc <- "Devis"
      ndoc <- pull(mydf[ids,], ID_Quote)
    }
    if (mode == "bill") {
      doc <- "Facture"
      ndoc <- pull(mydf[ids,], ID_Bill)
    }
    write(x = paste0("$columns: 12; \n$doc: \"", doc, "\"; \n$ndoc: \"", ndoc, "\";"), file = tempVar)
    sass(input = sass_import(tempSCSS), output = tempCSS)
    
    # Set up parameters to pass to Rmd document
    params <- list(info = list(), config = list(), client = list(), services = list())
    params$info$date <- pull(mydf[ids,], Date)
    params$info$doc <- doc
    params$info$ndoc <- ndoc
    params$info$nclient <- idclient <- pull(mydf[ids,], ID_Client)
    params$client$name <- pull(filter(clientsdata(), ID_Client == idclient), Name)
    params$client$firstname <- pull(filter(clientsdata(), ID_Client == idclient), Firstname)
    params$client$company <- pull(filter(clientsdata(), ID_Client == idclient), Company)
    params$client$department <- pull(filter(clientsdata(), ID_Client == idclient), Department)
    params$client$address1 <- pull(filter(clientsdata(), ID_Client == idclient), Address1)
    params$client$address2 <- pull(filter(clientsdata(), ID_Client == idclient), Address2)
    params$client$postal_code <- pull(filter(clientsdata(), ID_Client == idclient), Postal_code)
    params$client$city <- pull(filter(clientsdata(), ID_Client == idclient), City)
    params$client$mobile <- pull(filter(clientsdata(), ID_Client == idclient), Office_line)
    params$client$e_mail <- pull(filter(clientsdata(), ID_Client == idclient), e_mail)
    if (mode == "quote") {
      amount <- round(pull(mydf[ids,], Amount), 2)
      discount <- pull(mydf[ids,], Discount)
      net <- round(pull(mydf[ids,], Net_payable), 2)
      params$services$data <- dfserv() %>%
          filter(ID_Quote == ndoc) %>%
          select(Designation, Quantity, Unit, Unit_price) %>%
          mutate(Unit_price = round(Unit_price, 2)) %>%
          mutate(Total = round(Quantity * Unit_price, 2))
      params$services$totdata <- tibble(
          x = c("Amount", "Discount", "Net payable"),
          y = c(amount, paste(discount, "%"), net)
        )
    }
    if (mode == "bill") {
      idaddress <- pull(mydf[ids,], ID_Address)
      params$info$nclient <- paste(params$info$nclient, idaddress, sep = "\n")
      params$info$doc <- "Facture"
      params$billing$company <- pull(filter(billingaddressesdata(), ID_Address == idaddress), Company)
      params$billing$department <- pull(filter(billingaddressesdata(), ID_Address == idaddress), Department)
      params$billing$address1 <- pull(filter(billingaddressesdata(), ID_Address == idaddress), Address1)
      params$billing$address2 <- pull(filter(billingaddressesdata(), ID_Address == idaddress), Address2)
      params$billing$postal_code <- pull(filter(billingaddressesdata(), ID_Address == idaddress), Postal_code)
      params$billing$city <- pull(filter(billingaddressesdata(), ID_Address == idaddress), City)
      params$billing$siret <- pull(filter(billingaddressesdata(), ID_Address == idaddress), Register_Siret)
      params$comment <- input$comment
      amount <- round(pull(mydf[ids,], Amount), 2)
      discount <- pull(mydf[ids,], Discount)
      deposit <- round(pull(mydf[ids,], Deposit), 2)
      net <- round(pull(mydf[ids,], Net_payable), 2)
      params$services$data <- dfserv() %>%
          filter(ID_Bill == ndoc) %>%
          select(Designation, Quantity, Unit, Unit_price) %>%
          mutate(Unit_price = round(Unit_price, 2)) %>%
          mutate(Total = round(Quantity * Unit_price, 2))
      params$services$totdata <- tibble(
          x = c("Amount", "Discount", "Deposit", "Net payable"),
          y = c(amount, paste(discount, "%"), deposit, net)
        )
    }
    params$config <- map(setslist$config, unlist)
    params$bankinfo <- map(setslist$bankinfo, unlist)
    params$logo <- map(setslist$logo, unlist)
    params$services$tva <- "no"
    
    # knit the document
    render(tempReport, #output_file = input$output_name,
                      params = params, #output_format = hpdf_document_base(),
                      envir = new.env(parent = globalenv()),
                      encoding = "UTF-8")
    
    # Close modal dialog
    removeModal()
    
    # Copy the document to the final directory
    tempOutput <- normalizePath(file.path(tempdir(), "template.pdf"), mustWork = FALSE)
    finalOutput <- normalizePath(file.path(filepath(), paste0(input$output_name, ".pdf")), mustWork = FALSE)
    file.copy(tempOutput, finalOutput, overwrite = TRUE)
    
  })


  
  ###########################################################
  #################  DISPLAY DATATABLE   ####################
  ###########################################################
  
  output$origTable <- renderDataTable({
    datatable(df(), selection = "single", caption = NULL)
  })
  
  ################################################
  #################  RETURN   ####################
  ################################################
  return(list(data = reactive(df()), serv = reactive(dfserv()), up = reactive(rv$update)))

}