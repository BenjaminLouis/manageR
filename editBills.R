editBillsUI <- function(id, mode = "bill") {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      inline(actionButton(ns("add"), paste("New", mode), icon = icon("plus", lib = "glyphicon")), va = "middle"),
      inline(actionButton(ns("delRow"), paste("Delete", mode), icon = icon("remove", lib = "glyphicon")), va = "middle"),
      inline(actionButton(ns("edit"), "Edit status", icon = icon("wrench", lib = "glyphicon")), va = "middle"),
      inline(actionButton(ns("savedata"), label = "Save change", icon = icon("floppy-save", lib = "glyphicon")), va = "middle"),
      inline(actionButton(ns("downloadpdf"), label = "Download PDF", icon = icon("download-alt", lib = "glyphicon")), va = "middle")
    ),
    br(),
    DT::dataTableOutput(ns("origTable"))
  )
}


editBills <- function(input, output, session, data = reactive(NULL), servicesdata = reactive(NULL), clientsdata = reactive(NULL), quotesdata = NULL, billingaddressesdata = NULL, path, filename, mode = "bill") {
  
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
     rv$datapresta <- tibble::tibble(
       Designation = vector(mode = "character"),
       Quantity = vector(mode = "double"),
       Unit = vector(mode = "character"),
       Unit_price = vector(mode = "double")
     )
  })
  
  # Delete button
  # -------------
  observeEvent(input$delRow, {
    ids <- input$origTable_rows_selected
    if (length(ids) > 0) {
      x <- tibble::as_tibble(df())
      z <- tibble::as_tibble(dfserv())
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
  
  # Add New button
  # --------------
  observeEvent(input$add, {
    addData()
  })
  
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
  
  # UI adding data to edit in modal dialog
  # --------------------------------------
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
    mylist[[5]] <- shiny::tableOutput(ns("allservice"))
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
    mylist[[5]] <- fluidRow(column(width = 6,
                                   selectizeInput(inputId = ns("id_billing_address"), label = "Billing Address", choices = c("", pull(filter(billingaddressesdata(), ID_Client == pull(filter(quotesdata(), ID_Quote == input$which_est), ID_Client)), ID_Address)))),
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
  
  
  # DEFINITION PRESTATIONS
  dttemp <- reactive({    
    tibble::tibble(
      Designation = input$design,
      Quantity = input$qtity,
      Unit = input$unit,
      Unit_price = input$price
    )})
  # Action du bouton pour ajouter des lignes de presta
  observeEvent(input$add_service, {
    rv$datapresta <- rbind(rv$datapresta, dttemp())
    output$allservice <- shiny::renderTable(rv$datapresta)
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
    rv$datapresta <- tibble::tibble(
      Designation = vector(mode = "character"),
      Quantity = vector(mode = "double"),
      Unit = vector(mode = "character"),
      Unit_price = vector(mode = "double")
    )
    shinyjs::hide("allservice")
    #reset("create_devis_page")
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
      mydf <- tibble::as_tibble(mydf[rv$no,])
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
  
  
  # Update button
  # --------------
  observeEvent(input$update, {
    x <- tibble::as_tibble(df())
    presta <- tibble::as_tibble(isolate(rv$datapresta))
    dd <- tibble::as_tibble(dfserv())
    x1 <- tibble::add_row(x)
    rv$no <- nrow(x1)
    ids <- rv$no
    if (mode == "quote") {
      x1$ID_Quote[ids] <- input$id_est
      x1$ID_Client[ids] <- input$dclient
      x1$Date[ids] <- parse_date(input$date, format = "%d-%m-%Y")
      x1$Status[ids] <- "In_progress"
      x1$Status_comment[ids] <- ""
      z <- bind_cols(tibble::tibble(ID_Quote = rep(x1$ID_Quote[ids], nrow(rv$datapresta)),
                                       N_Service = 1:nrow(rv$datapresta)),
                            presta)
      z <- bind_rows(dd, z)
    }
    if (mode == "bill") {
      serv <- filter(dd, ID_Quote == input$which_est)
      amount <- sum(serv$Quantity*serv$Unit_price)
      x1$ID_Bill[ids] <- input$id_bill
      x1$ID_Client[ids] <- pull(filter(quotesdata(), ID_Quote == input$which_est), ID_Client)
      x1$ID_Address[ids] <- input$id_billing_address
      x1$Date[ids] <- parse_date(input$date, format = "%d-%m-%Y")
      x1$Amount[ids] <- amount
      x1$Status[ids] <- "In_progress"
      x1$Payment[ids] <- ""
    }
    newdf <<- x1
    newservices <<- z
    rv$update <- rv$update + 1
  })
  
  # Update Status button
  # --------------------
  observeEvent(input$update_status, {
    x <- tibble::as_tibble(df())
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
      mydf <- tibble::as_tibble(mydf[rv$no, ])
      updateTextInput(session, "status", value = mydf$Status)
      if (mode == "quote") {
        updateTextInput(session, "comment", value = mydf$Status_comment)
      }
      if (mode == "bill") {
        updateTextInput(session, "payment", value = mydf$Payment)
      }
    }
  })
  
  # Save as csv button
  # ------------------
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
    if (mode == "quote") { write_delim(x = dfserv(), path = normalizePath(file.path(path(), filename[2])), delim = input$sep) }
    removeModal()
  })
  
  # Display Datatable
  # -----------------
  output$origTable <- DT::renderDataTable({
    datatable(df(), selection = "single", caption = NULL)
  })
  
  # Return
  if (mode == "quote") {
    return(list(data = reactive(df()), serv = reactive(dfserv())))
  }
  if (mode == "bill") { 
    return(reactive(df()))
  }
}