server <- function(input, output, session) {
  
  # Home
  # ----
  path <- callModule(wdLoad, "wd")
  initquotes <- callModule(loadingOptions, "files", path = path, filename = "Quotes.csv", coltypes = cols(.default = col_character(), Amount = col_double(), Discount = col_double(), Net_payable = col_double()))
  initbills <- callModule(loadingOptions, "files", path = path, filename = "Bills.csv", coltypes = cols(.default = col_character(), Amount = col_double(), Discount = col_double(), Net_payable = col_double(), Deposit = col_double()))
  initclients <- callModule(loadingOptions, "files", path = path, filename = "Clients.csv")
  initbillingaddresses <- callModule(loadingOptions, "files", path = path, filename = "Billing_Addresses.csv")
  initservices <- callModule(loadingOptions, "files", path = path, filename = "Services.csv", coltypes = cols(.default = col_character(), N_Service = col_integer(), Quantity = col_double(), Unit_price = col_double()))
  
  # Quotes
  # --------------
  resquote <- callModule(editBills, "quotes", data = initquotes, servicesdata = initservices, clientsdata = clients, quotesdata = NULL, path = path, filename = c("Quotes.csv", "Services.csv"), mode = "quote")
  quotes <- reactive(resquote$data())
  proxyservicesquote <- reactive(resquote$up())
  
  # Bills
  # -----
  resbill <- callModule(editBills, "bills", data = initbills, servicesdata = services, clientsdata = clients, quotesdata = quotes, billingaddressesdata = billingaddresses, path = path, filename = c("Bills.csv", "Services.csv"), mode = "bill")
  bills <- reactive(resbill$data())
  proxyservicesbill <- reactive(resbill$up())
  
  # Services
  # --------
  services <- reactive({
    if (proxyservicesquote() == 0 & proxyservicesbill() == 0) {
      df <- initservices()
    }
    else {
      observeEvent(proxyservicesquote, {
        df <- resquote$serv() 
      })
      observeEvent(proxyservicesbill, {
        df <- resbill$serv()
      })
    }
    df
  })

  
  # Clients
  # -------
  clients <- callModule(editableDT, "clients", data = initclients, path = path, filename = "Clients.csv")#, ncol = 4)
  
  # Billing addresses
  # -----------------
  billingaddresses <- callModule(editableDT, "billingaddresses", data = initbillingaddresses, path = path, filename = "Billing_Addresses.csv")#, ncol = 4)
}