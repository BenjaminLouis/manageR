server <- function(input, output, session) {
  
  # Home
  # ----
  path <- callModule(wdLoad, "wd")
  initquotes <- callModule(loadingOptions, "files", path = path, filename = "Quotes.csv", coltypes = cols(.default = col_character(), Date = col_date(format = "%d-%m-%Y")))
  initbills <- callModule(loadingOptions, "files", path = path, filename = "Bills.csv", coltypes = cols(.default = col_character(), Date = col_date(format = "%d-%m-%Y"), Amount = col_double()))
  initclients <- callModule(loadingOptions, "files", path = path, filename = "Clients.csv")
  initbillingaddresses <- callModule(loadingOptions, "files", path = path, filename = "Billing_Addresses.csv")
  initservices <- callModule(loadingOptions, "files", path = path, filename = "Services.csv", coltypes = cols(.default = col_character(), N_Service = col_integer(), Quantity = col_double(), Unit_price = col_double()))
  
  # Quotes
  # --------------
  res <- callModule(editBills, "quotes", data = initquotes, servicesdata = initservices, clientsdata = clients, quotesdata = NULL, path = path, filename = c("Quotes.csv", "Services.csv"), mode = "quote")
  quotes <- reactive(res$data())
  services <- reactive(res$serv())
  
  # Bills
  # -----
  bills <- callModule(editBills, "bills", data = initbills, servicesdata = services, clientsdata = clients, quotesdata = quotes, billingaddressesdata = billingaddresses, path = path, filename = "Bills.csv", mode = "bill")
  
  # Clients
  # -------
  clients <- callModule(editableDT, "clients", data = initclients, path = path, filename = "Clients.csv")#, ncol = 4)
  
  # Billing addresses
  # -----------------
  billingaddresses <- callModule(editableDT, "billingaddresses", data = initbillingaddresses, path = path, filename = "Billing_Addresses.csv")#, ncol = 4)
}