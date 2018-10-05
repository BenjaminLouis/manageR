#' Server function
#' 
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @importFrom readr cols col_character col_double col_integer
#' @importFrom shiny callModule reactive observeEvent
#' @importFrom xml2 read_xml
#' 
app_server <- function(input, output, session) {
  
  # Home
  # ----
  initsets <- reactive(read_xml(system.file("www/config.xml", package = "manageR")))
  settings <- callModule(mod_edit_settings, "config", settingsdata = initsets, package = "manageR")
  path <- callModule(mod_wd_load, "wd")
  initquotes <- callModule(mod_loading_options, "files", path = path, filename = reactive("Quotes.csv"), coltypes = cols(.default = col_character(), Amount = col_double(), Discount = col_double(), Net_payable = col_double()))
  initbills <- callModule(mod_loading_options, "files", path = path, filename = reactive("Bills.csv"), coltypes = cols(.default = col_character(), Amount = col_double(), Discount = col_double(), Net_payable = col_double(), Deposit = col_double()))
  initclients <- callModule(mod_loading_options, "files", path = path, filename = reactive("Clients.csv"))
  initbillingaddresses <- callModule(mod_loading_options, "files", path = path, filename = reactive("Billing_Addresses.csv"))
  initservices <- callModule(mod_loading_options, "files", path = path, filename = reactive("Services.csv"), coltypes = cols(.default = col_character(), N_Service = col_integer(), Quantity = col_double(), Unit_price = col_double()))
  
  # Quotes
  # --------------
  resquote <- callModule(mod_edit_bills, "quotes", data = initquotes, servicesdata = initservices, clientsdata = clients, quotesdata = NULL, path = path, filename = c("Quotes.csv", "Services.csv"), mode = "quote")
  quotes <- reactive(resquote$data())
  proxyservicesquote <- reactive(resquote$up())
  
  # Bills
  # -----
  resbill <- callModule(mod_edit_bills, "bills", data = initbills, servicesdata = services, clientsdata = clients, quotesdata = quotes, billingaddressesdata = billingaddresses, path = path, filename = c("Bills.csv", "Services.csv"), mode = "bill")
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
  clients <- callModule(mod_edit_table, "clients", data = initclients, path = path, filename = "Clients.csv")#, ncol = 4)
  
  # Billing addresses
  # -----------------
  billingaddresses <- callModule(mod_edit_table, "billingaddresses", data = initbillingaddresses, path = path, filename = "Billing_Addresses.csv")#, ncol = 4)
}