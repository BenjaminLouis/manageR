#' UI function
#' 
#' @importFrom shiny icon h4 br
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar sidebarMenu menuItem dashboardBody tabItems tabItem
#' 
app_ui <- function() {
  dashboardPage(
    
    ## HEADER ##
    ############
    dashboardHeader(title = "ManageR"),
    
    ## SIDEBAR ##
    #############
    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Quotes", tabName = "quotes", icon = icon("money")),
        menuItem("Bills", tabName = "bills", icon = icon("bills")),
        menuItem("Budget", tabName = "budget", icon = icon("euro")),
        menuItem("Clients", tabName = "clients", icon = icon("address-card")),
        menuItem("Billing addresses", tabName = "addresses", icon = icon("envelope")),
        menuItem("Missions", tabName = "missions", icon = icon("briefcase"))
      )
    ),
    
    ## BODY ##
    ##########
    dashboardBody(
      #useShinyjs(),
      tabItems(
        
        # Home tab
        #----------
        tabItem(
          tabName = "home",
          h4("Welcome to manageR !!"),
          mod_wd_loadUI("wd"),
          br(),
          mod_loading_optionsInput("files")
        ),
        
        # Quotes tab
        #-------------------
        tabItem(
          tabName = "quotes",
          h4("Quotes management"),
          mod_edit_billsUI("quotes", docmode = "quote")
        ),
        
        # Bills tab
        #----------
        tabItem(
          tabName = "bills",
          h4("Bills management"),
          mod_edit_billsUI("bills", docmode = "bill")
        ),
        
        # Budget tab
        #-----------
        # tabItem(
        #   tabName = "budget",
        #   h4("Budget management")
        # ),
        
        # Clients tab
        #------------
        tabItem(
          tabName = "clients",
          h4("Clients informations"),
          mod_edit_tableUI("clients")
        ),
        
        # Billing adresses tab
        #---------------------
        tabItem(
          tabName = "addresses",
          h4("Billing addresses information"),
          mod_edit_tableUI("billingaddresses")
        )#,
        
        # Missions tab
        #-------------
        # tabItem(
        #   tabName = "missions",
        #   h4("Missions information")
        # )
      )
    )
  )
}