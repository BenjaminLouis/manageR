ui <- dashboardPage(
  #includeCSS("styles.css"),
  
  ## HEADER ##
  ############
  dashboardHeader(title = "ManageR"),
  
  ## SIDEBAR ##
  #############
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Cost estimates", tabName = "estimates", icon = icon("money")),
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
        wdLoadUI("wd"),
        br(),
        loadingOptionsUI("files")
      ),
      
      # Cost estimates tab
      #-------------------
      tabItem(
        tabName = "estimates",
        h4("Cost estimates management"),
        editBillsUI("estimates", mode = "estimate")
      ),
      
      # Bills tab
      #----------
      tabItem(
        tabName = "bills",
        h4("Bills management"),
        editBillsUI("bills", mode = "bill")
      ),
      
      # Budget tab
      #-----------
      tabItem(
        tabName = "budget",
        h4("Budget management")
      ),
      
      # Clients tab
      #------------
      tabItem(
        tabName = "clients",
        h4("Clients informations"),
        editableDTUI("clients")
      ),
      
      # Billing adresses tab
      #---------------------
      tabItem(
        tabName = "addresses",
        h4("Billing addresses information"),
        editableDTUI("billingaddresses")
      ),
      
      # Missions tab
      #-------------
      tabItem(
        tabName = "missions",
        h4("Missions information")
      )
    )
  )
)
