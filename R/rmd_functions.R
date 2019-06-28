#' Internal function to process the logo part in Rmd template. Do not use outside
#'
#' @param logo list of logo from the YAML params
#'
#' @importFrom htmltools HTML
#' 
#' @export
#' 
process_logo <- function(logo) {
  image <- NULL
  if (logo$yes) {
    image <- paste0("<img src = \"", logo$file, "\" alt = \"logo\" style = \"width:", logo$width, ";\">")
  }
  return(HTML(image))
}


#' Internal function to process the company name part in Rmd template. Do not use outside
#'
#' @param name name of company from the YAML params
#'
#' @importFrom htmltools HTML
#' 
#' @export
#' 
process_name <- function(name) {
  x <- NULL
  if (!name %in% c("NA", "") & !is.na(name)) {
    x <- name
  }
  return(HTML(x))
}

#' Internal function to process the config part in Rmd template. Do not use outside
#'
#' @param config list of config from the YAML params
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map_int
#' @importFrom glue glue
#' @importFrom htmltools HTML
#' @importFrom kableExtra column_spec collapse_rows
#' @importFrom knitr kable
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' 
#' @export
#' 
process_config <- function(config) {
  lconfig <- config
  lconfig <- lconfig[names(lconfig) != "name"]
  lconfig[map_int(lconfig, length) == 0] <- NA
  lconfig[lconfig == "NA" | lconfig == ""] <- NA
  lconfig$address3 <- concatenate(lconfig$postal_code, lconfig$city)
  lconfig <- lconfig[c("address1", "address2", "address3", "mobile", "e_mail", "web", "siret")]
  if (!is.na(lconfig$siret)) { lconfig$siret <- paste(":", lconfig$siret)}
  lconfig <- lconfig[!is.na(lconfig)]
  address <- insert_fa("home")
  mobile <- insert_fa("mobile-alt")
  e_mail <- insert_fa("at")
  web <- insert_fa("globe")
  siret <- "Siret" 
  lconfig <- as_tibble(lconfig) %>%
    gather() %>%
    mutate(key = gsub("address\\d", "address", key)) %>%
    mutate(key = paste0("{", key, "}")) %>%
    kable() %>%
    column_spec(column = 1, width = "8mm", extra_css = "text-align:center;") %>%
    collapse_rows(valign = "top") %>%
    glue() %>%
    remove_header() %>%
    HTML()
  return(lconfig)
}

#' Internal function to process the boxheader part in Rmd template. Do not use outside
#'
#' @param info list of info from the YAML params
#' @param which either 1, 2 or 3 depending of the boxheader targeted
#'
#' @importFrom htmltools HTML
#' @importFrom stringr str_split
#' 
#' @export
#' 
process_boxheader <- function(info, which) {
  if (which == 1) {
    tobox <- paste0("<p>Date</p><hr><p>", ifelse(info$date == "NA", format(Sys.Date(), "%d/%m/%Y"), info$date), "</p>")
  } else if (which == 2) {
    tobox <- paste0("<p>N\u00b0 " , info$doc, "</p><hr><p>", info$ndoc, "</p>")
  } else if (which == 3) {
    nclient <- unlist(str_split(info$nclient, pattern = "\n"))
    nclient <- paste0("<p>", nclient, "</p>", collapse = "")
    tobox <- paste0("<p>N\u00b0 Client", "<hr>", nclient)
  }
  return(HTML(tobox))
}
  

#' Internal function to process the client and billing parts in Rmd template. Do not use outside
#'
#' @param client client list from YAML params
#' @param billing billing list from YAML params
#' @param info info list from YAML params
#'
#' @importFrom htmltools HTML
#' 
#' @export
#' 
process_client <- function(client, billing, info) {
  lclient <- client
  lclient[lclient == "NA" | lclient == ""] <- NA
  lclient$who <- concatenate(lclient$firstname, lclient$name)
  lclient$where <- concatenate(lclient$company, lclient$department)
  lclient$address3 <- concatenate(lclient$postal_code, lclient$city)
  lclient <- lclient[c("who", "where","address1", "address2", "address3", "mobile", "e_mail")]
  lclient <- lclient[!is.na(lclient)]
  lclient <- lapply(lclient, function(x) paste("<p>", x ,"</p>"))
  header <- switch(tolower(info$doc),
                   facture = "Livraison :",
                   devis = "Client :",
                   invoice = "Delivery :",
                   quote = "Client :")
  box1 <- paste0("<p class = \"headclient\">", header, 
                 "</p>",paste(lclient, collapse = ""))
  if (tolower(info$doc) %in% c("facture", "invoice")) {
    lbilling <- billing
    lbilling[lbilling == "NA" | lbilling == ""] <- NA
    lbilling$where <- concatenate(lbilling$company, lbilling$department)
    lbilling$address3 <- concatenate(lbilling$postal_code, lbilling$city)
    if (!is.na(lbilling$siret)) { lbilling$siret <- paste("Siret", lbilling$siret)}
    lbilling <- lbilling[c("where", "siret","address1", "address2", "address3", "mobile", "e_mail")]
    lbilling <- lbilling[!is.na(lbilling)]
    lbilling <-  lapply(lbilling, function(x) paste("<p>", x ,"</p>"))
    header <- switch(tolower(info$doc),
                     facture = "Facturation :",
                     invoice = "Billing :")
    box <- paste0("<div class = \"box\" id = \"bill\"><div class = \"row\"><div class = \"col-6\">",box1,"</div><div class = \"col-6\"><p class = \"headclient\">", header, "</p>",paste(lbilling, collapse = ""), "</div></div></div>")
  } else {
    box <- paste0("<div class = \"box\" id = \"quote\"><div class = \"row\">",box1,"</div></div>")
  }
  return(HTML(box))
}


#' Internal function to process the comment and billing parts in Rmd template. Do not use outside
#'
#' @param comment comment from YAML params
#' @param info info list from YAML params
#'
#' @importFrom htmltools HTML
#' 
#' @export
#' 
process_comment <- function(comment, info) {
  if (tolower(info$doc) %in% c("invoice", "facture")) {
    if (!(comment %in% c("NA", "")) & !is.na(comment)) {
      comment <- paste("<p class = \"bold\">Commentaire : </p><p>", comment,"</p>")
    } else {
      comment <- NULL
    }
    HTML(comment)
  }
}


#' Internal function to process the services part in Rmd template. Do not use outside
#'
#' @param services services list from YAML params
#'
#' @importFrom dplyr bind_rows mutate_at vars matches mutate
#' @importFrom htmltools HTML
#' @importFrom kableExtra column_spec
#' @importFrom knitr kable
#' @importFrom tibble tibble
#' 
#' @export
#' 
process_services <- function(services) {
  data <- tibble('D\u00e9signation' = NA,#character(),
                 'Quantit\u00e9' = NA,#double(),
                 'Unit\u00e9' = NA,#character(),
                 'Prix unitaire' = NA,#double(),
                 'Total' = NA)#double())
  if (any(!(services$data %in% c("NA", ""))) & any(!is.na(services$data))) {
    temp <- services$data
    colnames(temp) <- colnames(data)
    data <- bind_rows(data, temp)
    data <- data[-1,]
  }
  data <- data %>%
    mutate_at(vars(matches('Prix unitaire')), parse_amount) %>%
    mutate('Total' = parse_amount(Total)) %>%
    kable(format = "html") %>%
    column_spec(column = 1, width = "40%", extra_css = "text-align:justify;") %>%
    column_spec(column = 2, width = "13%", extra_css = "text-align:right;") %>%
    column_spec(column = 3, width = "11%", extra_css = "text-align:right;") %>%
    column_spec(column = 4, width = "20%", extra_css = "text-align:right;") %>%
    column_spec(column = 5, width = "16%", extra_css = "text-align:right;") %>%
    HTML()
  return(data)
}


#' Internal function to process the total part in Rmd template. Do not use outside
#'
#' @param services services list from YAML params
#' @param mode doc mode from YAML params
#'
#' @importFrom htmltools HTML
#' @importFrom kableExtra column_spec
#' @importFrom knitr kable
#' @importFrom tibble tibble
#' 
#' @export
#' 
process_total <- function(services, mode) {
  if (tolower(mode) %in% c("devis", "quote")) {
    totdata <- tibble(
      x = c("Amount", "Discount", "Net payable"),
      y = c(0, paste(0, "%"), 0)
    ) 
  } else if (tolower(mode) %in% c("facture", "invoice")) {
    totdata <- tibble(
      x = c("Amount", "Discount", "Deposit", "Net payable"),
      y = c(0, paste(0, "%"), 0, 0)
    ) 
  }
  if (any(!(services$totdata %in% c("NA", ""))) & any(!is.na(services$totdata))) {
    totdata <- services$totdata
  }
  totdata[totdata$x != "Discount", 2] <- parse_amount(as.numeric(unlist(totdata[totdata$x != "Discount", 2]))) # ugly!
  if (tolower(mode) %in% c("devis", "quote")) {
    totdata$x <- c("Total", "Remise", "Net \u00e0 payer")
  } else if (tolower(mode) %in% c("facture", "invoice")) {
    totdata$x <- c("Total", "Remise", "Acompte", "Net \u00e0 payer") 
  }

  totdata <- totdata %>%
    kable(format = "html") %>%
    column_spec(column = 1, width = "55.6%", extra_css = "text-align:right; font-weight:bold;") %>%
    column_spec(column = 2, width = "44.4%", extra_css = "text-align:right;") %>%
    remove_header() %>%
    HTML()
  return(totdata)
}

#' Internal function to process the tva part in Rmd template. Do not use outside
#'
#' @param services services list from YAML params
#'
#' @importFrom htmltools HTML
#' 
#' @export
#' 
process_tva <- function(services) {
  tva <- NULL
  if (services$tva == "no") {
    tva <- "TVA non applicable, article 293B du CGI"
  }
  return(HTML(tva))
}


#' Internal function to process the bank part in Rmd template. Do not use outside
#'
#' @param info info list from YAML params
#' @param bankinfo bank list from YAML params
#' @param config config list from YAML params
#'
#' @importFrom htmltools HTML
#' 
#' @export
#' 
process_bank <- function(info, bankinfo, config) {
  if (tolower(info$doc) %in% c("devis", "quote")) {
    foot <- paste("<p id = \"footquote\">Afin de valider ce devis, merci de le signer et le renvoyer \u00e0", config$e_mail, ":</p>",
                  "<div class = \"footbox\"><p class = \"medium\">Bon pour accord le : </p>",
                  "<p class = \"medium\">Signature : </p></div>")
  }
  if (tolower(info$doc) %in% c("facture", "invoice")) {
    foot <- paste("<p>Merci d\u2019effectuer le paiement \u00e0 r\u00e9ception de la facture par virement bancaire en utilisant les coordonn\u00e9es ci-dessous et en pr\u00e9cisant le num\u00e9ro de la facture dans le libell\u00e9 :</p>",
                  "<table><tr><th class = \"bold\">Titulaire :</th><th>",bankinfo$holder,"</th></tr>",
                  "<tr><th class = \"bold\">Banque :</th><th>",bankinfo$bank,"</th></tr>",
                  "<tr><th class = \"bold\">BIC :</th><th>",bankinfo$bic,"</th></tr>",
                  "<tr><th class = \"bold\">IBAN :</th><th>",bankinfo$iban,"</th></tr></table>")
  }
  return(HTML(foot))
}


#' Internal function to process annexes. Do not use outside
#'
#' @param file character. Path of the annexes file
#'
#' @importFrom rmarkdown render
#' @importFrom xml2 read_html xml_find_all xml_children xml_new_root xml_add_child xml_remove
#' @importFrom purrr walk
#' @importFrom htmltools HTML
#' 
#' @export
#' 
process_annexes <- function(file) {
  temphtml <- normalizePath(file.path(tempdir(), "annexes.html"), mustWork = FALSE, winslash = "/")
  render(file, output_format = "html_document", output_file = temphtml, 
         envir = new.env(parent = globalenv()), quiet = TRUE)
  
  toadd <- read_html(temphtml) %>% xml_find_all(".//body") %>% xml_children()
  newroot <- xml_new_root("div", class = "newpage")
  walk(toadd, ~xml_add_child(.x = newroot, .value = .x))
  walk(xml_find_all(newroot, "script"), xml_remove)
  HTML(gsub("<\\?xml.+\\?>\n", "", as.character(newroot)))
}

