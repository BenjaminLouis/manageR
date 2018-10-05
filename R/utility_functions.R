#' A function to display shiny elements inline
#'
#' @param x the elements to display
#' @param m margin size (in px)
#' @param va vertical alignement (top, middle, bottom)
#'
#' @return the element to display in a html div tag with the relevant css style
#' 
#' @importFrom shiny tags
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' inline(textInput("text", label = "Enter text", value = ""))
#' }
inline = function(x, m = 10, va = "top") {
  tags$div(style = paste0("display:inline-block; margin:", m, "px; vertical-align:", va, ";"), x)
}


#' Function to insert fontawesom icon in Rmd rendered with weasydoc
#'
#' @param x string. Name of the fontawesome icon
#'
#' @return a html code as string
#' @export
#' 
#' @importFrom base64enc base64encode
#' @importFrom fontawesome fa
#' @importFrom glue glue
#' 
#' @examples
#' insert_fa("r-project")
insert_fa <- function(x) {
  if (Sys.info()["sysname"] == "Linux") {
    res <- glue("<i class=\"fa fa-{x}\"></i>") 
  }
  if (Sys.info()["sysname"] == "Windows") { 
    fa_r <- fa(glue("{x}"))
    fa_r_raw <- charToRaw(fa_r)
    fa_r_enc <- paste0("data:image/svg+xml;charset=utf-8;base64,", base64encode(fa_r_raw))
    res <- glue("<img alt=\"icon\" src=\"{fa_r_enc}\">")
  }
  return(res)
}


#' Title
#'
#' @param x a HTML table with <thead> header tags
#'
#' @return the same HTML table but without the header part
#' @export
#'
#' @examples
#' library(knitr)
#' tab <- kable(iris, format = "html")
#' tab
#' 
#' #Removing header
#' remove_header(tab)
#' 
remove_header <- function(x) { gsub("<thead>.*</thead>", "", x) }


#' Concatenate strings and ignore missing values
#'
#' @param ... several string values
#'
#' @return Concatenated strings given as input with space delimiter. A missing 
#' value (\code{NA}) is simply ignored. If all niputs are \code{NA}'s, \code{NA} is
#' returned
#' @export
#'
#' @examples
#' concatenate("Hello", NA, "world", NA)
#' concatenate(NA, NA)
concatenate <- function(...) {
  st <- glue(..., .na = "", .sep = " ")
  st <- gsub("\\s{2,}", " ", st)
  st <- gsub("(^\\s+)|(\\s+$)", "", st)
  st <- gsub("^$", NA, st)
  return(st)
}


#' Transform a number into a string representing the number with two decimals, 
#' a comma delimiter and the euro sign
#'
#' @param x a numeric value
#'
#' @return the numric value with two decimals, a comma delimiter, and the euro sign
#' @export
#'
#' @examples
#' parse_amount(pi)
parse_amount <- function(x) {
  x <- format(x, nsmall = 2, digits = 2)
  x <- gsub("\\.", ",", x)
  x <- paste(x, "\u20AC")
  return(x)
}