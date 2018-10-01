#' A function ti display shiny elements inline
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