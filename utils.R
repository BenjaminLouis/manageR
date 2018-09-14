inline = function(x, m = 10, va = "top") {
  tags$div(style = paste0("display:inline-block; margin:", m, "px; vertical-align:", va, ";"), x)
}