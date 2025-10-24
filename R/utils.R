wooclap <- function(x){
  title <- ifelse(knitr::is_latex_output(), "\\large{Question Time!}", "Question Time")
  cat(
    sprintf("<center> Question %s Time! {{< qrcode %s >}} </center>",title, x)
  )
}