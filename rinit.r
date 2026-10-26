library(knitr)
opts_chunk$set(
  fig.pos="htb",
  out.extra="",
  dev="ragg_png",
  dev.args = list(bg = "transparent"),
  out.width="100%",
  fig.showtext=TRUE,
  message = qmd_message,
  warning = qmd_warning,
  echo = qmd_echo)

## Checking rinit packages installation
library(tidyverse)
library(ofce)
library(gt)
library(ggiraph)
library(scales)
library(glue)
library(PrettyCols)
library(lobstr)
library(quarto)
library(showtext)

font_size <- ifelse(knitr::is_html_output(), 12, 9)
small_font_size <- 0.6*font_size
options(
  ofce.base_size = font_size,
  ofce.background_color = "transparent",
  ofce.source_data.src_in = "project",
  ofce.caption.ofce = FALSE,
  ofce.marquee = "munch",
  ofce.caption.srcplus = NULL,
  ofce.caption.wrap = 0)

ggplot2::set_theme(ofce::theme_ofce())

if(knitr::is_html_output())
  ggplot2::update_theme(text = element_text(size = 10)) else
    ggplot2::update_theme(text = element_text(size = 8))

tooltip_css  <-
  "font-family:Arimo;
  background-color:snow;
  border-radius:5px;
  border-color:gray;
  border-style:solid;
  border-width:0.5px;
  font-size:9pt;
  padding:4px;
  box-shadow: 2px 2px 2px gray;
  r:20px;"

margin_download <- function(data, output_name = "donnees", label = "données") {
  if(knitr::is_html_output()) {
    if(lobstr::obj_size(data)> 1e+5)
      cli::cli_alert("la taille de l'objet est supérieure à 100kB")
    fn <- tolower(output_name)
    downloadthis::download_this(
      data,
      icon = "fa fa-download",
      class = "dbtn",
      button_label  = label,
      output_name = fn)
  } else
    return(invisible(NULL))
}

margin_link <- function(data, output_name = "donnees", label = "données") {
  if(knitr::is_html_output()) {
    link <- stringr::str_c("dnwld/", output_name, ".csv")
    vroom::vroom_write(data, link, delim = ";")
    downloadthis::download_link(
      link,
      icon = "fa fa-download",
      class = "dbtn",
      button_label  = label)
  } else
    return(invisible(NULL))
}


milliards <- function(x, n_signif = 3L) {
  stringr::str_c(
    format(
      x,
      digits = n_signif,
      big.mark = " ",
      decimal.mark = ","),
    " milliards d'euros")
}

f_taux <- function(x) {
  str_replace(str_c(signif(x,3),"%"), "\\.", ",")
}

if(.Platform$OS.type=="windows")
  Sys.setlocale(locale = "fr_FR.utf8") else
    Sys.setlocale(locale = "fr_FR")

ccsummer <- function(n=4) PrettyCols::prettycols("Summer", n=n)
ccjoy <- function(n=4) PrettyCols::prettycols("Joyful", n=n)

bluish <- ccjoy()[1]
redish <- ccjoy()[2]
yelish <- ccsummer()[2]
greenish <- ccsummer()[4]
darkgreenish <- ccsummer()[3]
darkbluish <- ccjoy()[4]

pays_long <- c(FRA = "France", EUZ = "Zone euro", DEU = "Allemagne", ESP = "Espagne", GBR = "Royaume-Uni", USA = "Etats-Unis d'Amérique",
               BRA = "Brésil", CHI = "Chine", PECO = "Pays d'Europe centrale et orientale", NLD = "Pays-Bas", CHE = "Suisse",
               NOR = "Norvège", GRC = "Grèce", SWE  = "Suède", ITA = "Italie", AUT = "Autriche", FIN = "Finlande", AUS = "Australie",
               BEL  = "Belgique", DEN = "Danemark", PRT = "Portugal", CAN ="Canada", MEX = "Mexique", IND = "Inde", JPN= "Japon")

date_trim <- function(date) {
  str_c("T", lubridate::quarter(date), " ", lubridate::year(date))
}

date_mois <- function(date) {
  str_c(lubridate::month(date,label = TRUE, abbr = FALSE), " ", lubridate::year(date))
}

date_jour <- function(date) {
  str_c(lubridate::day(date), " ", lubridate::month(date,label = TRUE, abbr = FALSE), " ", lubridate::year(date))
}

conflicted::conflict_prefer_all("dplyr", quiet = TRUE)
conflicted::conflicts_prefer(lubridate::year, .quiet = TRUE)
conflicted::conflicts_prefer(lubridate::month, .quiet = TRUE)
conflicted::conflicts_prefer(lubridate::quarter, .quiet = TRUE)

date1 <- function(x) {
  fnan <- which(!is.na(x)) |> min()
  r <- stringr::str_sub(x, 3)
  r[[fnan]] <- stringr::str_c(x[fnan])
  return(r)
}
