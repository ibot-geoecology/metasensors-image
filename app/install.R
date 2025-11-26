# Seznam požadovaných balíčků
required_packages <- c(
  "shiny",
  "dplyr",
  "stringr",
  "optparse",
  "lubridate",
  "purrr",
  "DT",
  "shinymanager",
  "tibble"
)

missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(missing_packages) > 0) {
  install.packages(missing_packages)
}