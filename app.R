library(shiny)
library(purrr)
library(dplyr)
library(dbplyr)
library(tidyr)

source("query.R")

appUI <- basicPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  div(navlistPanel("Queries", id = "tabs", widths = c(2,10)),
      style = "margin: 15px 15px 0 15px")
)

appServer <- function(.init = NULL) {
  server <- function(input, output) {
    queries <- list()
    iwalk(.init, function(base, id) {
      label <- pluck(base, attr_getter("meta"), "label")
      queries[[id]] <<- queryServer(id, base, queries = queries)
      insertTab("tabs", tabPanel(label, queryUI(id)))
    })
  }
}
