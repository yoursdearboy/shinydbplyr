source("misc.R")
source("meta.R")
source("join.R")
source("pivot.R")
source("select.R")

decode <- function(df) {
  columns <- pluck(df, attr_getter("meta"), "columns", .default = list())
  columns <- compact(columns, "levels")
  reduce(names(columns), function(acc, name) {
    column <- columns[[name]]
    levels <- column$levels
    mutate(acc, across(all_of(name), ~ recode(., !!!levels)))
  }, .init = df)
}

queryUI <- function(id) {
  ns <- NS(id)
  tagList(
    modal(id = ns("join-modal"), class = "join-modal",
          header = h4("Join data"),
          joinUI(ns("join")),
          footer = actionButton(ns("doJoin"), "Update", "data-dismiss" = "modal", class = "btn-primary")),
    modal(id = ns("pivot-modal"), class = "pivot-modal",
          header = h4("Pivot"),
          pivotUI(ns("pivot")),
          footer = actionButton(ns("doPivot"), "Update", "data-dismiss" = "modal", class = "btn-primary")),
    modal(id = ns("select-modal"), class = "select-modal",
          header = h4("Select columns"),
          selectUI(ns("select")),
          footer = actionButton(ns("doSelect"), "Update", "data-dismiss" = "modal", class = "btn-primary")),
    div(button("Join", "data-toggle" = "modal", "data-target" = selector(id = ns("join-modal"))),
        button("Pivot", "data-toggle" = "modal", "data-target" = selector(id = ns("pivot-modal"))),
        button("Select", "data-toggle" = "modal", "data-target" = selector(id = ns("select-modal"))),
        actionButton(ns("doRefresh"), "Refresh", class = "btn-primary"),
        style = "margin-bottom: 15px;"),
    div(DT::dataTableOutput(ns("data"))))
}

queryServer <- function(id, base, queries = list(), .init = list()) {
  moduleServer(id, function(input, output, session) {
    join <- joinServer("join", base = base, queries = queries, .init = .init$join)
    pivot <- pivotServer("pivot", .init = .init$pivot)
    query <- reactive({
      input$doJoin
      input$doPivot
      input$doRefresh

      isolate({
        base %>%
          join() %>%
          pivot()
      })
    })
    selection <- selectServer("select", query, .init = .init$select)

    output$data <- DT::renderDataTable({
      query()
      input$doSelect

      isolate({
        selection <- selection()
        data <- collect(selection)
        attr(data, "meta") <- attr(selection, "meta")
        data <- decode(data)
        colnames <- map_chr(colnames(selection), ~ pluck(query(), attr_getter("meta"), "columns", ., "label", .default = .))
        DT::datatable(
          data,
          rownames = NULL,
          colnames = colnames,
          class = "table table-bordered display nowrap",
          options = list(
            dom = "t",
            paging = FALSE,
            scrollX = TRUE,
            scrollY = "90vh"
          )
        )
      })
    })

    selection
  })
}


# Debug -------------------------------------------------------------------

# ui <- basicPage(
#   tags$head(
#     tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
#   ),
#   div(queryUI("demo"),
#       style = "margin-top: 15px")
# )
# 
# server <- function(input, output) {
#   demo <- map(demo, reactiveVal)
#   queryServer("demo", demo$patient(), queries = demo, .init = list(
#     select = c("record_id", "patient_family_name", "diag.diagnosis", "diag.diagnosis_other", "diag.patient_risk_group"),
#     join = list(list(target = "diag", as = "diag")),
#     pivot = FALSE
#   ))
# }
# 
# shinyApp(ui = ui, server = server)
