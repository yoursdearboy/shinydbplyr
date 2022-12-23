selectUI <- function(id) {
  ns <- NS(id)
  checkboxGroupInput(ns("columns"), label = NULL)
}

selectServer <- function(id, query, .init = NULL) {
  inited <- FALSE
  moduleServer(id, function(input, output, session) {
    # this variable needed to be able to update selected columns
    # on query change
    # cause observe below doesn't update it
    buffer <- reactiveVal()
    observeEvent(input$columns, {
      if (!inited) {
        inited <<- TRUE
        value <- .init
        if (is.null(value)) {
          value <- colnames(query())
        }
        buffer(value)
      } else {
        buffer(input$columns)
      }
    }, ignoreNULL = FALSE)

    # this variable needed to get old choice list
    # to detect new choices
    # and select them automatically
    cache <- reactiveVal()
  
    observe({
      query <- query()

      values <- colnames(query)
      names <- map_chr(values, ~ pluck(query, attr_getter("meta"), "columns", ., "label", .default = .))

      isolate({
        selected <- buffer()

        values_old <- cache()
        if (!is.null(values_old)) {
          values_extra <- values[! values %in% values_old]
          selected <- c(selected, values_extra)
        }

        selected <- if (!is.null(selected)) {
          selected[selected %in% values]
        }

        updateCheckboxGroupInput(session, "columns", choiceValues = values, choiceNames = names, selected = selected)
        buffer(selected)
        cache(values)
      })
    })

    reactive({
      query <- query()

      columns <- buffer()
      if (length(columns) == 0) {
        columns <- colnames(query)
      }

      query %>%
        select(all_of(columns))
    })
  })
}
