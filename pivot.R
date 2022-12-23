pivot <- function(query, names_vary = "fastest") {
  indexed <- query %>%
    group_by(record_id) %>%
    mutate(i = row_number()) %>%
    ungroup()
  width <- indexed %>% summarise(i = max(i)) %>% pull(i) %>% as.numeric()

  result <- pivot_wider(indexed,
                        names_from = i,
                        values_from = -c(1, i),
                        names_vary = names_vary)

  meta <- attr(query, "meta")
  for (col in names(meta$columns)) {
    for (i in seq(width)) {
      key <- paste(col, i, sep = '_')
      old <- meta$columns[[col]]$label
      new <- sprintf("%s (%d)", old, i)
      meta$columns[[key]] <- list(label = new)
    }
  }

  attr(result, "meta") <- meta

  result
}

pivotUI <- function(id) {
  ns <- NS(id)
  checkboxInput(ns("pivot"), "Pivot?")
}

pivotServer <- function(id, query, .init = NULL) {
  inited <- FALSE
  moduleServer(id, function(input, output, session) {
    buffer <- reactiveVal()
    observeEvent(input$pivot, {
      if (!inited) {
        inited <<- TRUE
        value <- .init
        if (is.null(value)) {
          value <- FALSE
        }
        updateCheckboxInput(session, "pivot", value = value)
        buffer(value)
      } else {
        buffer(input$pivot)
      }
    }, ignoreNULL = FALSE)

    function(query) {
      if (!buffer()) return(query)
      pivot(query, names_vary = "slowest")
    }
  })
}
