join <- function(lhs, rhs, by = NULL, suffix = c(".x", ".y")) {
  lhs_columns <- pluck(lhs, attr_getter("meta"), "columns", .default = list())
  rhs_columns <- pluck(rhs, attr_getter("meta"), "columns", .default = list())

  dup <- setdiff(intersect(colnames(lhs), colnames(rhs)), by)
  if (length(dup) > 0) {
    dedup_x <- set_names(paste(dup, suffix[1], sep = ""), dup)
    dedup_y <- set_names(paste(dup, suffix[2], sep = ""), dup)

    names(lhs) <- recode(names(lhs), !!!dedup_x)
    names(rhs) <- recode(names(rhs), !!!dedup_y)

    if (length(lhs_columns) > 0) names(lhs_columns) <- recode(names(lhs_columns), !!!dedup_x)
    if (length(rhs_columns) > 0) names(rhs_columns) <- recode(names(rhs_columns), !!!dedup_y)
  }

  result <- left_join(lhs, rhs, by = by)
  attr(result, "meta") <- list(
    columns = c(lhs_columns, rhs_columns)
  )

  result
}

joinUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(id = ns("list")),
    div(actionButton(ns("add"), "Add join", class = "btn-primary"))
  )
}

joinServer <- function(id, base, queries, .init = NULL) {
  inited <- FALSE

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    ids <- reactiveVal(c())

    addId <- function(id) {
      old <- ids()
      new <- c(old, id)
      ids(new)
    }

    removeId <- function(id) {
      old <- ids()
      new <- old[old != id]
      ids(new)
    }

    addJoin <- function(.init = list()) {
      targets <- names(queries)
      names(targets) <- imap_chr(queries, function(q, name) {
        pluck(q(), attr_getter("meta"), "label", .default = name)
      })

      bys <- colnames(base)
      names(bys) <- map_chr(bys, function(f) {
        pluck(base, attr_getter("meta"), "columns", f, "label", .default = f)
      })

      id <- uid()
      id_target <- paste(id, "target", sep = "-")
      id_as <- paste(id, "as", sep = "-")
      id_by <- paste(id, "by", sep = "-")
      id_delete <- paste(id, "delete", sep = "-")
      id_container <- paste(id, "container", sep = "-")

      if (is.null(.init$by)) .init$by <- bys[1]

      target <- selectInput(ns(id_target), NULL, choices = c("", targets), selected = .init$target)
      by <- selectInput(ns(id_by), NULL, choices = bys, selected = .init$by)
      as <- textInput(ns(id_as), NULL, value = .init$as)
      delete <- actionButton(ns(id_delete), "x")
      container <- div(id = id_container,
                       div(target, class = "join-target"),
                       div(by,     class = "join-by"),
                       div(as,     class = "join-as"),
                       div(delete, class = "join-delete"))
      insertUI(selector(id = ns("list")), "beforeEnd", container)

      addId(id)

      observeEvent(input[[id_delete]], {
        removeUI(selector(id = id_container))
        removeId(id)
      })
    }

    observeEvent(input$add, addJoin())

    values <- eventReactive(ids(), {
      # FIXME: Find better way to initialize this value
      # It's value depends on inputs inserted using addJoin,
      # but for some reason these inputs are not in `input`
      # before any user interaction.
      # Therefore, just return these initial values.
      if (!inited) {
        inited <<- TRUE
        walk(.init, ~ addJoin(.))
        return(.init)
      }

      map(ids(), function(id) {
        id_target <- paste(id, "target", sep = "-")
        id_by <- paste(id, "by", sep = "-")
        id_as <- paste(id, "as", sep = "-")
        target <- input[[id_target]]
        by <- input[[id_by]]
        as <- input[[id_as]]
        if (is.null(target) || target == "") return()
        if (is.null(by) || by == "") return()
        list(
          target = target,
          by = by,
          as = as
        )
      })
    }, ignoreNULL = FALSE)

    function(lhs) {
      reduce(values(), function(lhs, value) {
        if (is.null(value)) return(lhs)

        rhs <- queries[[value$target]]()
        by <- value$by
        as <- value$as

        join(lhs, rhs, by)
      }, .init = lhs)
    }
  })
}
