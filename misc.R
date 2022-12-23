uid <- function(length = 10) {
  paste(sample(letters, length, replace = T), collapse = "")
}

selector <- function(id = NULL, class = NULL) {
  s <- ""
  if (!is.null(id)) s <- paste(s, "#", id, sep = "")
  if (!is.null(class)) s <- paste(s, ".", class, sep = "")
  s
}

button <- function(...) {
  args <- list(...)
  args$class <- "btn btn-default"
  do.call(tags$button, args)
}

modal <- function(..., id = NULL, class = NULL, header = NULL, footer = NULL) {
  div(class = "modal", id = id, class = class,
      div(class = "modal-dialog modal-dialog-scrollable",
          div(class = "modal-content",
              div(class = "modal-header", header),
              div(class = "modal-body", ...),
              div(class = "modal-footer modal-footer-left", footer))))
}
