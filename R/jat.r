.getReferenceYaml <- function(expr) {
  idx_yaml <- which(sapply(expr, function(expr) {
    any(grepl("^reactive:", expr))
  }))
  if (length(idx_yaml)) {
    yaml <- structure(
      list(
        yaml = sapply(idx_yaml, function(idx) expr[[idx]]),
        index = idx_yaml
       ),
      class = c("ReactiveReferenceYaml", "list")
    )
  } else {
    yaml <- structure(
      list(yaml = character(), index = character()),
      class = c("ReactiveReferenceYaml", "list")
    )
  }
}
# yaml <- .getReferenceYaml(expr = expr)
.parseYaml <- function(yaml) {
  nms <- vector("character", length(yaml))
  yaml_parsed <- lapply(seq(along=yaml), function(ii) {
    out <- yaml.load(yaml[ii])[[1]]
    if (is.null(out$where)) {
      out$where <- as.name("where")
    } else {
      out$where <- as.name(out$where)
    }
    if (is.null(out$as)) {
      out$as <- as.name(out$id)
    } else {
      out$as <- as.name(out$as)
    }
    nms[[ii]] <<- out$id
    out
  })
  names(yaml_parsed) <- nms
  yaml_parsed
}
# yaml_parsed <- .parseYaml(yaml = yaml)
.constructGetExpressionFromYaml <- function(yaml, where) {
  yaml_parsed <- .parseYaml(yaml = yaml)
  expr_get <- lapply(yaml_parsed, function(el) {
    substitute(AS <- get(x = ID, envir = WHERE, inherits = FALSE),
               list(AS = el$as, ID = el$id, WHERE = eval(el$where))
               )
  })
  expr_get
}
.computeObjectUids <- function(yaml_parsed, where) {
  sapply(yaml_parsed, function(el) {
    digest::digest(list(
      id = el$id,
      where = capture.output(eval(el$where))
      ## --> very important to use `capture.output` instead of the
      ## environment itself as changes in the environment will be
      ## recognized by `digest()` and thus would lead to a different
      ## UID being assigned each time
    ))
  })
}
.updateReactiveExpression <- function(expr, expr_add, idx_yaml) {
  for (ii in seq(along=idx_yaml)) {
    expr[[idx_yaml[ii]]] <- expr_add[[ii]]
  }
  expr
}
.processReferenceYaml <- function(expr, where) {
  yaml <- .getReferenceYaml(expr = expr)
  if (length(yaml$yaml)) {
    yaml_parsed <- .parseYaml(yaml = yaml$yaml)
    .computeObjectUids(yaml_parsed, where = where)
    expr_add <- .constructGetExpressionFromYaml(yaml = yaml$yaml, where = where)

    out <- .updateReactiveExpression(
      expr = expr,
      expr_add = expr_add,
      idx_yaml = yaml$index
    )
  } else {
    out <- expr
  }
  out
}
.constructGetChecksumExpressionFromYaml <- function(yaml, where) {
  yaml_parsed <- .parseYaml(yaml = yaml)
  expr <- lapply(yaml_parsed, function(el) {
    uid <- .computeObjectUid(id = el$id, where = eval(el$where))
    expr <- substitute(get(
        x = UID,
        envir = getOption("shiny")$.registry,
        inherits = FALSE
      )$checksum,
      list(UID = uid)
    )
    list(uid = uid, id = el$id, as = el$as, where = el$where, expr = expr)
  })
  names(expr) <- sapply(yaml_parsed, "[[", "as")
  expr
}
.computeObjectUid <- function(id, where) {
  eval(substitute(digest::digest(list(id = ID, where = WHERE)),
    list(
      ID = id,
      WHERE = capture.output(eval(where))
      ## --> very important to use `capture.output` instead of the
      ## environment itself as changes in the environment will be
      ## recognized by `digest()` and thus would lead to a different
      ## UID being assigned each time
    )
  ))
}
