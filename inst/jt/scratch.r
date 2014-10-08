options(shiny.suppressMissingContextError=TRUE)

rm(x_1)
rm(x_2)

## Dependee //
x_1 <- 10
# makeReactiveBinding("x_1")
makeReactiveBinding2("x_1")

attributes(values2)
values2$
## Dependent //



x_2 <- reactive2(x_3 + 10)
x_2
this <- attributes(x_2)$observable
this$getValue
attributes(x_2)$observable$.hash
ls(attributes(x_2)$observable$.dependents)
## --> function 'register()' is called inside the '.getValue()' function
## This must have something to do with registering the dependee

attributes(x_2)$observable$.dependents$register
this <- attributes(x_2)$observable$.dependents

## Register function //
## Belongs to 'Dependents'
this2$register

register2 <- function(depId=NULL, depLabel=NULL) {
#   ls(.getReactiveEnvironment())
  ctx <- .getReactiveEnvironment()$currentContext()
  ls(ctx)
  ctx$id
  this2$.dependents
  if (!this2$.dependents$containsKey(ctx$id)) {
    this2$.dependents$set(ctx$id, ctx)
    ctx$onInvalidate(function() {
      this2$.dependents$remove(ctx$id)
    })

    if (!is.null(depId) && nchar(depId) > 0)
      .graphDependsOnId(ctx$id, depId)
    if (!is.null(depLabel))
      .graphDependsOn(ctx$id, depLabel)
  }
}

this$getValue

## Update value function //
## Belongs to 'Observable'
self <- this
this$.updateValue()
this$.domain
this$.label

function() {
  ctx <- Context$new(self$.domain, self$.label, type = 'observable',
                     prevId = self$.mostRecentCtxId)
#   ls(ctx)
#   ctx$id
  .mostRecentCtxId <<- ctx$id
  ctx$onInvalidate(function() {
    self$.invalidated <<- TRUE
    self$.dependents$invalidate()
  })
#   self$.execCount <<- self$.execCount + 1L
  self$.execCount <- self$.execCount + 1L

#   self$.invalidated <<- FALSE
  self$.invalidated <- FALSE
# id=ctx$id
  wasRunning <- self$.running
#   self$.running <<- TRUE
  self$.running <- TRUE
  on.exit(.running <<- wasRunning)

  func <- function() {
    result <- withVisible(try(shinyCallingHandlers(self$.func()), silent=TRUE))
    .visible <<- result$visible
    .value <<- result$value
  }
  ctx$run <- function(func) {
      "Run the provided function under this context."
      withReactiveDomain(.domain, {
        env <- .getReactiveEnvironment()
        ls(env)
        .graphEnterContext(id)
        tryCatch(
          env$runWith(self, func),
          finally = .graphExitContext(id)
        )
      })
    }

  ctx$run(function() {
    result <- withVisible(try(shinyCallingHandlers(self$.func()), silent=TRUE))
    .visible <<- result$visible
    .value <<- result$value
  })
}


##------------------------------------------------------------------------------

## Classes involved:
## - Observable
## - Dependents
## - Map

##------------------------------------------------------------------------------

## Changes //
## - makeReactiveBinding2 --> reactiveValues2 -- .createReactiveValues2
## --> ReactiveValues2

## Most curious thing I've seen:
## In 'ReactiveValues2' part where 'value' and 'value_hash' are set via
## '.graphValueChange()':
##    .graphValueChange(sprintf('%s$%s', .label, key), value)
##    .graphValueChange(sprintf('%s$%s', .label, key_hash), value_hash)
## I don't really understand how this actually works
