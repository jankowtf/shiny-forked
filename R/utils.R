#' @include globals.R
#' @include map.R
NULL

#' Make a random number generator repeatable
#'
#' Given a function that generates random data, returns a wrapped version of
#' that function that always uses the same seed when called. The seed to use can
#' be passed in explicitly if desired; otherwise, a random number is used.
#'
#' @param rngfunc The function that is affected by the R session's seed.
#' @param seed The seed to set every time the resulting function is called.
#' @return A repeatable version of the function that was passed in.
#'
#' @note When called, the returned function attempts to preserve the R session's
#'   current seed by snapshotting and restoring
#'   \code{\link[base]{.Random.seed}}.
#'
#' @examples
#' rnormA <- repeatable(rnorm)
#' rnormB <- repeatable(rnorm)
#' rnormA(3)  # [1]  1.8285879 -0.7468041 -0.4639111
#' rnormA(3)  # [1]  1.8285879 -0.7468041 -0.4639111
#' rnormA(5)  # [1]  1.8285879 -0.7468041 -0.4639111 -1.6510126 -1.4686924
#' rnormB(5)  # [1] -0.7946034  0.2568374 -0.6567597  1.2451387 -0.8375699
#'
#' @export
repeatable <- function(rngfunc, seed = runif(1, 0, .Machine$integer.max)) {
  force(seed)

  function(...) {
    # When we exit, restore the seed to its original state
    if (exists('.Random.seed', where=globalenv())) {
      currentSeed <- get('.Random.seed', pos=globalenv())
      on.exit(assign('.Random.seed', currentSeed, pos=globalenv()))
    }
    else {
      on.exit(rm('.Random.seed', pos=globalenv()))
    }

    set.seed(seed)

    rngfunc(...)
  }
}

# Temporarily set x in env to value, evaluate expr, and
# then restore x to its original state
withTemporary <- function(env, x, value, expr, unset = FALSE) {

  if (exists(x, envir = env, inherits = FALSE)) {
    oldValue <- get(x, envir = env, inherits = FALSE)
    on.exit(
      assign(x, oldValue, envir = env, inherits = FALSE),
      add = TRUE)
  } else {
    on.exit(
      rm(list = x, envir = env, inherits = FALSE),
      add = TRUE
    )
  }

  if (!missing(value) && !isTRUE(unset))
    assign(x, value, envir = env, inherits = FALSE)
  else {
    if (exists(x, envir = env, inherits = FALSE))
      rm(list = x, envir = env, inherits = FALSE)
  }
  force(expr)
}

.globals$ownSeed <- NULL
# Evaluate an expression using Shiny's own private stream of
# randomness (not affected by set.seed).
withPrivateSeed <- function(expr) {
  withTemporary(.GlobalEnv, ".Random.seed",
    .globals$ownSeed, unset=is.null(.globals$ownSeed), {
      tryCatch({
        expr
      }, finally = {
        .globals$ownSeed <- getExists('.Random.seed', 'numeric', globalenv())
      })
    }
  )
}

# a homemade version of set.seed(NULL) for backward compatibility with R 2.15.x
reinitializeSeed <- if (getRversion() >= '3.0.0') {
  function() set.seed(NULL)
} else function() {
  if (exists('.Random.seed', globalenv()))
    rm(list = '.Random.seed', pos = globalenv())
  stats::runif(1)  # generate any random numbers so R can reinitialize the seed
}

# Version of runif that runs with private seed
p_runif <- function(...) {
  withPrivateSeed(runif(...))
}

# Version of sample that runs with private seed
p_sample <- function(...) {
  withPrivateSeed(sample(...))
}

# Return a random integral value in the range [min, max).
# If only one argument is passed, then min=0 and max=argument.
randomInt <- function(min, max) {
  if (missing(max)) {
    max <- min
    min <- 0
  }
  if (min < 0 || max <= min)
    stop("Invalid min/max values")

  min + sample(max-min, 1)-1
}

p_randomInt <- function(...) {
  withPrivateSeed(randomInt(...))
}

`%OR%` <- function(x, y) {
  if (is.null(x) || isTRUE(is.na(x)))
    y
  else
    x
}

`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

`%.%` <- function(x, y) {
  paste(x, y, sep='')
}

# Given a vector or list, drop all the NULL items in it
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

nullOrEmpty <- function(x) {
  is.null(x) || length(x) == 0
}
# Given a vector or list, drop all the NULL items in it
dropNullsOrEmpty <- function(x) {
  x[!vapply(x, nullOrEmpty, FUN.VALUE=logical(1))]
}

# Combine dir and (file)name into a file path. If a file already exists with a
# name differing only by case, then use it instead.
file.path.ci <- function(...) {
  result <- find.file.ci(...)
  if (!is.null(result))
    return(result)

  # If not found, return the file path that was given to us.
  return(file.path(...))
}

# Does a particular file exist? Case-insensitive for filename, case-sensitive
# for path (on platforms with case-sensitive file system).
file.exists.ci <- function(...) {
  !is.null(find.file.ci(...))
}

# Look for a file, case-insensitive for filename, case-sensitive for path (on
# platforms with case-sensitive filesystem). If found, return the path to the
# file, with the correct case. If not found, return NULL.
find.file.ci <- function(...) {
  default <- file.path(...)
  if (length(default) > 1)
    stop("find.file.ci can only check for one file at a time.")
  if (file.exists(default))
    return(default)

  dir <- dirname(default)
  name <- basename(default)

  # If we got here, then we'll check for a directory with the exact case, and a
  # name with any case.
  all_files <- list.files(dir, all.files=TRUE, full.names=TRUE,
                          include.dirs=TRUE)
  match_idx <- tolower(name) == tolower(basename(all_files))
  matches <- all_files[match_idx]
  if (length(matches) == 0)
    return(NULL)

  return(matches[1])
}

# Attempt to join a path and relative path, and turn the result into a
# (normalized) absolute path. The result will only be returned if it is an
# existing file/directory and is a descendant of dir.
#
# Example:
# resolve("/Users/jcheng", "shiny")  # "/Users/jcheng/shiny"
# resolve("/Users/jcheng", "./shiny")  # "/Users/jcheng/shiny"
# resolve("/Users/jcheng", "shiny/../shiny/")  # "/Users/jcheng/shiny"
# resolve("/Users/jcheng", ".")  # NULL
# resolve("/Users/jcheng", "..")  # NULL
# resolve("/Users/jcheng", "shiny/..")  # NULL
resolve <- function(dir, relpath) {
  abs.path <- file.path(dir, relpath)
  if (!file.exists(abs.path))
    return(NULL)
  abs.path <- normalizePath(abs.path, winslash='/', mustWork=TRUE)
  dir <- normalizePath(dir, winslash='/', mustWork=TRUE)
  # trim the possible trailing slash under Windows (#306)
  if (isWindows()) dir <- sub('/$', '', dir)
  if (nchar(abs.path) <= nchar(dir) + 1)
    return(NULL)
  if (substr(abs.path, 1, nchar(dir)) != dir ||
      substr(abs.path, nchar(dir)+1, nchar(dir)+1) != '/') {
    return(NULL)
  }
  return(abs.path)
}

isWindows <- function() .Platform$OS.type == 'windows'

# This is a wrapper for download.file and has the same interface.
# The only difference is that, if the protocol is https, it changes the
# download settings, depending on platform.
download <- function(url, ...) {
  # First, check protocol. If http or https, check platform:
  if (grepl('^https?://', url)) {

    # If Windows, call setInternet2, then use download.file with defaults.
    if (isWindows()) {
      # If we directly use setInternet2, R CMD CHECK gives a Note on Mac/Linux
      mySI2 <- `::`(utils, 'setInternet2')
      # Store initial settings
      internet2_start <- mySI2(NA)
      on.exit(mySI2(internet2_start))

      # Needed for https
      mySI2(TRUE)
      download.file(url, ...)

    } else {
      # If non-Windows, check for curl/wget/lynx, then call download.file with
      # appropriate method.

      if (nzchar(Sys.which("wget")[1])) {
        method <- "wget"
      } else if (nzchar(Sys.which("curl")[1])) {
        method <- "curl"

        # curl needs to add a -L option to follow redirects.
        # Save the original options and restore when we exit.
        orig_extra_options <- getOption("download.file.extra")
        on.exit(options(download.file.extra = orig_extra_options))

        options(download.file.extra = paste("-L", orig_extra_options))

      } else if (nzchar(Sys.which("lynx")[1])) {
        method <- "lynx"
      } else {
        stop("no download method found")
      }

      download.file(url, method = method, ...)
    }

  } else {
    download.file(url, ...)
  }
}

knownContentTypes <- Map$new()
knownContentTypes$mset(
  html='text/html; charset=UTF-8',
  htm='text/html; charset=UTF-8',
  js='text/javascript',
  css='text/css',
  png='image/png',
  jpg='image/jpeg',
  jpeg='image/jpeg',
  gif='image/gif',
  svg='image/svg+xml',
  txt='text/plain',
  pdf='application/pdf',
  ps='application/postscript',
  xml='application/xml',
  m3u='audio/x-mpegurl',
  m4a='audio/mp4a-latm',
  m4b='audio/mp4a-latm',
  m4p='audio/mp4a-latm',
  mp3='audio/mpeg',
  wav='audio/x-wav',
  m4u='video/vnd.mpegurl',
  m4v='video/x-m4v',
  mp4='video/mp4',
  mpeg='video/mpeg',
  mpg='video/mpeg',
  avi='video/x-msvideo',
  mov='video/quicktime',
  ogg='application/ogg',
  swf='application/x-shockwave-flash',
  doc='application/msword',
  xls='application/vnd.ms-excel',
  ppt='application/vnd.ms-powerpoint',
  xlsx='application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
  xltx='application/vnd.openxmlformats-officedocument.spreadsheetml.template',
  potx='application/vnd.openxmlformats-officedocument.presentationml.template',
  ppsx='application/vnd.openxmlformats-officedocument.presentationml.slideshow',
  pptx='application/vnd.openxmlformats-officedocument.presentationml.presentation',
  sldx='application/vnd.openxmlformats-officedocument.presentationml.slide',
  docx='application/vnd.openxmlformats-officedocument.wordprocessingml.document',
  dotx='application/vnd.openxmlformats-officedocument.wordprocessingml.template',
  xlam='application/vnd.ms-excel.addin.macroEnabled.12',
  xlsb='application/vnd.ms-excel.sheet.binary.macroEnabled.12')

getContentType <- function(ext, defaultType='application/octet-stream') {
  knownContentTypes$get(tolower(ext)) %OR% defaultType
}

# Create a zero-arg function from a quoted expression and environment
# @examples
# makeFunction(body=quote(print(3)))
makeFunction <- function(args = pairlist(), body, env = parent.frame()) {
  eval(call("function", args, body), env)
}

#' Convert an expression to a function
#'
#' This is to be called from another function, because it will attempt to get
#' an unquoted expression from two calls back.
#'
#' If expr is a quoted expression, then this just converts it to a function.
#' If expr is a function, then this simply returns expr (and prints a
#'   deprecation message).
#' If expr was a non-quoted expression from two calls back, then this will
#'   quote the original expression and convert it to a function.
#
#' @param expr A quoted or unquoted expression, or a function.
#' @param env The desired environment for the function. Defaults to the
#'   calling environment two steps back.
#' @param quoted Is the expression quoted?
#' @param caller_offset If specified, the offset in the callstack of the
#'   functiont to be treated as the caller.
#'
#' @examples
#' # Example of a new renderer, similar to renderText
#' # This is something that toolkit authors will do
#' renderTriple <- function(expr, env=parent.frame(), quoted=FALSE) {
#'   # Convert expr to a function
#'   func <- shiny::exprToFunction(expr, env, quoted)
#'
#'   function() {
#'     value <- func()
#'     paste(rep(value, 3), collapse=", ")
#'   }
#' }
#'
#'
#' # Example of using the renderer.
#' # This is something that app authors will do.
#' values <- reactiveValues(A="text")
#'
#' \dontrun{
#' # Create an output object
#' output$tripleA <- renderTriple({
#'   values$A
#' })
#' }
#'
#' # At the R console, you can experiment with the renderer using isolate()
#' tripleA <- renderTriple({
#'   values$A
#' })
#'
#' isolate(tripleA())
#' # "text, text, text"
#'
#' @export
exprToFunction <- function(expr, env=parent.frame(2), quoted=FALSE,
                           caller_offset=1) {
  # Get the quoted expr from two calls back
  expr_sub <- eval(substitute(substitute(expr)), parent.frame(caller_offset))

  # Check if expr is a function, making sure not to evaluate expr, in case it
  # is actually an unquoted expression.
  # If expr is a single token, then indexing with [[ will error; if it has multiple
  # tokens, then [[ works. In the former case it will be a name object; in the
  # latter, it will be a language object.
  if (!is.null(expr_sub) && !is.name(expr_sub) && expr_sub[[1]] == as.name('function')) {
    # Get name of function that called this function
    called_fun <- sys.call(-1 * caller_offset)[[1]]

    shinyDeprecated(msg = paste("Passing functions to '", called_fun,
      "' is deprecated. Please use expressions instead. See ?", called_fun,
      " for more information.", sep=""))
    return(expr)
  }

  if (quoted) {
    # expr is a quoted expression
    makeFunction(body=expr, env=env)
  } else {
    # expr is an unquoted expression
    makeFunction(body=expr_sub, env=env)
  }
}
## [@change: jat, start]
exprToFunction2 <- function(expr, env=parent.frame(2), quoted=FALSE,
  caller_offset=1) {
  # Get the quoted expr from two calls back
  expr_sub <- eval(substitute(substitute(expr)), parent.frame(caller_offset))

  ## [@change: jat, start]
  ## Retrieve dependencies from YAML markup
  expr_sub <- .processReferenceYaml(expr = expr_sub, where = env)
#   print(expr_sub)
  ## [@change: jat, end]

  # Check if expr is a function, making sure not to evaluate expr, in case it
  # is actually an unquoted expression.
  # If expr is a single token, then indexing with [[ will error; if it has multiple
  # tokens, then [[ works. In the former case it will be a name object; in the
  # latter, it will be a language object.
  if (!is.null(expr_sub) && !is.name(expr_sub) && expr_sub[[1]] == as.name('function')) {
    # Get name of function that called this function
    called_fun <- sys.call(-1 * caller_offset)[[1]]

    shinyDeprecated(msg = paste("Passing functions to '", called_fun,
        "' is deprecated. Please use expressions instead. See ?", called_fun,
        " for more information.", sep=""))
    return(expr)
  }

  if (quoted) {
    # expr is a quoted expression
    makeFunction(body=expr, env=env)
  } else {
    # expr is an unquoted expression
    makeFunction(body=expr_sub, env=env)
#     print(tmp)
  }
}

## [@change: jat, end]
#' Install an expression as a function
#'
#' Installs an expression in the given environment as a function, and registers
#' debug hooks so that breakpoints may be set in the function.
#'
#' This function can replace \code{exprToFunction} as follows: we may use
#' \code{func <- exprToFunction(expr)} if we do not want the debug hooks, or
#' \code{installExprFunction(expr, "func")} if we do. Both approaches create a
#' function named \code{func} in the current environment.
#'
#' @seealso Wraps \code{\link{exprToFunction}}; see that method's documentation
#'   for more documentation and examples.
#'
#' @param expr A quoted or unquoted expression
#' @param name The name the function should be given
#' @param eval.env The desired environment for the function. Defaults to the
#'   calling environment two steps back.
#' @param quoted Is the expression quoted?
#' @param assign.env The environment in which the function should be assigned.
#' @param label A label for the object to be shown in the debugger. Defaults to
#'   the name of the calling function.
#'
#' @export
installExprFunction <- function(expr, name, eval.env = parent.frame(2),
                                quoted = FALSE,
                                assign.env = parent.frame(1),
                                label = as.character(sys.call(-1)[[1]])) {
  func <- exprToFunction(expr, eval.env, quoted, 2)
  assign(name, func, envir = assign.env)
  registerDebugHook(name, assign.env, label)
}

#' Parse a GET query string from a URL
#'
#' Returns a named character vector of key-value pairs.
#'
#' @param str The query string. It can have a leading \code{"?"} or not.
#' @param nested Whether to parse the query string of as a nested list when it
#'   contains pairs of square brackets \code{[]}. For example, the query
#'   \samp{a[i1][j1]=x&b[i1][j1]=y&b[i2][j1]=z} will be parsed as \code{list(a =
#'   list(i1 = list(j1 = 'x')), b = list(i1 = list(j1 = 'y'), i2 = list(j1 =
#'   'z')))} when \code{nested = TRUE}, and \code{list(`a[i1][j1]` = 'x',
#'   `b[i1][j1]` = 'y', `b[i2][j1]` = 'z')} when \code{nested = FALSE}.
#' @export
#' @examples
#' parseQueryString("?foo=1&bar=b%20a%20r")
#'
#' \dontrun{
#' # Example of usage within a Shiny app
#' shinyServer(function(input, output, clientData) {
#'
#'   output$queryText <- renderText({
#'     query <- parseQueryString(clientData$url_search)
#'
#'     # Ways of accessing the values
#'     if (as.numeric(query$foo) == 1) {
#'       # Do something
#'     }
#'     if (query[["bar"]] == "targetstring") {
#'       # Do something else
#'     }
#'
#'     # Return a string with key-value pairs
#'     paste(names(query), query, sep = "=", collapse=", ")
#'   })
#' })
#' }
#'
parseQueryString <- function(str, nested = FALSE) {
  if (is.null(str) || nchar(str) == 0)
    return(list())

  # Remove leading ?
  if (substr(str, 1, 1) == '?')
    str <- substr(str, 2, nchar(str))

  pairs <- strsplit(str, '&', fixed = TRUE)[[1]]
  pairs <- strsplit(pairs, '=', fixed = TRUE)

  keys   <- vapply(pairs, function(x) x[1], FUN.VALUE = character(1))
  values <- vapply(pairs, function(x) x[2], FUN.VALUE = character(1))
  # Replace NA with '', so they don't get converted to 'NA' by URLdecode
  values[is.na(values)] <- ''

  # Convert "+" to " ", since URLdecode doesn't do it
  keys   <- gsub('+', ' ', keys,   fixed = TRUE)
  values <- gsub('+', ' ', values, fixed = TRUE)

  keys   <- vapply(keys,   URLdecode, character(1), USE.NAMES = FALSE)
  values <- vapply(values, URLdecode, character(1), USE.NAMES = FALSE)

  res <- setNames(as.list(values), keys)
  if (!nested) return(res)

  # Make a nested list from a query of the form ?a[1][1]=x11&a[1][2]=x12&...
  for (i in grep('\\[.+\\]', keys)) {
    k <- strsplit(keys[i], '[][]')[[1L]]  # split by [ or ]
    res <- assignNestedList(res, k[k != ''], values[i])
    res[[keys[i]]] <- NULL    # remove res[['a[1][1]']]
  }
  res
}

# Assign value to the bottom element of the list x using recursive indices idx
assignNestedList <- function(x = list(), idx, value) {
  for (i in seq_along(idx)) {
    sub <- idx[seq_len(i)]
    if (is.null(x[[sub]])) x[[sub]] <- list()
  }
  x[[idx]] <- value
  x
}

# decide what to do in case of errors; it is customizable using the shiny.error
# option (e.g. we can set options(shiny.error = recover))
shinyCallingHandlers <- function(expr) {
  withCallingHandlers(expr, error = function(e) {
    handle <- getOption('shiny.error')
    if (is.function(handle)) handle()
  })
}

#' Print message for deprecated functions in Shiny
#'
#' To disable these messages, use \code{options(shiny.deprecation.messages=FALSE)}.
#'
#' @param new Name of replacement function.
#' @param msg Message to print. If used, this will override the default message.
#' @param old Name of deprecated function.
shinyDeprecated <- function(new=NULL, msg=NULL,
                            old=as.character(sys.call(sys.parent()))[1L]) {

  if (getOption("shiny.deprecation.messages") %OR% TRUE == FALSE)
    return(invisible())

  if (is.null(msg)) {
    msg <- paste(old, "is deprecated.")
    if (!is.null(new))
      msg <- paste(msg, "Please use", new, "instead.",
        "To disable this message, run options(shiny.deprecation.messages=FALSE)")
  }
  # Similar to .Deprecated(), but print a message instead of warning
  message(msg)
}

#' Register a function with the debugger (if one is active).
#'
#' Call this function after exprToFunction to give any active debugger a hook
#' to set and clear breakpoints in the function. A debugger may implement
#' registerShinyDebugHook to receive callbacks when Shiny functions are
#' instantiated at runtime.
#'
#' @param name Name of the field or object containing the function.
#' @param where The reference object or environment containing the function.
#' @param label A label to display on the function in the debugger.
#' @noRd
registerDebugHook <- function(name, where, label) {
  if (exists("registerShinyDebugHook", mode = "function")) {
    registerShinyDebugHook <- get("registerShinyDebugHook", mode = "function")
    params <- new.env(parent = emptyenv())
    params$name <- name
    params$where <- where
    params$label <- label
    registerShinyDebugHook(params)
  }
}

Callbacks <- R6Class(
  'Callbacks',
  portable = FALSE,
  class = FALSE,
  public = list(
    .nextId = integer(0),
    .callbacks = 'Map',

    initialize = function() {
      .nextId <<- as.integer(.Machine$integer.max)
      .callbacks <<- Map$new()
    },
    register = function(callback) {
      id <- as.character(.nextId)
      .nextId <<- .nextId - 1L
      .callbacks$set(id, callback)
      return(function() {
        .callbacks$remove(id)
      })
    },
    invoke = function(..., onError=NULL) {
      for (callback in .callbacks$values()) {
        if (is.null(onError)) {
          callback(...)
        } else {
          tryCatch(callback(...), error = onError)
        }
      }
    },
    count = function() {
      .callbacks$size()
    }
  )
)

# convert a data frame to JSON as required by DataTables request
dataTablesJSON <- function(data, req) {
  n <- nrow(data)
  q <- parseQueryString(req$QUERY_STRING, nested = TRUE)
  ci <- q$search[['caseInsensitive']] == 'true'

  # global searching
  i <- seq_len(n)
  if (q$search[['value']] != '') {
    i0 <- apply(data, 2, function(x) {
      grep2(q$search[['value']], as.character(x),
            fixed = q$search[['regex']] == 'false', ignore.case = ci)
    })
    i <- intersect(i, unique(unlist(i0)))
  }

  # search by columns
  if (length(i)) for (j in names(q$columns)) {
    col <- q$columns[[j]]
    # if the j-th column is not searchable or the search string is "", skip it
    if (col[['searchable']] != 'true') next
    if ((k <- col[['search']][['value']]) == '') next
    j <- as.integer(j)
    dj <- data[, j + 1]
    r  <- commaToRange(k)
    ij <- if (length(r) == 2 && is.numeric(dj)) {
      which(dj >= r[1] & dj <= r[2])
    } else {
      grep2(k, as.character(dj), fixed = col[['search']][['regex']] == 'false',
            ignore.case = ci)
    }
    i <- intersect(ij, i)
    if (length(i) == 0) break
  }
  if (length(i) != n) data <- data[i, , drop = FALSE]

  # sorting
  oList <- list()
  for (ord in q$order) {
    k <- ord[['column']]  # which column to sort
    d <- ord[['dir']]     # direction asc/desc
    if (q$columns[[k]][['orderable']] != 'true') next
    col <- data[, as.integer(k) + 1]
    oList[[length(oList) + 1]] <- (if (d == 'asc') identity else `-`)(
      if (is.numeric(col)) col else xtfrm(col)
    )
  }
  if (length(oList)) {
    i <- do.call(order, oList)
    data <- data[i, , drop = FALSE]
  }
  # paging
  if (q$length != '-1') {
    i <- seq(as.integer(q$start) + 1L, length.out = as.integer(q$length))
    i <- i[i <= nrow(data)]
    fdata <- data[i, , drop = FALSE]  # filtered data
  } else fdata <- data

  fdata <- unname(as.matrix(fdata))
  # WAT: toJSON(list(x = matrix(nrow = 0, ncol = 1))) => {"x": } (#299)
  if (nrow(fdata) == 0) fdata <- list()
  # WAT: toJSON(list(x = matrix(1:2))) => {x: [ [1], [2] ]}, however,
  # toJSON(list(x = matrix(1))) => {x: [ 1 ]} (loss of dimension, #429)
  if (length(fdata) && all(dim(fdata) == 1)) fdata <- list(list(fdata[1, 1]))

  res <- toJSON(list(
    draw = q$draw,
    recordsTotal = n,
    recordsFiltered = nrow(data),
    data = fdata
  ))
  httpResponse(200, 'application/json', res)
}

# when both ignore.case and fixed are TRUE, we use grep(ignore.case = FALSE,
# fixed = TRUE) to do lower-case matching of pattern on x
grep2 <- function(pattern, x, ignore.case = FALSE, fixed = FALSE, ...) {
  if (fixed && ignore.case) {
    pattern <- tolower(pattern)
    x <- tolower(x)
    ignore.case <- FALSE
  }
  # when the user types in the search box, the regular expression may not be
  # complete before it is sent to the server, in which case we do not search
  if (!fixed && inherits(try(grep(pattern, ''), silent = TRUE), 'try-error'))
    return(seq_along(x))
  grep(pattern, x, ignore.case = ignore.case, fixed = fixed, ...)
}

getExists <- function(x, mode, envir = parent.frame()) {
  if (exists(x, envir = envir, mode = mode, inherits = FALSE))
    get(x, envir = envir, mode = mode, inherits = FALSE)
}

# convert a string of the form "lower,upper" to c(lower, upper)
commaToRange <- function(string) {
  if (!grepl(',', string)) return()
  r <- strsplit(string, ',')[[1]]
  if (length(r) > 2) return()
  if (length(r) == 1) r <- c(r, '')  # lower,
  r <- as.numeric(r)
  if (is.na(r[1])) r[1] <- -Inf
  if (is.na(r[2])) r[2] <- Inf
  r
}

# for options passed to DataTables/Selectize/..., the options of the class AsIs
# will be evaluated as literal JavaScript code
checkAsIs <- function(options) {
  evalOptions <- if (length(options)) {
    nms <- names(options)
    if (length(nms) == 0L || any(nms == '')) stop("'options' must be a named list")
    i <- unlist(lapply(options, function(x) {
      is.character(x) && inherits(x, 'AsIs')
    }))
    if (any(i)) {
      # must convert to character, otherwise toJSON() turns it to an array []
      options[i] <- lapply(options[i], paste, collapse = '\n')
      nms[i]  # options of these names will be evaluated in JS
    }
  }
  list(options = options, eval = evalOptions)
}

srcrefFromShinyCall <- function(expr) {
  srcrefs <- attr(expr, "srcref")
  num_exprs <- length(srcrefs)
  if (num_exprs < 1)
    return(NULL)
  c(srcrefs[[1]][1], srcrefs[[1]][2],
    srcrefs[[num_exprs]][3], srcrefs[[num_exprs]][4],
    srcrefs[[1]][5], srcrefs[[num_exprs]][6])
}

# Indicates whether the given querystring should cause the associated request
# to be handled in showcase mode. Returns the showcase mode if set, or NULL
# if no showcase mode is set.
showcaseModeOfQuerystring <- function(querystring) {
  if (nchar(querystring) > 0) {
    qs <- parseQueryString(querystring)
    if (exists("showcase", where = qs)) {
      return(as.numeric(qs$showcase))
    }
  }
  return(NULL)
}

showcaseModeOfReq <- function(req) {
  showcaseModeOfQuerystring(req$QUERY_STRING)
}

# Returns (just) the filename containing the given source reference, or an
# empty string if the source reference doesn't include file information.
srcFileOfRef <- function(srcref) {
  fileEnv <- attr(srcref, "srcfile")
  # The 'srcfile' attribute should be a non-null environment containing the
  # variable 'filename', which gives the full path to the source file.
  if (!is.null(fileEnv) &&
      is.environment(fileEnv) &&
      exists("filename", where = fileEnv))
    basename(fileEnv[["filename"]])
  else
    ""
}

# Format a number without sci notation, and keep as many digits as possible (do
# we really need to go beyond 15 digits?)
formatNoSci <- function(x) {
  if (is.null(x)) return(NULL)
  format(x, scientific = FALSE, digits = 15)
}

# Returns a function that calls the given func and caches the result for
# subsequent calls, unless the given file's mtime changes.
cachedFuncWithFile <- function(dir, file, func, case.sensitive = FALSE) {
  dir <- normalizePath(dir, mustWork=TRUE)
  mtime <- NA
  value <- NULL
  function(...) {
    fname <- if (case.sensitive)
      file.path(dir, file)
    else
      file.path.ci(dir, file)

    now <- file.info(fname)$mtime
    if (!identical(mtime, now)) {
      value <<- func(fname, ...)
      mtime <<- now
    }
    value
  }
}

# turn column-based data to row-based data (mainly for JSON), e.g. data.frame(x
# = 1:10, y = 10:1) ==> list(list(x = 1, y = 10), list(x = 2, y = 9), ...)
columnToRowData <- function(data) {
  do.call(
    mapply, c(
      list(FUN = function(...) list(...), SIMPLIFY = FALSE, USE.NAMES = FALSE),
      as.list(data)
    )
  )
}

#' Validate input values and other conditions
#'
#' For an output rendering function (e.g. \code{\link{renderPlot}()}), you may
#' need to check that certain input values are available and valid before you
#' can render the output. \code{validate} gives you a convenient mechanism for
#' doing so.
#'
#' The \code{validate} function takes any number of (unnamed) arguments, each of
#' which represents a condition to test. If any of the conditions represent
#' failure, then a special type of error is signaled which stops execution. If
#' this error is not handled by application-specific code, it is displayed to
#' the user by Shiny.
#'
#' An easy way to provide arguments to \code{validate} is to use the \code{need}
#' function, which takes an expression and a string; if the expression is
#' considered a failure, then the string will be used as the error message. The
#' \code{need} function considers its expression to be a failure if it is any of
#' the following:
#'
#' \itemize{
#'   \item{\code{FALSE}}
#'   \item{\code{NULL}}
#'   \item{\code{""}}
#'   \item{An empty atomic vector}
#'   \item{An atomic vector that contains only missing values}
#'   \item{A logical vector that contains all \code{FALSE} or missing values}
#'   \item{An object of class \code{"try-error"}}
#'   \item{A value that represents an unclicked \code{\link{actionButton}}}
#' }
#'
#' If any of these values happen to be valid, you can explicitly turn them to
#' logical values. For example, if you allow \code{NA} but not \code{NULL}, you
#' can use the condition \code{!is.null(input$foo)}, because \code{!is.null(NA)
#' == TRUE}.
#'
#' If you need validation logic that differs significantly from \code{need}, you
#' can create other validation test functions. A passing test should return
#' \code{NULL}. A failing test should return an error message as a
#' single-element character vector, or if the failure should happen silently,
#' \code{FALSE}.
#'
#' Because validation failure is signaled as an error, you can use
#' \code{validate} in reactive expressions, and validation failures will
#' automatically propagate to outputs that use the reactive expression. In
#' other words, if reactive expression \code{a} needs \code{input$x}, and two
#' outputs use \code{a} (and thus depend indirectly on \code{input$x}), it's
#' not necessary for the outputs to validate \code{input$x} explicitly, as long
#' as \code{a} does validate it.
#'
#' @param ... A list of tests. Each test should equal \code{NULL} for success,
#'   \code{FALSE} for silent failure, or a string for failure with an error
#'   message.
#' @param errorClass A CSS class to apply. The actual CSS string will have
#'   \code{shiny-output-error-} prepended to this value.
#' @export
#' @examples
#' # in ui.R
#' fluidPage(
#'   checkboxGroupInput('in1', 'Check some letters', choices = head(LETTERS)),
#'   selectizeInput('in2', 'Select a state', choices = state.name),
#'   plotOutput('plot')
#' )
#'
#' # in server.R
#' function(input, output) {
#'   output$plot <- renderPlot({
#'     validate(
#'       need(input$in1, 'Check at least one letter!'),
#'       need(input$in2 == '', 'Please choose a state.')
#'     )
#'     plot(1:10, main = paste(c(input$in1, input$in2), collapse = ', '))
#'   })
#' }
validate <- function(..., errorClass = character(0)) {
  results <- sapply(list(...), function(x) {
    # Detect NULL or NA
    if (is.null(x))
      return(NA_character_)
    else if (identical(x, FALSE))
      return("")
    else if (is.character(x))
      return(paste(as.character(x), collapse = "\n"))
    else
      stop("Unexpected validation result: ", as.character(x))
  })

  results <- na.omit(results)
  if (length(results) == 0)
    return(invisible())

  # There may be empty strings remaining; these are message-less failures that
  # started as FALSE
  results <- results[nzchar(results)]

  stopWithCondition(c("validation", errorClass), paste(results, collapse="\n"))
}

#' @param expr An expression to test. The condition will pass if the expression
#'   meets the conditions spelled out in Details.
#' @param message A message to convey to the user if the validation condition is
#'   not met. If no message is provided, one will be created using \code{label}.
#'   To fail with no message, use \code{FALSE} for the message.
#' @param label A human-readable name for the field that may be missing. This
#'   parameter is not needed if \code{message} is provided, but must be provided
#'   otherwise.
#' @export
#' @rdname validate
need <- function(expr, message = paste(label, "must be provided"), label) {

  force(message) # Fail fast on message/label both being missing

  if (!isTruthy(expr))
    return(message)
  else
    return(invisible(NULL))
}

isTruthy <- function(x) {
  if (inherits(x, 'try-error'))
    return(FALSE)

  if (!is.atomic(x))
    return(TRUE)

  if (is.null(x))
    return(FALSE)
  if (length(x) == 0)
    return(FALSE)
  if (all(is.na(x)))
    return(FALSE)
  if (is.character(x) && !any(nzchar(na.omit(x))))
    return(FALSE)
  if (inherits(x, 'shinyActionButtonValue') && x == 0)
    return(FALSE)
  if (is.logical(x) && !any(na.omit(x)))
    return(FALSE)

  return(TRUE)
}

# add class(es) to the error condition, which will be used as names of CSS
# classes, e.g. shiny-output-error shiny-output-error-validation
stopWithCondition <- function(class, message) {
  cond <- structure(
    list(message = message),
    class = c(class, 'shiny.silent.error', 'error', 'condition')
  )
  stop(cond)
}

#' Collect information about the Shiny Server environment
#'
#' This function returns the information about the current Shiny Server, such as
#' its version, and whether it is the open source edition or professional
#' edition. If the app is not served through the Shiny Server, this function
#' just returns \code{list(shinyServer = FALSE)}.
#'
#' This function will only return meaningful data when using Shiny Server
#' version 1.2.2 or later.
#' @export
#' @return A list of the Shiny Server information.
serverInfo <- function() {
  .globals$serverInfo
}
.globals$serverInfo <- list(shinyServer = FALSE)

setServerInfo <- function(...) {
  infoOld <- serverInfo()
  infoNew <- list(...)
  infoOld[names(infoNew)] <- infoNew
  .globals$serverInfo <- infoOld
}

# see if the file can be read as UTF-8 on Windows, and converted from UTF-8 to
# native encoding; if the conversion fails, it will produce NA's in the results
checkEncoding <- function(file) {
  # skip *nix because its locale is normally UTF-8 based (e.g. en_US.UTF-8), and
  # *nix users have to make a conscious effort to save a file with an encoding
  # that is not UTF-8; if they choose to do so, we cannot do much about it
  # except sitting back and seeing them punished after they choose to escape a
  # world of consistency (falling back to getOption('encoding') will not help
  # because native.enc is also normally UTF-8 based on *nix)
  if (!isWindows()) return('UTF-8')
  # an empty file?
  size <- file.info(file)[, 'size']
  if (size == 0) return('UTF-8')

  x <- readLines(file, encoding = 'UTF-8', warn = FALSE)
  # if conversion is successful and there are no embedded nul's, use UTF-8
  if (!any(is.na(iconv(x, 'UTF-8'))) &&
        !any(readBin(file, 'raw', size) == as.raw(0))) return('UTF-8')

  # check if there is a BOM character: this is also skipped on *nix, because R
  # on *nix simply ignores this meaningless character if present, but it hurts
  # on Windows
  if (identical(charToRaw(readChar(file, 3L, TRUE)), charToRaw('\UFEFF'))) {
    warning('You should not include the Byte Order Mark (BOM) in ', file, '. ',
            'Please re-save it in UTF-8 without BOM. See ',
            'http://shiny.rstudio.com/articles/unicode.html for more info.')
    if (getRversion() < '3.0.0')
      stop('R does not support UTF-8-BOM before 3.0.0. Please upgrade R.')
    return('UTF-8-BOM')
  }

  enc <- getOption('encoding')
  msg <- c(sprintf('The file "%s" is not encoded in UTF-8. ', file),
           'Please convert its encoding to UTF-8 ',
           '(e.g. use the menu `File -> Save with Encoding` in RStudio). ',
           'See http://shiny.rstudio.com/articles/unicode.html for more info.')
  if (enc == 'UTF-8') stop(msg)
  # if you publish the app to ShinyApps.io, you will be in trouble
  warning(c(msg, ' Falling back to the encoding "', enc, '".'))

  enc
}

# try to read a file using UTF-8 (fall back to getOption('encoding') in case of
# failure, which defaults to native.enc, i.e. native encoding)
readUTF8 <- function(file) {
  enc <- checkEncoding(file)
  # readLines() does not support UTF-8-BOM directly; has to go through file()
  if (enc == 'UTF-8-BOM') {
    file <- base::file(file, encoding = enc)
    on.exit(close(file), add = TRUE)
  }
  x <- readLines(file, encoding = enc, warn = FALSE)
  enc2native(x)
}

# similarly, try to source() a file with UTF-8
sourceUTF8 <- function(file, ...) {
  source(file, ..., keep.source = TRUE, encoding = checkEncoding(file))
}
