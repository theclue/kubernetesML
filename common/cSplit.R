cSplit <- function(indt, splitCols, sep = ",", direction = "wide", 
                   makeEqual = NULL, fixed = TRUE) {
  ## requires data.table >= 1.8.11
  require(data.table)
  if (!is.data.table(indt)) setDT(indt)
  if (any(!vapply(indt[, splitCols, with = FALSE],
                  is.character, logical(1L)))) {
    indt[, eval(splitCols) := lapply(.SD, as.character),
         .SDcols = splitCols]
  }
  X <- lapply(indt[, splitCols, with = FALSE], function(x) {
    strsplit(x, split = sep, fixed = fixed)
  })
  if (direction == "long") {
    if (is.null(makeEqual)) {
      IV <- function(x,y) if (identical(x,y)) TRUE else FALSE
      makeEqual <- ifelse(Reduce(IV, rapply(X, length, how = "list")),
                          FALSE, TRUE)
    }
  } else if (direction == "wide") {
    if (!is.null(makeEqual)) {
      if (!isTRUE(makeEqual)) {
        message("makeEqual specified as FALSE but set to TRUE")
        makeEqual <- TRUE
      }
      makeEqual <- TRUE
    } else {
      makeEqual <- TRUE
    }
  }
  if (isTRUE(makeEqual)) {
    SetUp <- lapply(seq_along(X), function(y) {
      A <- vapply(X[[y]], length, 1L)
      list(Mat = cbind(rep(seq_along(A), A), sequence(A)),
           Val = unlist(X[[y]]))
    })    
    Ncol <- max(unlist(lapply(SetUp, function(y) y[["Mat"]][, 2]), 
                       use.names = FALSE))
    X <- lapply(seq_along(SetUp), function(y) {
      M <- matrix(NA_character_, nrow = nrow(indt), ncol = Ncol)
      M[SetUp[[y]][["Mat"]]] <- SetUp[[y]][["Val"]]
      M
    })
    if (direction == "wide") {
      X <- lapply(seq_along(X), function(x) {
        colnames(X[[x]]) <- paste(splitCols[x], 
                                  sequence(ncol(X[[x]])), 
                                  sep = "_")
        X[[x]]
      })
      cbind(indt, do.call(cbind, X))[, eval(splitCols) := NULL][]
    } else {
      indt <- indt[rep(sequence(nrow(indt)), each = Ncol)]
      X <- lapply(X, function(y) as.vector(t(y)))
      indt[, eval(splitCols) := lapply(X, unlist, use.names = FALSE)][]
    }  
  } else {
    Rep <- vapply(X[[1]], length, integer(1L))
    indt <- indt[rep(sequence(nrow(indt)), Rep)]
    indt[, eval(splitCols) := lapply(X, unlist, use.names = FALSE)][]
  }
}