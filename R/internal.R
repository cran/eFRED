
.use_default_key <- function(){
  key <- Sys.getenv("FRED_KEY")
  if (key == ""){
    key <- "04dc6777a721d81414a460093933e0ae"
    Sys.setenv(FRED_KEY = key)
  }
  key
}


.id_parse <- function(...){
  series <- list(...)
  if (length(series) == 0) stop("No series provided.")
  vals <- sapply(series, function(i) toupper(as.character(i)))
  nm <- names(series)
  if (is.null(nm)){
    vals <- Reduce(c, vals)
    nm   <- vals
  } else {
    nm <- Reduce(c, sapply(seq_along(nm), function(i){
      if (length(vals[[i]]) > 1){
        nmval <- names(vals[[i]])
        if (is.null(nmval)) nmval <- vals[[i]]
        if (nm[i] == "") return(nmval)
        return(paste0(nm[i], nmval))
      } else if (nm[i] == "") return(vals[[i]])
      nm[i]
    }))
    vals <- Reduce(c, vals)
  }
  list(id = vals, name = nm)
}

.real_parse <- function(start, end){
  if (!is.null(start)){start <- paste0("&realtime_start=", start)
  } else {start <- ""}
  if (!is.null(end)){end <- paste0("&realtime_end=", end)
  } else {end <- ""}
  paste0(start, end)
}

.loop_id_list <- function(series, fun, key, other){
  results <- lapply(seq_along(series$id), function(i){
    tryCatch(
      fun(series$id[i], key, other),
      error = function(e){
        warning(paste0("Series '", series$name[i], "' not found."))
        NULL
      }
    )
  })
  names(results) <- series$name
  results[!sapply(results, is.null)]
}

.loop_id_rbind <- function(series, fun, key, other){
  results <- lapply(seq_along(series$id), function(i){
    tryCatch(
      {
        ser <- fun(series$id[i], key, other)
        ser$group <- series$name[i]
        ser
      },
      error = function(e){
        warning(paste0("Series '", series$name[i], "' not found."))
        NULL
      }
    )
  })
  Reduce(rbind, results)
}

.loop_id_merge <- function(series, fun, key, other, all=TRUE, by="date"){
  results <- lapply(seq_along(series$id), function(i){
    tryCatch(
      {
        ser <- fun(series$id[i], key, other)
        colnames(ser)[which(colnames(ser) == "value")] <- series$name[i]
        ser
      },
      error = function(e){
        warning(paste0("Series '", series$name[i], "' not found."))
        NULL
      }
    )
  })
  df <- results[[1]]
  if (length(results) > 1){
    for (i in 2:length(results)) df <- merge(df, results[[i]], all=all, by=by)
  }
  df
}

.loop_general_rbind <- function(ids, fun, key, other){
  results <- lapply(ids, function(i){
    tryCatch(
      {
        ser <- fun(i, key, other)
        ser$group <- i
        ser
      },
      error = function(e){
        warning(paste0("ID '", i, "' not found."))
        NULL
      }
    )
  })
  Reduce(rbind, results)
}


.args_parse <- function(args){
  if (length(args) == 0) return("")
  nm <- names(args)
  if (is.null(nm)) stop("API arguments must be named.")
  paste(
    sapply(seq_along(args), function(i){
      paste0("&",nm[i],"=",as.character(args[[i]]))
    }),
    collapse=""
  )
}
