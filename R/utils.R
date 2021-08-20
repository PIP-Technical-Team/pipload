add_and <- function(x) {
  if (!(is.character(x))) {
    warning("`x` must be character. coercing to character")
    x <- as.character(x)
  }

  lx <- length(x)
  if (lx == 1) {
    y <- x
  }
  else if (lx == 2) {
    y <- paste(x[1], "and", x[2])
  }
  else {
    y <- c(x[1:lx-1], paste("and", x[lx]))
    y <- paste(y, collapse = ", ")
  }
  return(y)
}


list_of_countries <- function(root_dir          = Sys.getenv("PIP_DATA_ROOT_FOLDER"),
                              maindir           = pip_create_globals(root_dir)$PIP_DATA_DIR) {

  countries <- fs::dir_ls(path    = maindir,
                          recurse = FALSE,
                          type    = "directory"
  )
  countries <- as.character(countries)

  country_list <- gsub(maindir, "", countries)
  country_list <- country_list[!grepl("^_", country_list)]
  return(country_list)
}



# Nesting and unnesting data.tables
.nest   <- function(...) {
  list(data.table::data.table(...))
}

.unnest <- function(...) {
    unlist(data.table::data.table(...))
}

