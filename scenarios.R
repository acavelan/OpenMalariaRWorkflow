vary <- function(...) {
  x <- list(...)
  if (length(x) != 1 || is.null(names(x)) || names(x) == "") {
    stop("Use vary(name = values), e.g. vary(eir = c(5, 20))")
  }
  structure(x[[1]], vary = names(x))
}

s <- function(scaffold, ...) {
  x <- list(...)
  if (length(x) %% 2 != 0) stop("Scenarios must be xpath/value pairs")

  replacements <- data.frame(
    xpath = unlist(x[c(TRUE, FALSE)], use.names = FALSE),
    id = seq_len(length(x) / 2),
    stringsAsFactors = FALSE
  )
  values <- x[c(FALSE, TRUE)]

  vars <- list()
  for (value in values) {
    name <- attr(value, "vary", exact = TRUE)
    if (!is.null(name)) vars[[name]] <- value
  }

  grid <- if (length(vars)) expand.grid(vars, KEEP.OUT.ATTRS = FALSE) else data.frame(.fixed = 1)
  rows <- vector("list", nrow(grid))

  for (i in seq_len(nrow(grid))) {
    scenario <- replacements
    scenario$value <- vapply(values, function(value) {
      name <- attr(value, "vary", exact = TRUE)
      as.character(if (is.null(name)) value else grid[[name]][i])
    }, character(1))
    rows[[i]] <- list(scaffold = scaffold, meta = grid[i, names(vars), drop = FALSE], replacements = scenario)
  }

  rows
}

plain_xpath <- function(path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  xpath <- "/*[local-name()='scenario']"

  for (part in parts) {
    if (startsWith(part, "@")) {
      xpath <- paste0(xpath, "/", part)
    } else {
      xpath <- paste0(xpath, "/*[local-name()='", part, "']")
    }
  }

  xpath
}

set_xml_value <- function(doc, path, value) {
  if (startsWith(path, "@")) {
    xml2::xml_set_attr(xml2::xml_root(doc), sub("^@", "", path), value)
    return(invisible(doc))
  }

  nodes <- xml2::xml_find_all(doc, plain_xpath(path))
  if (length(nodes) == 0) stop("XPath did not match: ", path)

  if (grepl("/@", path, fixed = TRUE) || startsWith(path, "@")) {
    attr <- sub("^.*@", "", path)
    xml2::xml_set_attr(xml2::xml_parent(nodes), attr, value)
  } else {
    xml2::xml_set_text(nodes, value)
  }

  invisible(doc)
}

write_scenarios <- function(spec, experiment_folder, om) {
  if (!om$output_format %in% c("bin", "bin.gz", "txt", "txt.gz")) stop("Unknown output_format: ", om$output_format)

  index <- 1L
  scenarios <- list()

  for (item in spec) {
    for (scaffold in item$scaffold) {
      doc <- xml2::read_xml(scaffold)
      set_xml_value(doc, "@schemaVersion", om$version)
      set_xml_value(doc, "@xsi:schemaLocation", paste0(
        "http://openmalaria.org/schema/scenario_", om$version,
        " scenario_", om$version, ".xsd"
      ))

      for (row in seq_len(nrow(item$replacements))) {
        set_xml_value(doc, item$replacements$xpath[row], item$replacements$value[row])
      }

      xml2::write_xml(doc, file.path(experiment_folder, "xml", paste0(index, ".xml")))
      meta <- item$meta
      meta$scaffoldName <- scaffold
      meta$index <- index
      meta$outputFile <- file.path("out", paste0(index, ".", om$output_format))
      scenarios[[index]] <- meta
      index <- index + 1L
    }
  }

  data.table::rbindlist(scenarios, fill = TRUE)
}
