is_xml <- function(x) inherits(x, c("xml_node", "xml_nodeset"))

s <- function(scaffold, ...) {
  replacements <- list(...)
  if (length(replacements) && (is.null(names(replacements)) || any(!nzchar(names(replacements))))) {
    stop('Name each replacement with its XPath, e.g. "demography/@popSize" = 200')
  }
  valid <- function(x) is_xml(x) || length(x) == 1 &&
    (!is.list(x) || length(names(x)) == 1 && nzchar(names(x)) && length(x[[1]]) > 0)
  if (!all(vapply(replacements, valid, logical(1)))) {
    stop("Use scalar or XML fixed values, or list(name = values) for varying values")
  }
  list(scaffold = scaffold, replacements = replacements)
}

write_scenarios <- function(spec, experiment_folder, om, validate_xml = TRUE, overwrite = FALSE) {
  variables <- do.call(c, unname(Filter(function(x) is.list(x) && !is_xml(x), spec$replacements)))
  variables <- variables[!duplicated(names(variables), fromLast = TRUE)]
  scenarios <- data.table::as.data.table(expand.grid(
    c(variables, list(scaffoldName = spec$scaffold)),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  ))
  scenarios$index <- seq_len(nrow(scenarios))
  if (validate_xml) schema <- xml2::read_xml(file.path(om$path, paste0("scenario_", om$version, ".xsd")))

  xml_folder <- file.path(experiment_folder, "xml")
  if (file.exists(xml_folder) && !overwrite) {
    stop("XML folder already exists: ", xml_folder, ". Use overwrite = TRUE to replace it.")
  }
  if (overwrite && unlink(xml_folder, recursive = TRUE)) stop("Could not remove: ", xml_folder)
  if (!dir.create(xml_folder, recursive = TRUE)) stop("Could not create: ", xml_folder)

  for (row in seq_len(nrow(scenarios))) {
    index <- scenarios$index[row]
    doc <- xml2::read_xml(scenarios$scaffoldName[row])
    root <- xml2::xml_root(doc)
    xml2::xml_set_attr(root, "schemaVersion", om$version)
    xml2::xml_set_attr(root, "xsi:schemaLocation", paste0(
      "http://openmalaria.org/schema/scenario_", om$version,
      " scenario_", om$version, ".xsd"
    ))

    for (replacement in seq_along(spec$replacements)) {
      path <- names(spec$replacements)[replacement]
      value <- spec$replacements[[replacement]]
      if (is.list(value) && !is_xml(value)) value <- scenarios[[names(value)]][row]

      nodes <- xml2::xml_find_all(root, path, xml2::xml_ns(doc))
      if (!length(nodes)) stop("XPath did not match: ", path)

      if (is_xml(value)) {
        if (xml2::xml_type(nodes[[1]]) == "attribute") stop("Cannot add XML to an attribute: ", path)
        children <- if (inherits(value, "xml_nodeset")) value else list(value)
        for (child in children) xml2::xml_add_child(nodes, child)
      } else if (xml2::xml_type(nodes[[1]]) == "attribute") {
        xml2::xml_set_attr(xml2::xml_parent(nodes), xml2::xml_name(nodes[[1]]), value)
      } else {
        xml2::xml_set_text(nodes, value)
      }
    }

    if (validate_xml) {
      valid <- xml2::xml_validate(doc, schema)
      if (!valid) stop("Invalid scenario ", index, ":\n", paste(attr(valid, "errors"), collapse = "\n"))
    }
    xml2::write_xml(doc, file.path(experiment_folder, "xml", paste0(index, ".xml")))
  }

  message(nrow(scenarios), " scenarios created")
  scenarios
}
