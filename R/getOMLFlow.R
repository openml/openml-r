#' Download an OpenML flow.
#'
#' @param id [\code{integer(1)}]\cr
#'   The ID of the desired flow.
#' @template arg_hash
#' @template arg_verbosity
#' @return [\code{\link{OMLFlow}}].
#' @export
getOMLFlow = function(id, session.hash = getSessionHash(), verbosity = NULL) {
  id = asCount(id)
  assertString(session.hash)

  url = getAPIURL("openml.implementation.get", implementation_id = id)
  content = try(downloadXML(url, NULL, verbosity, session_hash = session.hash), silent = TRUE)
  if (is.error(content))
    stop("Flow (temporarily) not available.")
  doc = parseXMLResponse(content, "Getting implementation", "implementation", as.text = TRUE)
  flow = parseOMLFlow(doc)

  # download source and/or binary files:
  downloadFileAndSavePath = function(flow, binOrSource, mode, verbosity) {
    url = flow[[sprintf("%s.url", binOrSource)]]
    if (!is.na(url)) {
      file = basename(url)
      f = findCachedFlow(flow$id, file)[[1L]]
      flow[[sprintf("%s.path", binOrSource)]] = f$path
      if (f$found) {
        showInfo(verbosity, "File %s found in cache.", file)
      } else {
        showInfo(verbosity, "Downloading '%s' to '%s'", url, file)
        download.file(url, f$path, mode = mode, quiet = TRUE)
      }
    }
    return(flow)
  }

  flow = downloadFileAndSavePath(flow, "binary", "wb", verbosity)
  flow = downloadFileAndSavePath(flow, "source", "w", verbosity)

  return(flow)
}

parseOMLFlow = function(doc) {
  args = filterNull(list(
    id = xmlRValI(doc, "/oml:implementation/oml:id"),
    name = xmlRValS(doc, "/oml:implementation/oml:name"),
    version = xmlRValS(doc, "/oml:implementation/oml:version"),
    external.version = xmlOValS(doc, "/oml:implementation/oml:external_version"),
    description = xmlRValS(doc, "/oml:implementation/oml:description"),
    creator = xmlOValsMultNsS(doc, "/oml:implementation/oml:creator"),
    contributor = xmlOValsMultNsS(doc, "/oml:implementation/oml:contributor"),
    upload.date = xmlRValS(doc, "/oml:implementation/oml:upload_date"),
    licence = xmlOValS(doc, "/oml:implementation/oml:licence"),
    language = xmlOValS(doc, "/oml:implementation/oml:language"),
    full.description = xmlOValS(doc, "/oml:implementation/oml:full_description"),
    installation.notes = xmlOValS(doc, "/oml:implementation/oml:installation_notes"),
    dependencies = xmlOValS(doc, "/oml:implementation/oml:dependencies"),
    bibliographical.reference = parseOMLBibRef(doc),
    parameter = parseOMLParameters(doc),
    collection.date = xmlOValS(doc, "/oml:implementation/oml:collection_date"),
    source.url = xmlOValS(doc, "/oml:implementation/oml:source_url"),
    binary.url = xmlOValS(doc, "/oml:implementation/oml:binary_url"),
    source.format = xmlOValS(doc, "/oml:implementation/oml:source_format"),
    binary.format = xmlOValS(doc, "/oml:implementation/oml:binary_format"),
    source.md5 = xmlOValS(doc, "/oml:implementation/oml:source_md5"),
    binary.md5 = xmlOValS(doc, "/oml:implementation/oml:binary_md5"),
    components = list()
  ))

  ## components section
  comp_ns = getNodeSet(doc, "/oml:implementation/oml:component/oml:implementation")
  comp = vector("list", length = length(comp_ns))
  for (i in seq_along(comp_ns)) {
    # save subcomponent temporarily on disk
    # FIXME: this is not very elegant
    file2 = tempfile("components")
    saveXML(comp_ns[[i]], file = file2)
    comp[[i]] = parseOMLFlow(parseXMLResponse(file2, type = "implementation"))
    names(comp)[i] = xmlRValS(doc, paste0("/oml:implementation/oml:component[", i, "]/oml:identifier"))
    unlink(file2)
  }
  args[["components"]] = comp

  do.call(makeOMLFlow, args)
}

parseOMLParameters = function(doc) {
  path = "/oml:implementation/oml:parameter"

  ns = getNodeSet(doc, path)
  nr.pars = length(ns)

  par.names = xmlValsMultNsS(doc, sprintf("%s/oml:name", path))
  par.types = xmlOValsMultNsSPara(doc, sprintf("%s/oml:data_type", path), exp.length = nr.pars)
  par.defs = xmlOValsMultNsSPara(doc, sprintf("%s/oml:default_value", path), exp.length = nr.pars)
  par.descs = xmlOValsMultNsSPara(doc, sprintf("%s/oml:description", path), exp.length = nr.pars)
  par.rec.range = xmlOValsMultNsSPara(doc, sprintf("%s/oml:recommendedRange", path), exp.length = nr.pars)

  par = vector("list", length(par.names))
  for (i in seq_along(par)) {
    par[[i]] = makeOMLFlowParameter(name = par.names[i], data.type = par.types[i],
      default.value = par.defs[i], description = par.descs[i], recommended.range = par.rec.range[i])
  }
  return(par)
}

parseOMLBibRef = function(doc) {
  path = "/oml:implementation/oml:bibliographical_reference"

  ns = getNodeSet(doc, path)

  bib.citation = xmlValsMultNsS(doc, sprintf("%s/oml:citation", path))
  bib.url = xmlValsMultNsS(doc, sprintf("%s/oml:url", path))

  # FIXME: Map()
  bib = vector("list", length(bib.citation))
  for (i in seq_along(bib.citation)) {
    bib[[i]] = makeOMLBibRef(bib.citation[i], bib.url[i])
  }
  if (length(bib) > 0L)
    return(bib)
  return(NULL)
}
