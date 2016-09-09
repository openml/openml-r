xmlNs = function(doc, path, optional) {
  ns = getNodeSet(doc, path)
  if (length(ns) == 0L) {
    if (optional)
      NULL
    else
      stopf("Required XML node not found: %s", path)
  } else {
    ns
  }
}

xmlVal = function(doc, path, optional, fun) {
  ns = xmlNs(doc, path, optional)
  # path not found, also cant be no optional, otherwise exception in call before
  if (is.null(ns))
    return(NULL)
  if (length(ns) == 1L) {
    fun(xmlValue(ns[[1L]]))
  } else {
    stopf("Multiple XML nodes found: %s", path)
  }
}

xmlValueNA = function(x, ...) {
  val = xmlValue(x, ...)
  setNA = ifelse(val == "", NA, val)
  return(setNA)
}

getChildrenStringsNA = function(x, ...) {
  val = getChildrenStrings(x, ...)
  #if (attr.rm) attributes(val) = NULL
  setNA = ifelse(val == "", NA, val)
  return(setNA)
}

xmlOValS = function(doc, path) {
  xmlVal(doc, path, TRUE, as.character)
}

xmlOValI = function(doc, path) {
  xmlVal(doc, path, TRUE, as.integer)
}

xmlOValR = function(doc, path) {
  xmlVal(doc, path, TRUE, as.numeric)
}

xmlOValD = function(doc, path) {
  xmlVal(doc, path, FALSE, as.Date)
}

xmlRValS = function(doc, path) {
  xmlVal(doc, path, FALSE, as.character)
}

xmlRValI = function(doc, path) {
  xmlVal(doc, path, FALSE, as.integer)
}

xmlRValR = function(doc, path) {
  xmlVal(doc, path, FALSE, as.numeric)
}

xmlRValD = function(doc, path) {
  xmlVal(doc, path, FALSE, function(x) as.POSIXct(x, tz="CET"))
}

xmlREValI = function(doc, path) {
  val = xmlRValI(doc, path)
  if (is.na(val))
    return(integer(0L))
  else
    return(val)
}

xmlREValR = function(doc, path) {
  val = xmlRValR(doc, path)
  if (is.na(val))
    return(numeric(0L))
  else
    return(val)
}

xmlREValI = function(doc, path) {
  val = xmlRValI(doc, path)
  if (is.na(val))
    return(integer(0L))
  else
    return(val)
}

xmlValsMultNs = function(doc, path, fun, val) {
  ns = getNodeSet(doc, path)
  vapply(ns, function(x) fun(xmlValue(x)), val)
}

xmlValsMultNsS = function(doc, path) {
  xmlValsMultNs(doc, path, as.character, character(1))
}

xmlOValsMultNsS = function(doc, path, empty.return = NULL) {
  val = xmlValsMultNs(doc, path, as.character, character(1))
  if (length(val) == 0L)
    return(empty.return)
  else
    return(val)
}

xmlOValsMultNsSPara = function(doc, path, subs = NA_character_, exp.length) {
  val = xmlValsMultNs(doc, path, as.character, character(1L))
  if (length(val) == 0L)
    return(rep(subs, times = exp.length))
  val[is.na(val) | !nzchar(val)] = subs
  if (length(val) != exp.length)
    val = c(val, rep(subs, times = exp.length - length(val)))
  return(val)
}

xmlValsMultNsI = function(doc, path) {
  xmlValsMultNs(doc, path, as.integer, integer(1L))
}

xmlValsMultNsN = function(doc, path) {
  xmlValsMultNs(doc, path, as.numeric, numeric(1L))
}

parseXMLResponse = function(file, msg = NA_character_,
  type = NA_character_, as.text = FALSE, return.doc = TRUE) {

  doc = try(xmlParse(file, asText = as.text))
  if (is.error(doc))
    stopf("Error in parsing XML for type %s in file: %s", type, file)

  r = xmlRoot(doc)

  if (return.doc)
    return(doc)
  return(r)
}
