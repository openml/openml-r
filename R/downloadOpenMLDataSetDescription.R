downloadOpenMLDataSetDescription = function(id, file, show.info) {
  id = asInt(id)
  assertPathForOutput(file, overwrite = TRUE)
  downloadAPICallFile(api.fun = "openml.data.description", file = file, data.id = id, show.info = show.info)
}

parseOpenMLDataSetDescription = function(file) {
  assertFile(file)
  doc = parseXMLResponse(file, "Getting data set description", "data_set_description")

  args = list()
  args[["id"]] = xmlRValI(doc, "/oml:data_set_description/oml:id")
  args[["name"]] = xmlRValS(doc, "/oml:data_set_description/oml:name")
  args[["version"]] = xmlRValS(doc, "/oml:data_set_description/oml:version")
  args[["description"]] = xmlRValS(doc, "/oml:data_set_description/oml:description")
  args[["format"]] = xmlRValS(doc, "/oml:data_set_description/oml:format")
  args[["creator"]] = xmlOValsMultNsS(doc, "/oml:data_set_description/oml:creator")
  args[["contributor"]] = xmlOValsMultNsS(doc, "/oml:data_set_description/oml:contributor")
  args[["collection.date"]] = xmlOValS(doc, "/oml:data_set_description/oml:collection_date")
  args[["upload.date"]] = xmlRValD(doc, "/oml:data_set_description/oml:upload_date")
  args[["language"]] = xmlOValS(doc, "/oml:data_set_description/oml:language")
  args[["licence"]] = xmlOValS(doc, "/oml:data_set_description/oml:licence")
  args[["url"]] = xmlRValS(doc, "/oml:data_set_description/oml:url")
  args[["default.target.attribute"]] = xmlOValS(doc, "/oml:data_set_description/oml:default_target_attribute")
  args[["row.id.attribute"]] = xmlOValS(doc, "/oml:data_set_description/oml:row_id_attribute")
  args[["ignore.attribute"]] = xmlOValsMultNsS(doc, "/oml:data_set_description/oml:ignore_attribute")
  args[["version.label"]] = xmlOValS(doc, "/oml:data_set_description/oml:version_label")
  args[["citation"]] = xmlOValS(doc, "/oml:data_set_description/oml:citation")
  args[["visibility"]] = xmlOValS(doc, "/oml:data_set_description/oml:visibility")
  args[["original.data.url"]] = xmlOValS(doc, "/oml:data_set_description/oml:original_data_url")
  args[["paper.url"]] = xmlOValS(doc, "/oml:data_set_description/oml:paper.url")
  args[["update.comment"]] = xmlOValS(doc, "/oml:data_set_description/oml:update.comment")
  args[["md5.checksum"]] = xmlRValS(doc, "/oml:data_set_description/oml:md5_checksum")
  args[["data.set"]] = data.frame()

  dsd = do.call(makeOpenMLDataSetDescription, args)
}