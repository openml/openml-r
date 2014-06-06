setClass("ImplementationParameter", representation(
  name = "character",
  data.type = "character",
  default.value = "character",
  description = "character"
))

ImplementationParameter = function(name, data.type = "", default.value = "", description = "") {
  makeS3Obj("ImplementationParameter", 
    name = name,
    data.type = data.type,
    default.value = default.value,
    description = description
 )
}

#' @export
print.ImplementationParameter = function(x, ...) {
  cat(x$name)
  if (x$data.type != "") cat(' : ',x$data.type)
  if (x$default.value != "") cat(' (default value = ',x$default.value,' )')
  if (x$description != "") cat('\n   Description : ',x$description)
  cat('\n')
}
