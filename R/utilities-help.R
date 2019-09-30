# rd_gals <- function(name) {
#   obj <- match.fun(paste0("gals_", name))
#   obj <- obj()
#   param <- rd_gals_item(obj)
#
#   c(
#     "@section Definition:",
#     paste0(
#       "", obj$name, " ",
#       "uses the following parameters (fixed parameters are in bold):"
#     ),
#     "\\preformatted{",
#     param,
#     "}",
#     "Learn more about these definitions in \\code{vignette(\"gals\")}"
#   )
# }
#
#
# rd_gals_item <- function(x) {
#   param <- paste0(
#     lapply(names(x), function(name){
#
#       if(!name %in% x$fixed_params){
#         if(name != "fixed_params"){
#           paste0(name, " = ", paste0(x[[name]], collapse=", "))
#         }
#       } else {
#         if(name != "fixed_params"){
#           paste0("",name,"", " = ", paste0(x[[name]], collapse=", "))
#         }
#       }
#     })
#   )
#
#   param[which(!param =="NULL")]
# }
#
#
#
#
#
