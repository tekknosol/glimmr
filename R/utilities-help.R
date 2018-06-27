rd_gals <- function(name) {
  obj <- match.fun(paste0("gals_", name))
  obj <- obj()
  param <- rd_gals_item(obj)

  c(
    "@section Definition:",
    paste0(
      "", obj$name, " ",
      "uses the following parameters (fixed parameters are in bold):"
    ),
    "\\itemize{",
    paste0("  \\item ", param),
    "}"
  )
}


rd_gals_item <- function(x) {
  param <- paste0(
    lapply(names(x), function(name){

      if(!name %in% x$fixed_params){
        if(name != "fixed_params" & !is.null(x[[name]])){
          paste0(name, " = ", paste0(x[[name]], collapse=", "))
        }
      } else {
        if(name != "trimmer" & name != "fixed_params" & !is.null(x[[name]])){
          paste0("\\strong{",name,"}", " = ", paste0(x[[name]], collapse=", "))
        }
      }
    })
  )

  param[which(!param =="NULL")]
}





