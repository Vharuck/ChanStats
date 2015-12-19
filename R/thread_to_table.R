thread_to_table <- function(thread, field_classes = json_4mat) {
  #' Turns a list of post details into a data table containing all the fields in
  #' json_4mat, populated by post data as available. Use this function on each
  #' item in a list returned by read_thread, with lapply or something similar.
  #' read_thread actually returns post lists in a larger list, so it cannot be
  #' given directly to this function.
  
  nrows <- length(thread)
  varnames <- names(json_4mat)
  rbindlist(lapply(X = thread, varnames, json_4mat,
                   FUN = function(post, varnames, json_4mat) {
                     outrow <- setNames(lapply(json_4mat,
                                               FUN = function(x) {
                                                 vector(x, 1)
                                               }),
                                        nm = varnames)
                     keep_var <- intersect(varnames, names(post))
                     field_length <- vapply(post, FUN.VALUE = numeric(1),
                                            FUN = length)
                     keep_var <- keep_var[field_length[keep_var] > 0]
                     outrow[keep_var] <- post[keep_var]
                     return(outrow)
                   }))
}