get_active_thread_no <-
function(boards, tries = 5,
                                 url_format = 'https://a.4cdn.org/%s/catalog.json') {
  # Returns a list of numeric vectors with active thread numbers for each board.
  
    # Go through boards and get the catalog table object from the site
  catalogs <- sapply(X = boards, simplify = FALSE,
                     tries = tries,
                     FUN = function(board_short, tries) {
                       for (i in 1:tries) {
                         result <- tryCatch(expr = {
                           fromJSON(file = sprintf(url_format, board_short))
                         }, error = function(err) {
                           NULL
                         })
                         if (!is.null(result)) {
                           break
                         }
                       }
                       return(result)
                     })
  
  sapply(X = catalogs, simplify = FALSE,
         FUN = function(x) {
           flat_x <- unlist(x)
           as.numeric(flat_x[names(flat_x) == "threads.no"])
         })
  
}
