read_thread <-
function(board, number, tries = 5) {
  lapply(paste0("http://api.4chan.org/", board, "/res/", number, ".json"),
         FUN = function(x) {
           for (i in 1:tries) {
             json_thread <- tryCatch(expr = {
                              fromJSON(file = x, unexpected.escape = 'skip')
                            }, error = function(err) {
                              return(err)
                            })
             json_thread <- json_thread[["posts"]]
             if ('list' %in% class(json_thread)) {
               break
             }
           }
           return(json_thread)
         })
}
