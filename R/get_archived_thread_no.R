get_archived_thread_no <-
function(boards, tries = 5,
                                   url_format = "https://a.4cdn.org/%s/archive.json") {
  #' Returns a list of archived thread numbers for the specified boards. Use
  #' query_boards to get a data.table of information including which boards are
  #' archived.
  
    # Go through board list and get the catalog table object from the site
  archived_threads <- vector('list', length(boards))
  names(archived_threads) <- boards
  
  for (board_short in boards) {
    for (i in seq_len(tries)) {
      result <- tryCatch(expr = {
          fromJSON(file = sprintf(url_format, board_short))
        }, error = function(err) {
          cat(sprintf('Error for board /%s/: %s\n',
                      board_short, err[['message']]))
          NULL
        })
      if (!is.null(result)) {
        archived_threads[[board_short]] <- result
      }
    }
  }
  return(archived_threads)
}
