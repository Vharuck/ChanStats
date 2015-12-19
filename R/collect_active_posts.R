library(ChanStats)

collect_active_posts <- function(boards, f = identity, ...) {
  active_nos <- get_active_thread_no(boards, ...)
  active_threads <- mapply(FUN = function(b, n) {
                             cat('Reading threads from ', b[1], '...\n',
                                 sep = '')
                             read_thread(b, n)
                           },
                           names(active_nos), active_nos, SIMPLIFY = FALSE)
  cat('Making data tables from threads...\n')
  board_tables <- lapply(active_threads,
                         FUN = function(x) {
                           f(thread_to_table(Reduce(append, x)))
                         })
  cat('Combining threads into a table...\n')
  rbindlist(board_tables)
}