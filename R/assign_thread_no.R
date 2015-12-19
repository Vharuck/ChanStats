assign_thread_no <-
function(no, resto) {
  ## Quick thing to match thread posts to the original post.
  no <- as.numeric(no)
  resto <- as.numeric(resto)
  if (length(no) != length(resto)) {
    max_length <- max(length(no), length(resto))
    resto <- rep_len(resto, max_length)
    no <- rep_len(no, max_length)
  }
  is_op <- resto == 0
  replace(resto, is_op, no[is_op])
}
