count_replies <-
function(no, com) {
  reply_no_regex <- '(?<=<a href="#p)\\d+(?=" class="quotelink">)'
  com_replies <- regmatches(com, gregexpr(reply_no_regex, com, perl = TRUE))
  unduplicated_replies <- lapply(com_replies, FUN = unique)
  reply_factor <- factor(unlist(unduplicated_replies), levels = no)
  reply_counts <- table(reply_factor)
  as.vector(reply_counts[as.character(no)])
}
