decode_html <-
function(x, html_dictionary = NULL) {
  # Swaps HMTL character codes for regular ASCII characters
  # (e.g., "&gt;" -> ">").
  
  if (is.null(html_dictionary)) {
    html_dictionary <- c('&quot;' = '"',
                         '&amp;' = '&',
                         '&lt;' = '<',
                         '&gt;' = '>',
                         '<br( /)?>' = '\n',
                         setNames(intToUtf8(c(32:127, 160:255, 338, 339, 352,
                                              353, 376, 402, 8211, 8212,
                                              8216:8218, 8220:8222, 8224:8226,
                                              8230, 8240, 8364, 8482),
                                            multiple = TRUE),
                                  nm = paste0('&#',
                                              c(paste0("0", 32:99), 100:127,
                                                160:255, 338, 339, 352, 353,
                                                376, 402, 8211, 8212, 8216:8218,
                                                8220:8222, 8224:8226, 8230,
                                                8240, 8364, 8482),
                                              ';')))
  }
  for (i in seq_along(html_dictionary)) {
    x <- gsub(names(html_dictionary[i]), html_dictionary[i], x, perl = TRUE)
  }
  return(x)
}
