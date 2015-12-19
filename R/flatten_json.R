flatten_json <-
function(json) {
  #' Some json items are actually lists themselves. This function returns a list
  #' where each item is an atomic vector. Names for "flattened" list items are
  #' created as "[name of the list]_[name of the item in the list]". If the
  #' items aren't named, then the suffix will just be a number.

  list_fields <- names(json)[vapply(X = json, FUN.VALUE = logical(1),
                                    FUN = is.recursive)]
  while (length(list_fields) > 0L) {
    for (list_single in list_fields) {
      name_suffix <- if (is.null(names(json[[list_single]]))) {
                       seq_along(json[[list_single]])
                     } else {
                       names(json[[list_single]])
                     }
      names(json[[list_single]]) <- paste0(list_single, "_", name_suffix)
    }
    json <- append(json[!(names(json) %in% list_fields)],
                 Reduce(x = json[list_fields], append))
    list_fields <- names(json)[vapply(X = json, FUN.VALUE = logical(1),
                                    FUN = is.recursive)]
  }
  json
}
