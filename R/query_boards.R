query_boards <-
function(info_url = "http://a.4cdn.org/boards.json") {
  # Returns a data table of information about each board.
  
  boards_json <- fromJSON(file = info_url)[["boards"]]
  boards_json <- lapply(X = boards_json, FUN = flatten_json)
  
  fields_by_board <- lapply(X = boards_json, FUN = names)
  all_fields <- Reduce(x = fields_by_board, f = union)
  
    # Need to determine the vector class of each field to create a frame
  field_classes <- character(length(all_fields))
  names(field_classes) <- all_fields
  unknown_classes <- all_fields
    
  for (bj in boards_json) {
    bj_classes <- vapply(X = bj, FUN.VALUE = character(1),
                         FUN = function(x) class(x)[1])
    new_fields <- intersect(names(bj), unknown_classes)
    field_classes[new_fields] <- bj_classes[new_fields]
    unknown_classes <- setdiff(unknown_classes, new_fields)
    if (length(unknown_classes) == 0L) break
  }
  
    # Create an empty data.table, and populate it with each board's data
  board_info <- as.data.table(lapply(X = field_classes, FUN = vector,
                                     length = length(boards_json)))
  for (row_number in seq_along(boards_json)) {
    set(board_info, i = row_number,
        j = names(boards_json[[row_number]]),
        value = boards_json[[row_number]])
  }
  board_info
}
