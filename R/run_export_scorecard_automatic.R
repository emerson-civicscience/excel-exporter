run_export_scorecard_automatic <- function(input_file_name_and_dir,
                                           scorecard_uuid,
                                           bi.user,
                                           bi.password){

  number_of_ID_columns <- 1

  total_columns <- ncol(read.xlsx2(input_file_name_and_dir, sheetIndex =  1, check.names = FALSE, startRow = 13, endRow = 13))

  column_classes <- c("character", rep("numeric", (total_columns - number_of_ID_columns)))

  excel_input <- read.xlsx2(input_file_name_and_dir, 1, check.names = FALSE, colClasses = column_classes, startRow = 13) %>%
  	.[which(.[1] != ""), ]

  excel_input[1] <- 1:(nrow(excel_input))
  colnames(excel_input)[1] <- 'sort_order'

  dt_labels <- scorecard_label_handler(scorecard_uuid = scorecard_uuid,
  																		 bi.user = bi.user,
  																		 bi.password = bi.password)

  dt_write_excel <- left_join(dt_labels, excel_input) %>%
  	.[which(.$`Answer Text` != 'Total Responses'), ] %>%
  	.[, which(colnames(.) != "sort_order")]

  dt_write_excel[, (ncol(dt_labels)):ncol(dt_write_excel)] <- dt_write_excel[, (ncol(dt_labels)):ncol(dt_write_excel)] / 100

  return(dt_write_excel)

}
