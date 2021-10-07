run_export_scorecard_manual <- function(input_file_name_and_dir,
                                        starting_row = NULL){

  if(is.null(starting_row)){
    starting_row <- 1
  }

  number_of_ID_columns <- 3

  total_columns <- ncol(read.xlsx2(input_file_name_and_dir, sheetIndex =  1, check.names = FALSE, endRow = 1))

  column_classes <- c(rep("character", number_of_ID_columns), rep("numeric", (total_columns - number_of_ID_columns)))

  dt_write_excel <- read.xlsx2(input_file_name_and_dir, 1, check.names = FALSE, colClasses = column_classes, startRow = starting_row)

  return(dt_write_excel)

}
