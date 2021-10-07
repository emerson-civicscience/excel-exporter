run_export_scorecard <- function(input_file_name,
                                 input_file_location = NULL,
                                 output_file_name = NULL,
                                 output_file_location = NULL,
                                 starting_row = NULL,
                                 scorecard_uuid = NULL,
                                 bi.user = NULL,
                                 bi.password = NULL,
                                 python_file_name = NULL,
                                 python_file_location = NULL){

  if(is.null(input_file_location)){
    input_file_location <- '~/Excel Exporter/Inputs'
  }

  input_file_name_and_dir <- file.path(input_file_location, input_file_name)

  if(is.null(output_file_name)){
    if(grepl('Input', input_file_name)){
      output_file_name <- gsub('Input', 'Output', input_file_name)
    } else{
      output_file_name <- gsub('.xlsx', ' - Output.xlsx', input_file_name)
    }
  }

  ### Create dated directory if it doesn't exist

  if(is.null(output_file_location)){
    output_date <- today()
    output_file_dir <- file.path('~/Excel Exporter/Outputs', output_date)
  } else{
    output_file_dir <- output_file_location
  }

  if(!dir.exists(output_file_dir)){
    dir.create(output_file_dir)
  }

  output_file_name_and_dir <- file.path(output_file_dir, output_file_name)

  if(is.null(scorecard_uuid)){
    dt_write_excel <- run_export_scorecard_manual(input_file_name_and_dir,
                                                  starting_row)
  } else{
    dt_write_excel <- run_export_scorecard_automatic(input_file_name_and_dir,
                                                     scorecard_uuid,
                                                     bi.user = bi.user,
                                                     bi.password = bi.password)
  }

  pandas_df <- r_to_py(dt_write_excel)
  file_name_py <- r_to_py(output_file_name)

  python_file_name_and_dir <- file.path(python_file_location, python_file_name)

  source_python(python_file_name_and_dir)

  export_scorecard_to_excel(pandas_df,
                            file_name_py)

  original_file_name_and_dir <- file.path(getwd(), output_file_name)

  file_copy_status <- file.copy(from = original_file_name_and_dir,
                                to   = file.path(output_file_name_and_dir))

  if(file_copy_status == TRUE){
    file.remove(original_file_name_and_dir)
  }

}
