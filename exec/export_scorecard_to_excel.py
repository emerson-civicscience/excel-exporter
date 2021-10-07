#' @export

import pandas as pd
import numpy as np
import xlsxwriter as xw
import operator
# import datetime


def export_scorecard_to_excel(pandas_df, file_name_py):
	first_table_row = 0
	category_name_row = 4
	first_data_row = first_table_row + category_name_row
	first_table_col = 2
	first_data_col = first_table_col + 1
	
	chart_row = 2 # Where you want to insert chart in Excel. '2' means column C
	chart_col = 0 # Where you want to insert chart in Excel. '0' means row 1
	
	chart_number = 0
	chart_dict = {}
	writer = pd.ExcelWriter(file_name_py, engine='xlsxwriter')
	workbook = writer.book
	
	title_cell_format = workbook.add_format({'font_color': 'white'})
	
	first_data_cell = xw.utility.xl_rowcol_to_cell(row=first_data_row, col=first_data_col)
	
	banner_qtext_location = 'D2'
	stem_qtext_location = 'D3'
	
	chart_references = pandas_df['Question ID'].unique().tolist()
	
	
	def makeChart(chart_title, chart_series, chart_number):
		add_list = [first_data_col] * len(chart_series)
		series = list(map(operator.add, chart_series, add_list))
		series = list(map(xw.utility.xl_col_to_name, series))
		
		chart_name = 'chart' + str(chart_number)
		
		# Create a chart object.
		chart = workbook.add_chart({'type': 'column'}) # subtype???
		chart.set_size({'width': 870, 'height': 433})
		chart.set_legend({'font': {'name': 'Segoe UI Light', 'size': 10}, 'position': 'bottom'})
		chart.set_x_axis({'line': {'color': '#D9D9D9'},
		                    'num_font': {'name': 'Segoe UI Light', 'size': 11, 'color': '#595959'}})
		chart.set_y_axis({'line': {'none': True},
		                    'num_font': {'name': 'Segoe UI Light', 'size': 11, 'color': '#595959'}, 
		                    'major_gridlines': {'visible': True, 'line': {'color' : '#D9D9D9'}},
		                    'max': 1, 'major_unit': .1})
		    
		chart_dict[chart_name] = chart
		        
		chart.set_title({'name': chart_title, 'name_font': {'name': 'Segoe UI Light', 'size': 13, 'color': '#595959'}})
		   
		
		for row_in_sheet in range(first_data_row+1, crosstab_df_t_nrow+2):
			
			j = ','.join(['\'' + truncated_sheet_name + '\'!' + str(i) + str(row_in_sheet) for i in series])
			k = ','.join(['\'' + truncated_sheet_name + '\'!' + str(i) + str(category_name_row) for i in series])
			formula_for_series = '=(' + j + ')'
			formula_for_categories = '=(' + k + ')'
			col_with_series_name = first_table_col
			name_of_series = '\'' + truncated_sheet_name + '\'!' + xw.utility.xl_rowcol_to_cell(row=row_in_sheet-1, col=col_with_series_name)
			chart.add_series({'values': formula_for_series, 
			'name': name_of_series, 
			# 'type': 'percentage',
			'overlap': -25,
			'categories': formula_for_categories
			})
			
			worksheet.insert_chart(chart_row, chart_col, chart_dict[chart_name])
			# worksheet.set_zoom(70)
			
			
	for crosstab_loop in chart_references:
		
		crosstab_df = pandas_df.loc[pandas_df['Question ID']==crosstab_loop]
		crosstab_df.reset_index(drop=True, inplace=True)    
		chart_title = crosstab_df.loc[0, 'Question Text']
		chart_series = crosstab_df.index.tolist()
		crosstab_df_t = crosstab_df.T
		crosstab_df_t_nrow, crosstab_df_t_ncol = crosstab_df_t.shape
		truncated_sheet_name = str(crosstab_loop)[0:27]
		crosstab_df_t.to_excel(writer, sheet_name=truncated_sheet_name, index=True, startcol = first_table_col)        
		last_data_col = crosstab_df_t_ncol + first_table_col
		last_data_cell = xw.utility.xl_rowcol_to_cell(row=crosstab_df_t_nrow, col=last_data_col)
		worksheet = writer.sheets[truncated_sheet_name]
		worksheet.set_column(0, 0, 160)
		percent_format = workbook.add_format({'num_format': '0%'})
		worksheet.conditional_format(first_data_cell + ':' + last_data_cell, 
			{'type': 'cell', 'criteria': '<', 'value': 1, 'format': percent_format})
			
		chart_number += 1
			
		makeChart(chart_title, chart_series, chart_number)
			
		
			
	writer.save()


