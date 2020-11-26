/*

Script for converting four-column tab-separated UTF-8 CSV file + AMT output CSV into results.

Form of four-column CSV is 

<Source>  <BaselineTrans>  <VariantTrans>  <Label>

Invoke as follows:

sicstus -l four_column_csv_and_amt_output_to_results.pl -a <CSVFile> 

where <CSVFile> is the original four-column CSV file you want to process 

This assumes that the AMT results file name is of the form <CSVFile>_amt.csv

The name of the summary files will be of the form <CSVFile>_<Label>_results.csv, one for each <Label> in the input data

Example:

sicstus -l four_column_csv_and_amt_output_to_results.pl -a $ACCEPT/MT/Evaluations/combined_RSource_RTrans_PrEdTrans.csv 

*/

:- compile('crowdsource_translations.pl').

:- prolog_flag(argv, Args), four_column_csv_and_amt_output_to_results_from_command_line(Args).

:- halt.
