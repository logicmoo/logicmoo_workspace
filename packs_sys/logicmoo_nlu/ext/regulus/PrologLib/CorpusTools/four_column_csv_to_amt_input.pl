/*

Script for converting four-column tab-separated UTF-8 CSV files into AMT input CSVs.

Form of four-column CSV is 

<Source>  <BaselineTrans>  <VariantTrans> <Label>

Invoke as follows:

sicstus -l four_column_csv_to_amt_input.pl -a <CSVFile> 

where <CSVFile> is the CSV file you want to process.

The name of the output AMT file will be of the form <CSVFile>_<Label>_randomised_spreadsheet.csv.

The lines from the original file will be presented in random order, and the order of <BaselineTrans>  <VariantTrans>
will be randomised within each line.

Example:

sicstus -l four_column_csv_to_amt_input.pl -a '$ACCEPT/MT/Evaluations/a_vs_a_RSource_RTrans_PrEdTrans.csv' 

*/

:- compile('crowdsource_translations.pl').

:- prolog_flag(argv, Args), four_column_csv_to_amt_input_from_command_line(Args).

:- halt.
