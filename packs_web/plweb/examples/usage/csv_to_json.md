# Convert a CSV with column names to JSON

With the help of library(csv) and library(http/json), we read a CSV file
and use the column names in the first row to emit JSON.

For this example, we use the file `weather.csv`:

~~~
city,temp_lo,temp_hi,prcp,date
San Francisco,46,50,0.25,1994-11-27
San Francisco,43,57,0,1994-11-29
Hayward,37,54,,1994-11-29
~~~

The script `csv_to_json.pl` reads the CSV from standard input or from
the file provided as the first argument of the script. It converts
the contents to a list of dicts, then writes this as JSON to standard
output:

~~~
:- initialization(main, main).

:- use_module(library(csv)).
:- use_module(library(http/json)).

main :-
    (   current_prolog_flag(argv, [CSV_file|_])
    ->  csv_read_file(CSV_file, CSV, [])
    ;   csv_read_stream(current_input, CSV, [])
    ),
    CSV = [Colnames|Rows],
    Colnames =.. [row|Names],
    maplist(row_dict(Names), Rows, Dicts),
    json_write_dict(current_output, Dicts, [null('')]).

row_dict(Names, Row, Dict) :-
    Row =.. [row|Fields],
    pairs_keys_values(Data, Names, Fields),
    dict_create(Dict, _, Data).
~~~

The `null('')` option to `json_write_dict/3` is necessary to convert the
empty "prcp" field in the last row to a missing value represented as
`null` in the JSON output.

This is how we can use it to convert `weather.csv` to JSON:

~~~
$ swipl csv_to_json.pl weather.csv
[
  {
    "city":"San Francisco",
    "date":"1994-11-27",
    "prcp":0.25,
    "temp_hi":50,
    "temp_lo":46
  },
  {
    "city":"San Francisco",
    "date":"1994-11-29",
    "prcp":0,
    "temp_hi":57,
    "temp_lo":43
  },
  {
    "city":"Hayward",
    "date":"1994-11-29",
    "prcp":null,
    "temp_hi":54,
    "temp_lo":37
  }
]
~~~
