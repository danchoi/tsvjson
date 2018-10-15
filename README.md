# tsvjson

Unix style tool to convert TSV input into a stream of JSON objects.

## Example

input.tsv (whitespace is tabs):

    1	Dante	Inferno	poetry,literature
    2	Cervantes	Don Quixote	novels,literature

Pipe `input.tsv` into `tsvjson`, giving `tsvjson` a list of field specs: 

    cat input.tsv | tsvjson id:num author title categories:[string]
    {"categories":["poetry","literature"],"author":"Dante","id":1,"title":"Inferno"}
    {"categories":["novels","literature"],"author":"Cervantes","id":1,"title":"Don Quixote"}

The arguments to `tsvjson` is a list of field specs. A field spec is

    name[:type]

where type is 

    string 
    num
    number (long form of num ok)
    bool
    [type[:seps]] -- list type with separator

If [:type] is omitted, it is inferred to be string.

List types:

    [string]
    red,green,blue -> ["red", "green", "blue"]

    [string:,;]
    red,green;blue -> ["red", "green", "blue"]

    [num]
    1,2,3 -> [1,2,3]

    [string:,]
    "" -> []

Quote the field spec expression if you have shell characters in them (e.g.
a semicolon):

    cat input.tsv | tsvjson id:num author title 'categories:list:,;'

Or you can quote the whole series of field specs:

    cat input.tsv | 'tsvjson id:num author title categories:list:,;'

