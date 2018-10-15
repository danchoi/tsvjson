# tsvjson

Unix style tool to convert TSV input into a stream of JSON objects.

## Example

input.tsv (whitespace is tabs):

    1	Dante	Inferno	poetry,literature
    2	Cervantes	Don Quixote	novels,literature

    cat input.tsv | tsvjson id:num author title categories:list
    {"categories":["poetry","literature"],"author":"Dante","id":1,"title":"Inferno"}
    {"categories":["novels","literature"],"author":"Cervantes","id":1,"title":"Don Quixote"}


The arguments to `tsvjson` is a list of field specs. A field spec is

    name[:type]

where type is 

    string
    num
    bool
    list[:type[:seps]] -- defaults to string with comma sep

If type is omitted, it is string.

    list:string:, 
    red,green,blue -> ["red", "green", "blue"]

    list:string:[,;]
    red,green;blue -> ["red", "green", "blue"]

    list:number:, 
    1,2,3 -> [1,2,3]

    list:string:, 
    "" -> []

