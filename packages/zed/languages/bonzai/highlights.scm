(literal_string) @string
(literal_integer) @number
(literal_float) @float
(literal_boolean) @boolean

"fn" @keyword
"let" @keyword
"mut" @keyword
"require" @keyword
"extern" @keyword
"pub" @keyword
"if" @keyword
"then" @keyword
"else" @keyword
"while" @keyword
"match" @keyword
"case" @keyword
"type" @keyword
"spawn" @keyword

"->" @punctuation

(comment) @comment
(expr_binary
	callee: _ @function
)


(expr_variable) @variable


(let_name) @variable
(pat_constructor
	name: (pat_variable) @variable
)

(pat_wildcard) @punctuation
