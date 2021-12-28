{
    open Parser
}


let integer = ['0'-'9']+
let word = ['a'-'z' 'A'-'Z' '_']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let space = [' ' '\t' '\n']+

rule lang = parse
    | space
        { lang lexbuf } (* skip blanks *)
    | integer as lxm
        { INT (int_of_string lxm) }
    | word as lxm
        { WORD lxm }
    | '('
        { LPAREN }
    | ')'
        { RPAREN }
    | _  as c
        { Printf.eprintf "Unexpected token : %c" c; lang lexbuf }
    | eof
        { EOF }
