(* Instrumenting the assignments in a C++ source with calls to a dummy
   position function, which can be then found in the LLVM IR. *)

{
(* The name of the source file (before macro expansion) *)

let src_file = ref ""

(* The name of the file as given by the last #line (default in the
   current file name) *)

let cur_file = ref ""

(* Hack to roll back one lexeme in ocamllex buffer (should be safe if
   used in the semantic actions of the regular expression recognising
   the lexeme rolled back. *)

let rollback buffer =
  let open Lexing
in buffer.lex_curr_pos <- buffer.lex_curr_pos - 1;
   buffer.lex_curr_p <-
     {buffer.lex_curr_p with pos_cnum = buffer.lex_curr_p.pos_cnum - 1 }

(* The output channel *)

let out_chan = ref stdout

(* Virtual line number (according to cpp directive) and end of lines *)

let virt_lnum = ref 1

let handle_nl buffer = Lexing.new_line buffer; incr virt_lnum

(* String processing *)

let mk_str (len:int) (p: char list) : string =
  let s = Bytes.make len ' ' in
  let rec fill i =
    function [] -> s | c::l -> Bytes.set s i c; fill (i-1) l
in assert (len = List.length p); Bytes.to_string (fill (len-1) p)

(* Regions in the source file as intervals *)

type line = int
type col = int
type start = line * col
type stop = line * col
type interval = start * stop

(* Regions in the source file as pairs of (Lexing) positions *)
(*
type start_pos = Lexing.position
type stop_pos = Lexing.position
type reg = start_pos * stop_pos
*)

(* The global list of regions corresponding to the assignments found
   in the source file *)

let assignments = ref ([]: interval list)

(* Converting regions to integer intervals. Note: We subtract 1 to the
   end column (the default is the convention of Emacs). *)

let freeze (start,stop) =
  let open Lexing in
  let delta = !virt_lnum - stop.pos_lnum in
  let v_start = start.pos_lnum + delta
in (v_start, start.pos_cnum - start.pos_bol),
   (!virt_lnum, stop.pos_cnum - stop.pos_bol - 1)

(* Converting regions to strings *)

let string_of_reg ((start_line,start_col), (end_line,end_col)) =
  (string_of_int start_line, string_of_int start_col),
  (string_of_int end_line, string_of_int end_col)

(* Adding an interval *)

let add_region r = assignments := r :: !assignments

(* Reifying the region in the source file corresponding to the start
   of the last recognised lexeme and its end point. *)

let mk_reg buffer =
  freeze Lexing.(lexeme_start_p buffer, lexeme_end_p buffer)

(* Logs a region to the output channel and updates the global
   reference [assignments]. If a region spans more than one line, more
   information is included. *)

let log reg =
  if !cur_file = !src_file
  then let (start_line, start_col), (stop_line, stop_col) = string_of_reg reg
       in add_region reg;
          output_string !out_chan
            ("Assignment at line "
             ^ start_line ^ ", char " ^ start_col
             ^ (if start_line = stop_line
                then "--" ^ stop_col
                else " to line " ^ stop_line ^ ", char " ^ stop_col)
             ^ ".\n")
}

(* Regular expressions for literals *)

(* Assignment operators of interest *)

let op = "=" | "*=" | "/=" | "%=" | "+=" | "-=" (* | "++" | "--" *)

(* Operators to be skipped *)

let skip_op = ">=" | "<=" | "!=" | "==" | ">>=" | "<<=" | "&=" | "^=" | "|="

(* White space *)

let newline = '\n' | '\r' | "\r\n"
let blank = ' ' | '\t'

(* Integers *)

let digit = ['0'-'9']
let decimal = digit+

(* Identifiers *)

let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let letter = lowercase | uppercase
let start = '_' | letter
let alphanum = letter | digit | '_'
let ident = start alphanum*

(* Rules *)

rule scan = parse
  newline                     { handle_nl lexbuf; scan lexbuf }
| '"'                         { ignore(in_string 0 [] lexbuf); scan lexbuf }
| "//"                        { in_line_com lexbuf; scan lexbuf }
| "/*"                        { in_block_com lexbuf; scan lexbuf }
| op                          { after_op (mk_reg lexbuf) lexbuf; scan lexbuf }
| "virtual"                   { after_virtual lexbuf }
| "operator"                  { after_operator lexbuf }
| "using" | "namespace"       { ident_eq lexbuf }
| "class" | "typename"        { ident_opt_eq lexbuf }
| eof                         {}
| '#' blank* ("line" blank+)? { line_indicator lexbuf; scan lexbuf }
| skip_op                     (* Do not remove *)
| _                           { scan lexbuf }

(* Line indicator (#line n "foo.cc" | # n "foo.cc") *)

and line_indicator = parse
  decimal as d { end_indicator (int_of_string d) lexbuf }
| _            {}

and end_indicator n = parse
  blank* newline { handle_nl lexbuf }
| blank* "//"    { in_line_com lexbuf }
| blank+ '"'     { cur_file := in_string 0 [] lexbuf;
                   until_eol lexbuf;
                   if !cur_file = !src_file then virt_lnum := n }
| _              { assert false }

and until_eol = parse
  newline { handle_nl lexbuf }
| eof     {}
| _       { until_eol lexbuf }

(* Declaration of virtual methods *)

and after_virtual = parse
  newline                 { handle_nl lexbuf; after_virtual lexbuf }
| blank | ['\011'-'\013']
| [^'\n' '(']+            { after_virtual lexbuf }
| ';'                     { scan lexbuf } (* assert false *)
| '('                     { after_lpar 1 lexbuf }
| eof                     { assert false }

and after_lpar n = parse
  newline                 { handle_nl lexbuf; after_lpar n lexbuf }
| blank | ['\011'-'\013'] { after_lpar n lexbuf }
| ';'                     { scan lexbuf } (* assert false *)
| '('                     { after_lpar (n+1) lexbuf }
| ')'                     { if n=0 then after_rpar lexbuf
                            else after_lpar (n-1) lexbuf }
| eof                     { assert false }
| _                       { after_lpar n lexbuf }

and after_rpar = parse
  newline                 { handle_nl lexbuf; after_rpar lexbuf }
| blank | ['\011'-'\013'] { after_rpar lexbuf }
| '='                     { after_eq lexbuf }
| eof                     { assert false }
| _                       { scan lexbuf }

and after_eq = parse
  newline                 { handle_nl lexbuf; after_eq lexbuf }
| blank | ['\011'-'\013'] { after_eq lexbuf }
| eof                     { assert false }
| _                       { scan lexbuf } (* '0' *)

(* Operators *)

and after_operator = parse
  newline                 { handle_nl lexbuf; after_operator lexbuf }
| blank | ['\011'-'\013'] { after_operator lexbuf }
| op                      { scan lexbuf }
| _                       { rollback lexbuf; scan lexbuf } (* assert false *)

and ident_opt_eq = parse
  newline                 { handle_nl lexbuf; ident_opt_eq lexbuf }
| blank | ['\011'-'\013'] { ident_opt_eq lexbuf }
| ident?                  { before_eq lexbuf }
| eof                     { assert false }

and ident_eq = parse
  newline                 { handle_nl lexbuf; ident_eq lexbuf }
| blank | ['\011'-'\013'] { ident_eq lexbuf }
| ident                   { before_eq lexbuf }
| _                       { rollback lexbuf; scan lexbuf } (* assert false *)

and before_eq = parse
  newline                 { handle_nl lexbuf; before_eq lexbuf }
| blank | ['\011'-'\013']
| '='                     { before_eq lexbuf }
| _                       { rollback lexbuf; scan lexbuf }

(* Assignment operator *)

and after_op reg = parse
  newline                 { handle_nl lexbuf; after_op reg lexbuf }
| blank | ['\011'-'\013'] { after_op reg lexbuf }
| ']' | ',' | "default" | "delete" | "throw"
                          {}
| _                       { log reg; rollback lexbuf }

(* Inline comment *)

and in_line_com = parse
  newline { handle_nl lexbuf }
| eof     {}
| _       { in_line_com lexbuf }

(* Block comment *)

and in_block_com = parse
  newline { handle_nl lexbuf; in_block_com lexbuf }
| "*/"    {}
| eof     { assert false }
| _       { in_block_com lexbuf }

(* Strings *)

and in_string len acc = parse
  "\\\""  { in_string (len+1) ('"'::acc) lexbuf }
| '"'     { mk_str len acc }
| newline { assert false }
| eof     { assert false }
| _ as c  { in_string (len+1) (c::acc) lexbuf }

{
(* Standalone scanner *)

let find_candidates ~(src:string) ~(out:string) =
  match open_in src, open_out out with
    cin, cout ->
      let buffer = Lexing.from_channel cin
      in begin
           src_file := src;
           cur_file := src;
           out_chan := cout;
           scan buffer;
           flush_all ();
           close_in cin;
           close_out cout;
           List.rev !assignments
         end
  | exception Sys_error msg -> prerr_endline msg; exit 1
}
