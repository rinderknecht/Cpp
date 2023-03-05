(* Lexing a C++ source file with a list of regions *)

{
(* The name of the source file (before macro expansion) *)

let src_file = ref ""

(* The name of the file as given by the last #line (default in the
   current file name) *)

let cur_file = ref ""

(* The output channel *)

let out_chan = ref stdout

(* Copying the matched prefix of the input buffer
   to output channel *)

let copy lexbuf = output_string !out_chan (Lexing.lexeme lexbuf)

(* Virtual line number (according to cpp directive) and end of lines *)

let virt_lnum = ref 1

let handle_nl buffer = Lexing.new_line buffer; incr virt_lnum

(* String processing *)

let mk_str (len:int) (p: char list) : string =
  let s = Bytes.make len ' ' in
  let rec fill i =
    function [] -> s | c::l -> Bytes.set s i c; fill (i-1) l
in assert (len = List.length p); Bytes.to_string (fill (len-1) p)

(* Reifying the region in the source file corresponding to the start
   of the last recognised lexeme and its end. *)

let mk_reg buffer =
  Lexing.(lexeme_start_p buffer, lexeme_end_p buffer)

(* A list of values of the type [opening] contains a valid prefix of a
   well-parenthesised construct (either with parentheses proper, curly
   brackets, or square brackets). The data constructor [Src] stands
   for any opening construct present in the source code, whereas
   [Virt] represents a virtual opening parenthesis, one introduced by
   the calls to [pos]. *)

type opening = Src | Virt

let rec pop_virt = function
           Virt :: s -> output_char !out_chan ')'; pop_virt s
| (Src::_ | []) as s -> s

let pop_src = function
      Src :: s -> s
| Virt::_ | [] -> assert false

}

(* Regular expressions for literals *)

(* Assignment operators of interest *)

let op = "=" | "*=" | "/=" | "%=" | "+=" | "-=" (* | "++" | "--" *)

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

(* White space *)

let newline = '\n' | '\r' | "\r\n"
let blank = ' ' | '\t'

(* Rules *)

rule scan cand stack col = parse
  newline       { copy lexbuf; handle_nl lexbuf; scan cand stack 0 lexbuf }
| eof           { match cand with [] -> copy lexbuf | _ -> assert false }
| '"'           { copy lexbuf;
                  let len, _ = in_string 0 [] lexbuf
                  in scan cand stack (col+len+2) lexbuf }
| "//"          { copy lexbuf; in_line_com cand stack lexbuf }
| "/*"          { copy lexbuf; in_block_com cand stack (col+2) lexbuf }
| ['(' '{' '['] { copy lexbuf; scan cand (Src::stack) (col+1) lexbuf }
| [')' '}' ']'] { let stack' = pop_src (pop_virt stack)
                  in copy lexbuf; scan cand stack' (col+1) lexbuf }
| [',' ';']     { let stack' = pop_virt stack
                  in copy lexbuf; scan cand stack' (col+1) lexbuf }
| '#' blank* ("line" blank+)? (decimal as d)
     { copy lexbuf; end_indicator cand stack (int_of_string d) lexbuf }
| _  { copy lexbuf;
       match cand with
         [] -> scan [] stack (col+1) lexbuf
       | (_,(stop_line,stop_col))::cand' ->
         if stop_line = !virt_lnum && stop_col = col
         then (output_string !out_chan
                             (" (pos(" ^ string_of_int stop_line ^ ","
                              ^ string_of_int col ^ "),");
               scan cand' (Virt::stack) (col+1) lexbuf)
         else scan cand stack (col+1) lexbuf }

(* Line indicator (#line n "foo.cc" | # n "foo.cc") *)

and end_indicator cand stack n = parse
  blank* newline { copy lexbuf; handle_nl lexbuf;
                   scan cand stack 0 lexbuf }
| blank* "//"    { copy lexbuf; in_line_com cand stack lexbuf }
| blank+ '"'     { copy lexbuf;
                   cur_file := snd (in_string 0 [] lexbuf);
                   until_eol lexbuf;
                   (if !cur_file = !src_file then virt_lnum := n);
                   scan cand stack 0 lexbuf }
| _              { assert false }

and until_eol = parse
  newline { handle_nl lexbuf }
| eof     {}
| _       { until_eol lexbuf }

(* Inline comment *)

and in_line_com cand stack = parse
  newline { copy lexbuf; handle_nl lexbuf; scan cand stack 0 lexbuf }
| eof     {}
| _       { copy lexbuf; in_line_com cand stack lexbuf }

(* Strings *)

and in_string len acc = parse
  "\\\""  { copy lexbuf; in_string (len+1) ('"'::acc) lexbuf }
| '"'     { copy lexbuf; len, mk_str len acc }
| newline { assert false }
| eof     { assert false }
| _ as c  { copy lexbuf; in_string (len+1) (c::acc) lexbuf }

(* Block comment *)

and in_block_com cand stack col = parse
  newline { copy lexbuf; handle_nl lexbuf; in_block_com cand stack 0 lexbuf }
| "*/"    { copy lexbuf; scan cand stack (col+2) lexbuf }
| eof     { assert false }
| _       { copy lexbuf; in_block_com cand stack (col+1) lexbuf }

{
(* Standalone scanner *)

let find_rvalues ~(cand: Lexer.interval list) ~(src:string) ~(out:string) =
  match open_in src, open_out out with
    cin, cout ->
      let buffer = Lexing.from_channel cin
      in begin
           src_file := src;
           cur_file := src;
           out_chan := cout;
           output_string cout "void pos (int line, int col) {}\n\n";
           scan cand [] 0 buffer;
           flush_all ();
           close_in cin;
           close_out cout
         end
  | exception Sys_error msg -> prerr_endline msg; exit 1
}
