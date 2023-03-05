(* Instrumenting assignments to C++ variables of type float, so the
   assigment is also performed in double precision

   Given a C++ file, the purpose of this lexer is to locate a given
   line which is supposed to hold an assignment to a variable or array
   cell of type float, and then rewrite the C++ code in order to
   duplicate that assignment but in double precision (type "double"),
   without repeating any function call in the right-hand side (RHS),
   lest they imply side effects.

   Two kinds of left-hand sides (LHS) are covered: a variable (e.g.,
   "x = ..."), or a monodimensional array cell (e.g., "x[i] = ...").
   In addition, return expressions are also handled (e.g.,
   "return ...").

   The inputs are given on the command line in the following order:
   the C++ source file, the line number of an assignment in that file,
   the name of the output C++ file and the name of the file where the
   values assigned are dumped. (See file main.ml.) The output C++ file
   is identical to the input file, except where needed to compute the
   assignment at the given line number with type double. The dump file
   contains the values (in double precision), with additional
   information, for instance, the value of the index when dumping an
   array cell (e.g., the "i" in "a[i]").

   There are three logical parts added to the original C++ code.

   First, there is the declaration of the dumper, which is done at the
   start of the function containing the target line. Currently, there
   is no parameterisation beyond the name of the dump file. Assuming
   that the target line number is 123, that the dump file is named
   "foo.dump" and that the variable on the LHS is a cell of the array
   "LnStp", we would have the following declaration:

   bff::extractor::dump_t HCD_dump1;
   HDC_dump1.setStreamFile("foo.dump");
   HDC_dump1.setWriteBinary(true);
   HDC_dump1.setFileName(__FILE__);
   HDC_dump1.setLineNum(123);
   HDC_dump1.setVarName("LnStp");

   The variable "HDC_dump1" has been automatically generated, and the
   prefix "HDC_dump" is not parameterisable yet, but easily located in
   this source file.

   Second, there is the declaration of the new assignment in double
   precision and the export of its LHS (to the dump file). For
   instance, given the target line

   LnStp[start+N] += std::log(sdiv/Stp[start+N]);

   the present software rewrites it as follows:

   float HDC_tmp1 = std::log(sdiv/Stp[start+N]);
   double HDC_tmp2 = (double)LnStp[start+N] + (double)HDC_tmp1;
   HDC_dumper1.dump(start+N, HDC_tmp2, false);
   LnStp[start+N] += HDC_tmp1;

   Notice how the call to std::log is shared between the two versions
   of the line, and it is performed at the original precision, that
   is, float.

   Lastly, the dump counter must be incremented if the LHS is an array
   cell (as in the example we are unravelling). This must be done
   right after the close of the innermost enclosing loop. So, we have
   the scope

   ... f (...) {
     if (...) {
       for (...) {
         ...
         if (...) {
           if (...) {
            ...
           }
           else
           {
            ...
            LnStp[start+N] += std::log(sdiv/Stp[start+N]);
           }
         }
       }
     }
   }

   this tool rewrites, in total:

   ... f (...) {
     bff::extractor::dump_t HCD_dump1;
     HDC_dump1.setStreamFile("foo.dump");
     HDC_dump1..setWriteBinary(true);
     HDC_dump1.setFileName(__FILE__);
     HDC_dump1.setLineNum(123);
     HDC_dump1.setVarName("LnStp");
     if (...) {
       for (...) {
         ...
         if (...) {
           if (...) {
            ...
           }
           else
           {
            ...
            float HDC_tmp1 = std::log(sdiv/Stp[start+N]);
            double HDC_tmp2 = (double)LnStp[start+N] + (double)HDC_tmp1;
            HDC_dumper1.dump(start+N, HDC_tmp2, false);
            LnStp[start+N] += HDC_tmp1;
           }
         }
       }
       HDC_dumper1.incrementCounter();
     }
   }

   This pattern is complicated by the fact that the targeted
   assignment may be the single instruction in the body of a loop or a
   conditional, therefore a block has to be created around the
   rewrite. (See below the discussion about explicit and implicit
   blocks, and scoping in general.)

   Design overview

   For the sake of clarity, two passes on the C++ source code are
   performed, each being implemented by a scanner in this ocamllex
   specification.

   First, a scanner named [scan] is run. Its first task is to identify
   the first assignment on the given line. Then, if not a return
   statement, it extracts the left-hand side (a variable, an array
   cell), and the right-hand side.

   We need contextual information because we want to declare the
   dumper at the start of the current function, but also because we
   must increment the counter of the dumper (in case we dump an array
   cell) just after the close of the innermost enclosing loop, in
   order to distinguish how a single cell is modified at each run of
   the loop. In order to obtain this non-local information, we thread
   a state through all the parser (lexer) calls. A state gathers in a
   a record all we know about the input parsed so far, like, in this
   instance, the stack of opened blocks making up the current scope.

   The scanner [scan] results in a series of edits to be applied to
   the source file.

   The second pass is implemented by the scanner named [apply] which
   applies these edits and produce the rewritten C++ file.
*)

(* SHORT INTRODUCTION TO OCAMLLEX

   This is a specification for ocamllex, the lexer generator from the
   standard distribution of the OCaml system. See

   http://caml.inria.fr/pub/docs/manual-ocaml/lexyacc.html

   Note: In the following comments, escaped OCaml code is written
   between square brackets. Also, we will use the terms "lexer",
   "parser" and "rule" interchangeably.

   In short, the format of an ocamllex specification is this:

   { header }

   let ident = regexp ...

   rule entrypoint [arg_1 ... arg_n] =
     parse regexp { action }
         | ...
         | regexp { action }

   and entrypoint [arg_1 ... arg_m] =
     parse ...

   and ...

   { trailer }

   The header, the actions and the trailer are OCaml code, whilst the
   regexp are regular expressions. Each entrypoint defines a lexer
   (despite the keyword "parse"), which may receive arguments (arg_1
   ... arg_n), followed by an implicit argument called [lexbuf], which
   is the lexing buffer against which are the regular expressions
   matched.

   The standard library [Lexing] enables access to the contents of the
   lexing buffer and the state of the lexing engine. See

   http://caml.inria.fr/pub/docs/manual-ocaml/libref/Lexing.html
*)

{
(* HEADER *)

(* STRING PROCESSING *)

(* The call [mk_rev_str p] is the string corresponding to the list of
   characters [p], in reverse order. For instance [mk_rev_str
   ['a';'b'] = "ba"]. *)

let mk_rev_str (p: char list) : string =
  let len = List.length p in
  let s = Bytes.make len ' ' in
  let rec fill i =
    function [] -> s | c::l -> Bytes.set s i c; fill (i-1) l
in Bytes.to_string (fill (len-1) p)

(* The call [mk_str p] is the string corresponding to the list of
   characters [p]. For example, [mk_str ['a';'b'] = "ab"]. *)

let mk_str (p: char list) : string =
  let len = List.length p in
  let s = Bytes.make len ' ' in
  let rec fill i =
    function [] -> s | c::l -> Bytes.set s i c; fill (i+1) l
in Bytes.to_string (fill 0 p)

(* The call [explode s a] is the list made by pushing the characters
   in the string [s] on top of [a], in reverse order. For example,
   [explode "ba" ['c';'d'] = ['a'; 'b'; 'c'; 'd']]. *)

let explode s acc =
  let rec push = function
    0 -> acc
  | i -> s.[i-1] :: push (i-1)
in push (String.length s)

(* The call [push_back buf acc] reads the characters last matched in
   the lexing buffer [buffer] and pushes them on top of the stack
   [acc], in reverse order of matching. For instance, if the
   characters matched are "ab", the call has the same value as
   [explode "ab" acc]. *)

let push_back buffer acc =
  let open Lexing in
  let rec push = function
    0 -> acc
  | i -> lexeme_char buffer (i-1) :: push (i-1)
in push (lexeme_end buffer - lexeme_start buffer)


(* SCOPE DEFINITION *)

(* The type [block] models the current scope, which is based on blocks
   only (for our present purpose).

   The data constructor [Exp] stands for any left bracket present in
   the source code, whereas [Imp] represents an implicit left bracket
   in case a block is optional, e.g., around a loop body made of a
   single instruction, or a conditional alternative. In the former
   case, the location is that of the character just after '{'; in the
   latter case, the location in the source is that of the first non
   blank character after the closing parenthesis of the loop header.

   A location is simply a pair of integer, the first denoting the line
   (the first line being numbered 1), and the second denoting the
   column (the first column being numbered 0, which makes it more like
   an offset, but we will use the term "column", as done in some text
   editors like Emacs.

   Consider the following excerpt (we assume that the the file starts
   with "void f ..."):

void f (int m, bool b) {
  for (int i = 0; i < m; ++i) {
    if (b) while (int j = 0; j < i; ++j) a[i] = x;
  }
}

  An explicit block is opened at line 1 and column 24, another is at
  line 2 and column 31, an implicit block at line 3 and column
  11, another at line 3 and column 41. Then _two_ implicit blocks are
  closed at line 3 and column 49 (at the semicolon), and
  an explicit block is closed at line 4 and column 3, an explicit block
  is closed at line 5 and column 1.

  The function [loc_lt] ("location lower than") compares two locations
  for inequality, and is simply defined as a lexicographic ordering on
  the locations.
*)

type line = int
type column = int
type location = line * column
type kind = Loop | Gen

let loc_lt (l1,c1) (l2,c2) = l1 < l2 || (l1 = l2 && c1 < c2)

type block =
  Nil
| Exp of location * kind * block
| Imp of location * kind * block


(* STATE DEFINITION AND OPERATIONS *)

(* The buffer state contains all the necessary information compiled so
   far about the input. It is threaded to all parsers, and updated by
   all (at least, the current column is always updated). More
   precisely, it is a record gathering the following fields:

    * field [src_file]: the name of the source file (before macro
      expansion);

    * field [dump_file]: the name of the file where the extracted data
      are dumped;

    * field [out_chan]: the name of the output channel where the
      transformed C++ source is written;

    * field [scope]: a linear structure of blocks, modelling the
      current scope (a valid prefix of a nested block structure);

    * field [prefix]: the characters since the beginning of the
      current line, up to the current location, but not past the
      left-hand side of an assignment (that is, the left-hand side is
      not in [prefix], in reverse order of reading (stack order) --
      see figure below;

    * field [line]: the current line (the first is 1);

    * field [col]: the current column (the first column is 0);

    * field [target]: the number of the targeted line in the C++
      source.
*)

type state = {
  src_file  : string;
  dump_file : string;
  out_chan  : out_channel;
  scope     : block;
  prefix    : char list;
  line      : int;
  col       : int;
  target    : int
}

(* The aim is to segment the target line into the following zones:

   +--------+-----+----------+-----+---+-------+----+
   | prefix | LHS | operator | RHS | ; |  ...  | \n |
   +--------+-----+----------+-----+---+-------+----+

   The prefix is reversed simply because it is made by pushing the
   characters read from left to right on top of a stack. The left-hand
   side (LHS) should either be a variable or an access to an array
   cell. The operator is one of "=" | "+=" | "-=" | "*=" | "/=" (see
   the auxiliary regexp [oper] below). The right-hand side (RHS) is
   expected to be an arithmetic expression on floating-point
   numbers. In case a return statement is the target, the operator is
   the string "return" and the LHS is empty. For example,

   +--------------+------+----+-------+---+-------------+----+
   |    if (true) | a[i] | += | x * y | ; | // comment  | \n |
   +--------------+------+----+-------+---+-------------+----+

   Note that the line may contain breaks, and that the scanner will
   not look for an operator in places which are known not to be of
   normal interest, like in the header of a "for" loop.
*)

(* The value of the call [skip state lexbuf] is the state after the
   current state [state] if we want to ignore the string matched in
   the lexing buffer [lexbuf]. Accordingly, only the line and column
   are updated. Contrast this with [update state lexbuf] below, where
   the prefix is also managed.

   Note: The call [Lexing.lexeme_end lexbuf] evaluates in the offset
   in the input stream of the character following the last character
   of the matched string. The first character of the stream has offset
   0. The call [Lexing.lexeme_start lexbuf] evaluates in the offset in
   the input stream of the first character of the matched
   string. Therefore the variable [offset] below is indeed the length
   of the matched string.

   See http://caml.inria.fr/pub/docs/manual-ocaml/libref/Lexing.html
*)

let skip state lexbuf =
  let open Lexing
in match lexeme lexbuf with
     "\n" | "\r" -> new_line lexbuf;
                    { state with line=state.line+1; col=0 }
   | _ -> let offset = lexeme_end lexbuf - lexeme_start lexbuf
          in { state with col = state.col + offset }

(* The value of the call [update state lexbuf] is the state after the
   current state [state], after a string has been matched in the
   lexing buffer [lexbuf]. First, the current line and column are
   updated by calling [skip], then the prefix receives the matched
   string (reversed and character by character in a list). *)

let update state lexbuf =
  let state = skip state lexbuf in
  let open Lexing
in if state.col = 0
   then { state with prefix = [] }
   else { state with prefix = explode (lexeme lexbuf) state.prefix }

(* The call [copy state lexbuf] copies the matched string in the
   lexing buffer [lexbuf] to the output channel recorded in the state
   [state] (as [state.out_chan]), and it updates [state] as
   usual. (See function [skip] above.) *)

let copy state lexbuf =
  output_string state.out_chan (Lexing.lexeme lexbuf);
  skip state lexbuf


(* SCOPE OPERATIONS *)

(* As explained above, in order to correctly place the increment to
   the dumper counter, we need to keep track of the current scope
   during the scanning of the C++ source code. We have seen that we
   expect to find two kinds of blocks: either those which actually
   occur in the code as a pair of curly brackets { ... }, or those
   which are implicit, as in a loop or a conditional whose body is a
   single instruction.

   The call [push_eblock kind state] evaluates in the state after the
   current state [state], after an explicit block has just been
   recognised (by the scanner). It records with the value of the
   parameter [kind] whether it is either a loop or a general block (we
   do not distinguish the blocks which are bodies of conditionals).

   The call [push_iblock kind state] is made when encountering the
   opening of an implicit block.

   The dual functions for popping blocks from the scope when they
   close, are [pop_eblock] and [pop_iblock], and are self-evident.
*)

(* The function [trace_scope] is used for debugging only. *)
(*
let trace_scope fname state =
  print_string fname; print_string " at line "; print_int state.line;
  print_string " and column "; print_int state.col; print_newline ()
*)

let get_loc state = state.line, state.col

let push_eblock kind state =
(*  trace_scope "push_eblock" state; *)
  { state with scope = Exp (get_loc state, kind, state.scope) }

let push_iblock kind state =
(*  trace_scope "push_iblock" state; *)
  { state with scope = Imp (get_loc state, kind, state.scope) }

let pop_eblock state =
(*  trace_scope "pop_eblock" state; *)
  match state.scope with
    Exp(_,_,s) -> { state with scope=s }
  |          _ -> assert false

let pop_iblock state =
(*  trace_scope "pop_iblock" state; *)
  match state.scope with
    Imp(_,_,s) -> { state with scope=s }
  |          _ -> assert false

(* The call [outermost_block state] evaluates in the location of the
   opening bracket of the current function block. In other words, it
   finds the outermost opened block in the current scope. As explained
   above, that location is needed to place the declaration of the
   dumper. *)

let outermost_block state =
  let rec aux = function
    Exp (loc,_,Nil) -> loc
  | Exp (_,_,scope)
  | Imp (_,_,scope) -> aux scope
  |             Nil -> assert false
in aux state.scope

(* The predicate [enclosing_loop state] is [true] if, and only if,
   there is an enclosing loop in the current scope, that is, we are
   located inside an open loop body. It is needed to properly place
   the increment of the dumper in case we dump the value of an array
   cell. *)

let enclosing_loop state =
  let rec aux = function
     Exp(_,Loop,_) | Imp(_,Loop,_)  -> true
  | Exp(_,_,scope) | Imp(_,_,scope) -> aux scope
  |                             Nil -> false
in aux state.scope


(* SYMBOL GENERATORS *)

(* We have two generators: one for the temporary variables holding the
   copy of the assigned variable in double precision and the values of
   the function calls possibly found in the RHS. Note that the
   variables and the dumpers are prefixed with the strings "HDC_tmp"
   and "HDC_dumper", respectively. (In a future version, these
   prefixes should be given on the command line.)

   The function [gen_sym] is only called in the parser [scan] for
   generating a variable of type double, dual of the original float
   variable assigned in the target line, and in the parser [read_rhs],
   when a function call has been found and needs to be replaced by a
   fresh variable.

   The function [gen_dump] is solely called in the parser [scan], to
   produce a name for the dumper.
*)

let var_pref = "HDC_tmp" and dump_pref = "HDC_dumper"

let gen_sym =
  let count = ref 0
in fun () -> incr count; var_pref ^ string_of_int !count

let gen_dump =
  let count = ref 0
in fun () -> incr count; dump_pref ^ string_of_int !count


(* LEFT-HAND SIDES OF ASSIGNMENTS *)

(* The type [lhs] models the left-hand side (LHS) of the target line
   (see header for the format and an example).

   The value [Var (v,o)] models the LHS made of a C++ variable denoted
   by [v], followed by the assignment operator [o]. The value [Array
   (v,i,o)] denotes the LHS made of a C++ array variable [v], indexed
   by [i], and followed by the assignment operator [o]. The constant
   constructor [Ret] models a return statement.
*)

type operator = string
type variable = string
type index = string

type lhs =
  Var   of variable * operator
| Array of variable * index * operator
| Ret

(* Accessing variables and converting LHS to [string] *)

let get_var = function Var (v,_) | Array (v,_,_) -> v | Ret -> "return"

let string_of_lhs = function
        Var (lhs,op) -> lhs ^ " " ^ op
| Array (var,idx,op) -> var ^ "[" ^ idx ^ "] " ^ op
|                Ret -> "return"

(* Segmenting the LHS from the state prefix (see HEADER above for an
   example), that is, the list of characters read from the start of
   the target line, in reverse order. The call [read_lhs state op]
   evaluates into a value of type [lhs]. *)

let read_lhs state op =
  let rec array acc = function
        [] -> assert false
  | '['::l -> l, mk_str acc
  |   c::l -> array (c::acc) l

  and variable acc = function
    ('a'..'z'|'A'..'Z'|'0'..'9'|'_' as c)::l -> variable (c::acc) l
  | l -> mk_str acc, op, {state with prefix=l} in

  let rec aux = function
    ']'::l -> let pre, index = array [] l in
                let var, op, state = variable [] pre
              in Array (var, index, op), state
  | ' '::l -> aux l
  | ('a'..'z'|'A'..'Z'|'0'..'9'|'_' as c)::l ->
      let var, op, state = variable [c] l
      in Var (var, op), state
  | _ -> assert false

in match op with
     "return" -> Ret, state
   |        _ -> aux state.prefix

(* Pretty-printing

   The function [drop_ass] is only defined for the assigmnent
   operators which are combined with a floating-point operation, and
   it returns the string corresponding to that operation. Note that we
   took care of adding an opening parenthesis for the rest of the RHS,
   in case the operator has a high priority (multiplication and
   division).

   The function [new_lhs] construct the first half of the version of
   the targeted assignment in double precision. The parameter [d_var]
   is the generated name of the new variable of type double, which is
   the dual of the original assigned variable of type float.

   What remains to be appended is the RHS where any function call has
   been replaced by a temporary variable (and optionally, a closing
   parenthesis if the assignment is combined with a multiplication or
   a division). See call to [new_lhs] below for the context.
*)

let drop_ass = function
  "+=" -> " +" | "-=" -> " -" | "*=" -> " * (" | "/=" -> " / ("
| _ -> assert false

let new_lhs d_var = function
  Ret | Var (_,"=") | Array (_,_,"=") -> "double " ^ d_var ^ " ="
| Var (var,op) -> "double " ^ d_var ^ " = (double)" ^ var ^ drop_ass op
| Array (var,idx,op) -> "double " ^ d_var ^ " = (double)"
                        ^ var ^ "[" ^ idx ^ "]" ^ drop_ass op

(* EDITS *)

(* The result of the first pass on the C++ source file results in a
   series of edits, sorted by increasing locations where they
   apply. Implicitly, the input file is copied to the output file
   until the current edit is triggered by the current location in the
   input. There are two sorts of edits: one writes a string to the
   output when a given location in the input is reached, another
   interrupts the copying process, while the input continues to be
   read, amounting to skipping a section of the input.

   The type [edit] captures the edits we need to instrument the
   original C++ source. The empty edit is the constant constructor
   [Null]. The value [Skip (l1,l2,e)] represents the edit which skips
   the text in the input from location [l1] to location [l2], followed
   by the edit [e] to be applied next. The value [Write (l,s,e)]
   denotes the edit which writes the string [s] to the output once the
   location [l] has been reached in the input, followed by the edit
   [e] to be applied next.
*)

type edit =
  Null
| Skip  of location * location * edit
| Write of location * string * edit

(* Pretty-printing *)
(*
let string_of_loc (line,col) = string_of_int line, string_of_int col

let rec string_of_edit = function
  Null -> "*** End."
| Skip (start,stop,edit) ->
    let start_line, start_col = string_of_loc start
    and stop_line, stop_col = string_of_loc stop
    in "*** Skip from line " ^ start_line ^ " and column " ^ start_col
       ^ " to line " ^ stop_line ^ " and column " ^ stop_col ^ ".\n"
       ^ string_of_edit edit
| Write (loc,patch,edit) ->
    let line, col = string_of_loc loc
    in "*** Write when line=" ^ line ^ " and column=" ^ col
       ^ " the following text:\n[start of text]\n"
       ^ patch ^ "\n[end of text]\n" ^ string_of_edit edit
*)

(* Reversing an edit (because edits to be applied first are created
   first). *)

let rev_edit =
  let rec rev acc = function
                      Null -> acc
  | Skip (start,stop,edit) -> rev (Skip (start,stop,acc)) edit
  | Write (loc,patch,edit) -> rev (Write (loc,patch,acc)) edit
in rev Null

(* The function [patch] creates a patch, that is, the composition of a
   skip immediately followed by a write, for the target line. *)

let patch state block edit =
  let p = match state.scope with
    Imp _ -> mk_rev_str state.prefix ^ "{\n" ^ block ^ "\n}"
  | Exp _ -> mk_rev_str state.prefix ^ block
  |   Nil -> assert false
in Skip((state.target,0), get_loc state, Write((state.target,0), p, edit))


(* DUMPERS *)

let dumping dump var = function
  Array (_,idx,_) -> dump ^ ".dump(" ^ idx ^ ", " ^ var ^ ", false);"
|     Var _ | Ret -> dump ^ ".dump(" ^ var ^ ");"

(* The function [dump_decl] makes the edit which is meant to be
   written just after the block of the current C++ function is
   opened. That location is determined by a call to
   [outermost_block]. See HEADER for an example. *)

let dump_decl dump state lhs =
  Write (outermost_block state,
         "\nbff::extractor::dump_t " ^ dump ^ ";\n"
         ^ dump ^ ".setStreamFile(\"" ^ state.dump_file ^ "\");\n"
         ^ dump ^ ".setWriteBinary(true);\n"
         ^ dump ^ ".setFileName(__FILE__);\n"
         ^ dump ^ ".setLineNum("
         ^ string_of_int state.target ^ ");\n"
         ^ dump ^ ".setVarName(\"" ^ get_var lhs ^ "\");\n",
         Null)


(* LEXER ENGINE *)

(* Hack to roll back one lexeme in ocamllex buffer (should be safe if
   used in the semantic actions of the regular expression recognising
   the lexeme rolled back. Very useful! *)

let rollback lexbuf =
  let open Lexing in
  let len = String.length (lexeme lexbuf)
in lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - len;
   lexbuf.lex_curr_p <- {lexbuf.lex_curr_p
     with pos_cnum = lexbuf.lex_curr_p.pos_cnum - len}

(* END OF HEADER *)
}

(* REGULAR EXPRESSIONS FOR SOME C++ LITERALS *)

(* Assignment operators of interest *)

let oper = "=" | "+=" | "-=" | "*=" | "/=" (* "++" | "--" *)

(* Operators to be ignored *)

let no_op =
  ">=" | "<=" | "!=" | "==" | ">>=" | "<<=" | "&=" | "^=" | "|=" | "%="

(* White space *)

let nl = ['\n' '\r'] (* | "\r\n" *)
let blank = [' ' '\t']

(* Integers *)

let digit = ['0'-'9']
let dec = digit+

(* Floating-point literals *)

let frac = '.' dec | dec? '.' dec
let exp = ['e' 'E'] ['+' '-']? dec
let suf = ['f' 'l' 'F' 'L']
let float = frac exp? | dec exp

(* Identifiers *)

let letter = ['a'-'z' 'A'-'Z']
let start = '_' | letter
let alphanum = letter | digit | '_'
let ident = start alphanum*

(* RULES *)

(* The lexer [scan] is the entry point for the first pass, that is,
   generating a edit sequence. It takes two arguments: [scan state
   lexbuf]. Because we do not perform a prior pass lexing all the
   tokens, we need to recognise some tokens ourselves, simply to
   ignore them, like comments and strings -- both of which may contain
   irrelevant "assignments". That explains a lot of redundancy between
   the following lexing functions.

   Returning to [scan], its first case recognises a double quote, and
   the lexer [in_string] is called to parse the starting string. Note
   that we ignore the contents of the string ([let _, state =
   ...]). Note that we do not initialise the accumulator of
   [in_string] to the list ['"'], but simply [[]], because we do not
   care of the contents (in other words, the opening double quote will
   be missing, but we do not care).

   Let us keep in mind that the state [state] is threaded to all the
   lexers, which must take care of updating it depending of what has
   been parsed.

   The second case is actually a disjunction of particular cases, of
   the characters '=', '{', '}' and ';' which must not be mistaken for
   an assignment, a block opening, a block closing or the potential
   closing of an implicit block. (See below for these cases.)

   The third and fourth cases deal with the lexing of a line and block
   comment, respectively. Notice how we call [skip] instead of
   [update] on the current state, as we do not want to retain the
   comment in the prefix of the current line.

   The fifth case is that of the loops "for" and "while". First, their
   header must be skipped by calling [parens], which reads and stores
   everything until the closing parenthesis. Then the lexer [block] is
   called, whose aim is to determine whether the header is followed by
   an explicit block, that is '{', or not (in case of a single
   instruction). The scope is update depending on the situation, and
   the nature of the block, being a loop body, is also recorded. (See
   rule [block] below.)

   The sixth case is for the "do" loops, which expect a block
   immediately after the keyword "do".

   The seventh case is about conditionals. The corresponding action is
   almost identical to that of the "for" and "while" loops, except for
   the kind being "Gen" (see type [kind] above).

   The eight and ninth cases deal with explicit blocks: the former
   pushes a new (explicit) block on the scope, the latter pops one.

   The tenth case is special in that it determines whether a semicolon
   is actually closing an implicit block, as, for instance, in

   if (b) a[i] = x; // the implicit block contains the assignment

   If so, we roll back the semicolon into the lexing buffer (that is,
   we have the lexer engine forget that it has just read it), then we
   pop an implicit block and resume scanning. If not the closing of an
   implicit block, the state is updated as usual and scanning is
   resumed.

   The eleventh case (regular expression [no_op]) recognises some
   operators that may be mistaken for an assignment because they
   contain the symbol '=' in them. This case is necessary in order to
   get rid of these operators in their entirety, otherwise '<=' would
   be read as '<' and then '=' would be considered mistakenly as an
   assignment.

   The twelfth case is twofold: it recognises the operators of
   interest (by means of the regular expression [oper]) and the
   special case of a return statement. Let us comment line by line,
   or, rather, binding by binding ([let x=y in z] binds [x] to [y] in
   [z]).

   First, we determine whether we have reached the target line in the
   parsed source. If not, we simply call [scan] recursively, after
   updating the state as usual. If so, the work of creating the edits
   begins.

   The current state is updated _without_ the operator or the
   "return" keyword just recognised, that is why we call [skip]
   instead of [update]. The reason is that we have completely
   recognised the LHS (that is, everything since the beginning of the
   target line up to the assignment), we want to keep it invariant --
   which [update] does not.

   Then, the function [read_lhs] is called with the updated state. As
   seen above, that function segments the prefix of the current line
   up to the LHS, and produces a value of type [lhs], bound here to
   the variable called [lhs].

   Next, a fresh name for the dumper is generated by [gen_dump] and
   bound to the variable [dump].

   Then, the first edit is made by a call to [dump_decl], a function
   whose purpose is to build the edit which declares the dumper at the
   start of the current C++ function. This first edit is called [ed],
   and will be shadowed with further bindings to [ed], when a new edit
   is added (this way, we don't end up with [ed1], [ed2], [ed3]
   etc. which are used only once).

   We have described the first four group of bindings ("let in"),
   which dealt with the LHS and the dumper declaration. The next group
   is separated by a blank line and deals with the RHS.

   Let us recall that we stopped scanning the input after reading the
   operator. Now, the RHS is parsed by a call to [read_rhs]. That
   parser returns a triple: the RHS [rhs] where any function call has
   been replaced by a temporary variable, the declaration [aux] of
   those variables, initialised with their corresponding function
   calls, and the current, updated state (always called [state], to
   avoid [state1], [state2], etc. which are used only once).

   We need now to duplicate the RHS and cast its variables to the type
   double. This is the purpose of the call to the lexer
   [cast_to_double], which operates on a lexing buffer made from
   [rhs]. As a result, we bind the RHS in double precision to the
   variable [d_rhs].

   At this point, we have processed the LHS and the RHS. The following
   series of bindings deal with gathering the previous information and
   constructing the next edit.

   We generate a fresh variable for the variable in double precision,
   bound to [d_var].

   We build the line in double precision by appending the value of the
   call [new_lhs d_var lhs] to [d_rhs], and bind it to [double]. The
   call evaluates into the LHS of the new line in double precision,
   whilst [d_rhs] is the RHS already made. Note that we have to check
   whether we need to close a parenthesis after the new RHS, in case
   the assignment operator was "*=" or "/=" (see function [drop_ass]
   above).

   Next, we put together any auxiliary variable holding a function
   call ([aux]), the line in double precision ([double]) and the
   dumping of the variable in double precision ([dumping dump d_var
   lhs]). The result is the string called [com] (for "common", because
   it is common to the distinct cases that follow).

   Finally, we need to reconstruct the original line, without function
   calls: this is done by appending [rhs] to [string_of_lhs lhs],
   bound to [ass'].

   We are now done with the strings to be used for the edits, so it
   remains to actually build the corresponding edit with them, which
   is the purpose of the next series of bindings.

   The edit [ed] depends on the shape of [lhs]. Indeed, if we have a
   variable or a return statement, we do not need to need to increment
   the internal counter of the dumper, and, if we have an array
   access, the location of the increment depend on there being an
   enclosing loop above the assignment: if so, the increment must be
   performed right after the body of the loop is closed, otherwise, it
   can be done on the spot. Let us walk again through these cases with
   the code in view.

   First, in case of a variable or a return statement, we call [patch
   state (com^ass') ed], which creates a patch (that is, a skip
   immediately followed by a write) for the target line (see functiona
   definition) with the declarations [com^ass'] (optionally creating
   an explicit block if the current location is inside an implicit
   block).

   Otherwise, we are dealing with an array access. We start by
   creating the C++ line incrementing the internal counter of the
   dumper, and bind that string to [idx_incr]. We then proceed to
   check whether there exists an enclosing loop in the current scope,
   by means of a call to [enclosing_loop]. If not, we create a patch
   like for variables and return statements, except that it includes
   the increment of the internal counter of the dumper. If there is a
   loop, we need to locate its end, which we do by calling
   [end_of_loop]. That call returns a new state, which we use to
   create a separate write edit for the increment, as it cannot be
   included in the patch.

   Finally, the edits are reversed by the call [rev_edit ed], as we
   composed them by increasing order of locations in the source file,
   and the second pass of this tool will restart at the beginning of
   that file.
*)

rule scan state = parse
  '"'   { let _, state = in_string (update state lexbuf) [] lexbuf
          in scan state lexbuf }
| "'='" | "'{'" | "'}'" | "';'"
        { scan (update state lexbuf) lexbuf  }
| "//"  { let state = skip state lexbuf
          in scan (in_line_com state lexbuf) lexbuf }
| "/*"  { let state = skip state lexbuf
          in scan (in_block_com state lexbuf) lexbuf }
| ("for" | "while") blank* '('
        { let _,_,state =
            parens (update state lexbuf) update [] [] 0 lexbuf
          in scan (block state Loop lexbuf) lexbuf }
| "do"  { scan (block (update state lexbuf) Loop lexbuf) lexbuf }
| "if" blank* '('
        { let _,_,state =
            parens (update state lexbuf) update [] [] 0 lexbuf
          in scan (block state Gen lexbuf) lexbuf }
| '{'   { scan (push_eblock Gen (update state lexbuf)) lexbuf }
| '}'   { scan (pop_eblock (update state lexbuf)) lexbuf }
| ';'   { match state.scope with
            Imp _ -> rollback lexbuf; scan (pop_iblock state) lexbuf
          |     _ -> scan (update state lexbuf) lexbuf }
| no_op { scan (update state lexbuf) lexbuf }
| (oper | "return") as op
        { if state.target = state.line
          then
            let lhs, state = read_lhs (skip state lexbuf) op in
            let dump = gen_dump () in
            let ed = dump_decl dump state lhs in

            let rhs, aux, state = read_rhs state [] [] lexbuf in
            let d_rhs = cast_to_double [] (Lexing.from_string rhs) in

            let d_var = gen_sym () in
            let double = new_lhs d_var lhs ^ d_rhs
              ^ if op = "*=" || op = "/=" then ");" else ";" in
            let com = aux ^ double ^ "\n" ^ dumping dump d_var lhs
              ^ "\n" in
            let ass' = string_of_lhs lhs ^ rhs in

            let ed = match lhs with
              Ret | Var _ -> patch state (com^ass') ed
            | Array _ ->
                let idx_incr = "\n" ^ dump ^ ".incrementCounter();\n"
                in if enclosing_loop state
                   then let ed = patch state (com^ass') ed in
                        let state = end_of_loop state lexbuf
                        in Write (get_loc state, idx_incr, ed)
                   else patch state (com^idx_incr^ass') ed

            in rev_edit ed
          else scan (update state lexbuf) lexbuf }
| eof   { prerr_endline "Error: Wrong line number."; exit 1 }
| _     { scan (update state lexbuf) lexbuf }

(* The call [end_of_loop state lexbuf] assumes that there exists at
   least one loop in the current scope and returns the state after
   reaching the end of the first loop found in the current scope.

   The first three cases deal with lexing strings and comments. Note
   how we update the state by means of [skip] instead of [update]
   because we do not want to change the prefix, as it has outlived its
   usefulness by now.

   The fourth case deals with the occurrence of a block opening,
   yielding an explicit block to be pushed on the current scope. Note
   that we annotate this operation as a hack, because we specify that
   the block is general, not the body of a loop, although we do not
   know that: in fact, that does not matter here, as we solely concern
   ourselves with finding the end of the first enclosing loop.

   The fifth case handles the closing of a block. If the closed block
   is the body of a loop, we are done and simply update the state
   before returning it; if not, we pop an explicit block, update the
   state and resume the search for the end of a loop with a recursive
   call.

   The sixth case deals with the semicolon. This character is always
   distinguished when interpreting the scope because it may indicate
   the closing of an implicit block. Three situations may present
   themselves upon recognising a semicolon in the lexing
   buffer. First, if the current block is an implicit loop (that is,
   we are located within the unique instruction of a loop), we pop the
   current block (with [pop_iblock]) and update the state (with
   [skip]). Second, if the current block is implicit but not a loop
   (that is, it is a conditional), we roll back the semicolon in the
   lexing buffer and resume recursively the search, after popping the
   current block (with [pop_iblock]). Importantly, the state is _not_
   updated. This behaviour accounts for semicolons that close several
   implicit blocks, like

   while (int j = 0; j < i; ++j) if (b) a[i] = x;

   The third and last case is when the current block in scope is
   explicit or empty. Then, we update the state and resume recursively
   the search for the end of the first enclosing loop, because this
   means that we are inside a block "{ ..." and we just found ';',
   which is thus only a separator, with no impact on the scope.
*)

and end_of_loop state = parse
  '"'  { let _, state = in_string (skip state lexbuf) [] lexbuf
         in end_of_loop state lexbuf }
| "//" { let state = skip state lexbuf
         in end_of_loop (in_line_com state lexbuf) lexbuf }
| "/*" { let state = skip state lexbuf
         in end_of_loop (in_block_com state lexbuf) lexbuf }
| '{'  { let state = skip state lexbuf
         in end_of_loop (push_eblock Gen state) lexbuf } (* hack *)
| '}'  { match state.scope with
           Exp(_,Loop,_) | Imp(_,Loop,_) -> skip state lexbuf
         | _ -> let state = pop_eblock (skip state lexbuf)
                in end_of_loop state lexbuf }
| ';'  { match state.scope with
           Imp(_,Loop,_) -> skip (pop_iblock state) lexbuf
         |         Imp _ -> rollback lexbuf;
                            end_of_loop (pop_iblock state) lexbuf
         |             _ -> end_of_loop (skip state lexbuf) lexbuf }
| _    { end_of_loop (skip state lexbuf) lexbuf }


(* The call [block state kind lexbuf] tries to parse a block in the
   lexing buffer [lexbuf] with the state [state], by assuming that the
   current block is of kind [kind], that is, either a loop ([Loop]) or
   not ([Gen]). The parser [block] is called after the header of a
   loop or a conditional has been recognised in [scan], and we need to
   determine whether an explicit or an implicit block follows.

   The first case deals with markup (new lines and blank characters).

   The second and third cases process comments (which are dropped, as
   the update of the state by means of [skip] reveals).

   The fourth case identifies the opening of an explicit block (with
   '{'), therefore, an explicit block is pushed on the current scope
   and the state is updated.

   The fifth case is devoted to the semicolon, because it denotes and
   empty block (that is, the empty body of a loop or conditional). The
   character ';' is then rolled back into the lexing buffer and the
   state is _not_ updated, so it will be read again and interpreted
   again, possibly in a different context. (This is in accordance with
   the corresponding case in [end_of_loop], when the current block in
   scope is implicit, but not a loop.)

   The last and sixth case is a default case, with the read character
   being rolled back into the lexing buffer, but an implicit block
   being pushed on the current scope. (Contrast this with the previous
   case, where we do not change the scope, because the semicolon may
   close several scopes. See [end_of_loop] as well.)
*)

and block state kind = parse
  nl | blank { block (update state lexbuf) kind lexbuf }
| "//"       { let state = in_line_com (skip state lexbuf) lexbuf
               in block state kind lexbuf }
| "/*"       { let state = in_block_com (skip state lexbuf) lexbuf
               in block state kind lexbuf }
| '{'        { push_eblock kind (update state lexbuf) }
| ';'        { rollback lexbuf; state }
| _          { rollback lexbuf; push_iblock kind state }


(* The call [read_rhs state vdecl rpre lexbuf] parses the lexing
   buffer [lexbuf] and uses the accumulators [vdecl] and [rpre] to
   store auxiliary variable declarations receiving function calls, and
   the (reversed) prefix of the RHS, respectively.

   The first case parses strings.

   The second and third cases skim over comments.

   The fourth case identifies function calls. A fresh variable is
   generated with [gen_sym] and the parser [parens] is invoked to
   parse the arguments of the call. Let us look at the arguments of
   [parens]. (Please keep in mind that OCaml does not specify the
   order of evaluation of the arguments of a call.) First, the state
   is update with [skip]. The next argument is the parser [skip],
   which will be used by [parens] to update the current state. Indeed,
   we do not want to modify the prefix of the state (that is, the
   prefix of the current line up to the LHS). Next, the left-hand side
   of the declaration of the fresh variable is ["float " ^ var ^
   " = "] and added to the accumulator of declarations [vdecl]. Then,
   [push_back] is called, reading the string matched in the lexing
   buffer, which is the function call, and push its characters on top
   of previously extended accumulator. This way, we have now added an
   auxiliary variable with the recognised function call.

   The fifth case is the semicolon, which signals the end of the RHS
   we are parsing. Therefore, it is rolled back (because we do not
   want to have it in the RHS per se), and the value of the call is
   built: it is a triple whose first component is the RHS (as a
   string, in the normal order), the second is the declarations of
   auxiliary variables holding function calls, and the state (which is
   not updated because we rolled back the semicolon).

   The sixth case is the opening square bracket, that is, it indicates
   the start of an expression whose value is the index in an array
   access. Thus the parser [index1] is called to recognise that
   expression. (Technical note: the call is in tail position.)
*)

and read_rhs state vdecl rpre = parse
  '"' as c { let _, state = in_string (skip state lexbuf) [c] lexbuf
             in read_rhs state vdecl (c::rpre) lexbuf }
| "//"     { let state = in_line_com (skip state lexbuf) lexbuf
             in read_rhs state vdecl rpre lexbuf }
| "/*"     { let state = in_block_com (skip state lexbuf) lexbuf
             in read_rhs state vdecl rpre lexbuf }
| (ident? "::")? ident blank* '('
           { let var = gen_sym () in
             let vdecl, rpre, state =
               parens (skip state lexbuf) skip
                      (push_back lexbuf
                        (explode ("float " ^ var ^ " = ") vdecl))
                      (explode var rpre) 0 lexbuf
             in read_rhs state vdecl rpre lexbuf }
| ';' as c { rollback lexbuf; mk_rev_str (c::rpre), mk_rev_str vdecl, state }
| '[' as c { index1 (skip state lexbuf) vdecl (c::rpre) lexbuf }
| _ as c   { read_rhs (skip state lexbuf) vdecl (c::rpre) lexbuf }

(* The call [parens state up vdecl rpre level lexbuf] parses an
   excerpt of C++ source code after an opening parenthesis, up to the
   corresponding closing parenthesis.

   This may be required here in two different contexts: either we have
   not arrived yet to the targeted assignment (that is, we are parsing
   with [scan]), and we find a loop "for" or "while", or a conditional
   "if", followed by a parenthese; or, we are parsing the RHS (that
   is, with [read_rhs]) of the targeted assignment, and we have found
   a function call, whose argument we need to copy to be used in the
   declaration of an auxiliary variable. In the former case, we want
   to store what is read in the field [prefix] of the current state;
   in the latter, we do not want to modify that prefix. That is the
   rationale for the second parameter [up] of [parens], which is a
   parser itself, corresponding either to the argument [update] in the
   first call context, and to [skip] in the second one.

   Because arguments to a C++ call can include parentheses, we need to
   keep track of the level of embedding with [level], just as the
   parser [parens] does. The parameters [vdecl] and [rpre] correspond
   to their eponyms in [read_rhs].

   The first case expedites the new line character.

   The second case finds an opening parenthesis, and updates the
   embedding level accordingly ([level+1]).

   The third case is the closing parenthesis. If we are at level 0, it
   means that we are back out of the arguments in the C++ function
   call, and we can resume parsing the RHS, hence the call back to
   [read_rhs]; if we are not at level 0, we are still parsing
   arguments and we update the level of embedding accordingly
   ([level-1]).

   The fourth case deals with the special cases of the characters '('
   and ')', which could mislead us, as in [parens].

   Note that this is the only occurrence where we actually keep the
   parsed string.
*)

and parens state up vdecl rpre level = parse
  nl as c  { parens (up state lexbuf) up vdecl (c::rpre) level lexbuf }
| '(' as c { parens (up state lexbuf) up (c::vdecl) rpre (level+1) lexbuf }
| ')' as c { let state = up state lexbuf in
             if level = 0 then ('\n'::';'::c::vdecl), rpre, state
             else parens state up (c::vdecl) rpre (level-1) lexbuf }
| "'('" | "')'" as s
           { let state = up state lexbuf
             in parens state up (explode s vdecl) rpre level lexbuf }
| '"' as c { let s, state = in_string (up state lexbuf) [c] lexbuf
             in parens state up (explode s vdecl) rpre level lexbuf }
| "//"     { let state = in_line_com (up state lexbuf) lexbuf
             in parens state up vdecl rpre level lexbuf }
| "/*"     { let state = in_block_com (up state lexbuf) lexbuf
             in parens state up vdecl rpre level lexbuf }
| _ as c   { parens (up state lexbuf) up (c::vdecl) rpre level lexbuf }


(* Recognising the index of an array access in the RHS *)

and index1 state vdecl rpre = parse
  ']' as c { read_rhs (skip state lexbuf) vdecl (c::rpre) lexbuf }
| _ as c   { index1 (skip state lexbuf) vdecl (c::rpre) lexbuf }

(* The parser [cast_to_double] casts to double the variables in the
   RHS after function calls removal.

   The first case converts literal floats to literal doubles.

   The second case casts variables to double.

   The third case does _not_ cast struct fields.

   The fourth case identifies an array access and triggers the parsing
   of the index between square brackets.

   The fifth case terminates with the RHS finally cast to double
   precision.

   The sixth and last case is simply a recursive call augmenting the
   accumulator [rpre] with the character [c] just read.
*)

and cast_to_double rpre = parse
  (float as n) suf?
            { cast_to_double (explode n rpre) lexbuf }
| ident     { let rpre = push_back lexbuf (explode "(double)" rpre)
              in cast_to_double rpre lexbuf }
| '.' ident { cast_to_double (push_back lexbuf rpre) lexbuf }
| '[' as c  { index2 (c::rpre) lexbuf }
| ';'       { mk_rev_str rpre }
| _ as c    { cast_to_double (c::rpre) lexbuf }

and index2 rpre = parse
  ']' as c { cast_to_double (c::rpre) lexbuf }
| _ as c   { index2 (c::rpre) lexbuf }

(* Inline comment *)

and in_line_com state = parse
  nl  { update state lexbuf }
| eof { state }
| _   { in_line_com (skip state lexbuf) lexbuf }

(* Block comment *)

and in_block_com state = parse
  nl   { in_block_com (update state lexbuf) lexbuf }
| "*/" { skip state lexbuf }
| _    { in_block_com (skip state lexbuf) lexbuf }

(* Strings *)

and in_string state rpre = parse
  "\\\""   { in_string (update state lexbuf) ('"'::'\\'::rpre) lexbuf }
| '"' as c { mk_rev_str (c::rpre), update state lexbuf }
| _ as c   { in_string (update state lexbuf) (c::rpre) lexbuf }

(* Applying the patches

   The parser [apply] implements the second pass on the C++ source
   code. In the call [apply edits state lexbuf], the argument [edits]
   is the edits to be applied, [state] is the state of the lexing at
   the moment of the call, and [lexbuf] is the lexing buffer.

   If the edit is [Skip (start,stop,ed)], we check if the current
   location in the input is before [start]. If so, it means that we
   have not reached yet the point where we apply the edit, and,
   consequently, we copy the input to the output and update the state
   with the call [copy state lexbuf]. If not, we check whether the
   current location is after [stop], in which case we drop the current
   edit and apply the next (that is, [ed]), while copying the matched
   character. If the current location is between [start] and [stop],
   we must apply the edit, which consists in _not_ copying the input
   to the output.

   If the edit is [Write (loc,patch,ed)], we determine if the current
   location is before [loc]. If so, we have not attained yet the point
   where the edit should be applied, and we resume copying the input
   to the output. If not, we apply the edit, which means that we write
   the string [patch] to the output. We roll back the matched character,
   because the very next edit [ed] might apply at its location, and we
   resume [apply] with [ed].

   The last edit to consider is [Null], the empty edit. Keep in mind
   that that edit is interpreted as the copy of the input to the
   output, not the end of the application process.
*)

and apply edits state = parse
  _   { match edits with
          Skip (start,stop,ed) ->
            if loc_lt (get_loc state) start
            then apply edits (copy state lexbuf) lexbuf
            else if loc_lt stop (get_loc state)
                 then apply ed (copy state lexbuf) lexbuf
                 else apply edits (skip state lexbuf) lexbuf
        | Write (loc,patch,ed) ->
            if loc_lt (get_loc state) loc
            then apply edits (copy state lexbuf) lexbuf
            else (output_string state.out_chan patch;
                  rollback lexbuf;
                  apply ed state lexbuf)
        | Null -> apply edits (copy state lexbuf) lexbuf }
| eof {}

{
(* POSTLUDE *)

(* Standalone scanner (entry point from outside the module) *)

let make src lnum new_src dump =
  match open_in src, open_out new_src with
    cin, cout ->
      let buffer = Lexing.from_channel cin
      and init_state = { src_file  = src;
                         dump_file = dump;
                         out_chan  = cout;
                         scope     = Nil;
                         prefix    = [];
                         line      = 1;
                         col       = 0;
                         target    = lnum } in
      let edits = scan init_state buffer in
      let buffer = Lexing.from_channel (open_in src)
      in begin
           apply edits init_state buffer;
           flush_all ();
           close_in cin;
           close_out cout;
         end
  | exception Sys_error msg -> prerr_endline msg; exit 1
}
