let usage () =
  prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <src>.cc");
  Pervasives.exit 1

let missing file =
  prerr_endline ("File " ^ file ^ " does not exist.")

let _ = match Array.length Sys.argv with
  2 -> let src = Sys.argv.(1)
       in if Sys.file_exists src
          then if Filename.check_suffix src ".cc"
               then let prefix = Filename.chop_suffix src ".cc" in
                    let pos = prefix ^ ".pos" in
                    let cand = Lexer.find_candidates ~src ~out:pos in
                    let rhs = prefix ^ ".rhs.cc"
                    in Pos.find_rvalues ~cand ~src ~out:rhs
          else (missing src; usage ())
| _ -> usage ()
