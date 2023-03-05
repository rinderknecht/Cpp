(* See garland.mll for the documentation. *)

let usage () =
  prerr_endline ("Usage: " ^ Sys.argv.(0) ^ " <src> <line> <new_src> <dump>");
  Pervasives.exit 1

let missing file =
  prerr_endline ("File " ^ file ^ " does not exist.");
  usage ()

let invalid_num n =
  prerr_endline ("Line number " ^ n ^ " is invalid.");
  usage ()

let args = Array.length Sys.argv;;

if args = 5
then let src = Sys.argv.(1)
     in if Sys.file_exists src
     then let line =
            try int_of_string Sys.argv.(2) with
              Failure _ -> invalid_num Sys.argv.(2)
          in Garland.make src line Sys.argv.(3) Sys.argv.(4)
     else missing src
else usage ()
