
let read_scenario f =
  let buf = open_in f in
  let lexbuf = Lexing.from_channel buf in
  try
    let (sc:Scenario.scenario) = Parse.scenario Lex.next_token lexbuf in
    close_in buf;
    sc
  with e ->
    let open Lexing in
    let pos = lexbuf.lex_curr_p in
    Printf.eprintf "File %s, line %d, character %d: parse error\n"
                  f pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1);
    flush stderr;
    close_in buf;
    raise e

let main () =
  if Array.length Sys.argv = 0 then
    begin
      print_string "no scenario file given!\n";
      exit 1;
    end;
  let _ = read_scenario Sys.argv.(1) in
  print_string "Scenario file correctly read. Stop.\n";
  exit 0

let _ =
  if not !Sys.interactive then main ()

