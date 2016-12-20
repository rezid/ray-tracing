
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
  (* parse the command line argument and store it in global variable references *)
  Command.parse_command_line ();
  
  (* calcule the AST tree of the scenario file *)
  let ast = read_scenario !Command.input_file in
  print_endline ("scenario file accepted!");
  exit 0



let () =
  if not !Sys.interactive then main ()

