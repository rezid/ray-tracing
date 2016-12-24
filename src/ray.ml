let print_if_verbose s = if (!Command.verbose) then print_endline (s)

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
  let _ = read_scenario !Command.input_file in
  print_if_verbose "scenario file accepted!";
  (* create the scene from the AST tree *)
  let scene = Scene.create () in
  (* calcule the z coordonee of the screen *)
  let z = Camera.calcule_z (Scene.camera scene) !Command.hsize in
  (* calcule the origin of the ray *)
  let origin = Vect.make 0. 0. (Camera.viewdist (Scene.camera scene)) in
  (* open ppm picture file *)
  let output_file = Ppm.openfile !Command.hsize !Command.vsize "output.ppm" in
  print_if_verbose "file output.ppm opened!";
  (* trace rays for each pixel in the output file *)
  for y = !Command.vsize  downto 1 do
    for x = -(!Command.hsize) / 2  to !Command.hsize / 2 -1  do
      (* calcule direction of the ray*)
      let dir = Vect.normalise( Vect.diff (Vect.make ((float_of_int x) +. 0.5) ((float_of_int y) -. 0.5) z) origin) in
      (* ray tracing *)
      let color = Scene.ray_trace dir origin !Command.depth scene in
      Ppm.put_next_pixel output_file (Color.to_bytes color)
    done;
  done;
  print_if_verbose "color of each pixel calculated!";
  (* exit program *)
  exit 0

let () = 
  if not !Sys.interactive then main ()

