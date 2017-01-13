let print_if_verbose s = if (!Command.verbose) then print_endline (s)

let buildList i n =
  let rec aux acc i =
    if i <= n then
      aux (i::acc) (i+1)
    else (List.rev acc)
  in
  aux [] i

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

  let create_image t = 
    (* create the scene from the AST tree *)
    let scene = Scene.create t in
    (* calcule the z coordonee of the screen *)
    let z = Camera.calcule_z (Scene.camera scene) !Command.hsize in
    (* calcule the origin of the ray *)
    let origin = Vect.make 0. 0. (Camera.viewdist (Scene.camera scene)) in
    (* open ppm picture file *)
    let file_name = "truc" ^ (Printf.sprintf "%05d" t) in
    let output_file = Ppm.openfile !Command.hsize !Command.vsize ("../pics/" ^ file_name) in
    print_if_verbose "file output.ppm opened!";
    (* trace rays for each pixel in the output file *)
    for y = !Command.vsize / 2   downto 1 - !Command.vsize / 2  do
      for x = -(!Command.hsize) / 2  to !Command.hsize / 2 -1  do
        (* calcule direction of the ray*)
        let dir = Vect.normalise( Vect.diff (Vect.make ((float_of_int x) +. 0.5) ((float_of_int y) -. 0.5) z) origin) in
        (* ray tracing *)
        let color = Scene.ray_trace dir origin !Command.depth scene in
        Ppm.put_next_pixel output_file (Color.to_bytes color)
      done;
    done;
    Ppm.closefile output_file;
    print_if_verbose "color of each pixel calculated!";
    (* exit program *)
  in

  let list_t = buildList 0 !Command.anim in 
  Parmap.parmap ~ncores:!Command.max_proc_video create_image (Parmap.L(list_t));
  if !Command.anim > 0 then 
    exit (Sys.command "avconv  -y -v quiet  -r 10 -i ../pics/truc%5d.ppm -b:v 1000k ../pics/truc.mp4")
  else 
    exit 0

let () = 
  if not !Sys.interactive then main ()

