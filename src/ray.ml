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
  (* calcule intensité de la lumiére ambiante *)
  let ia = Scene.ambiant scene in
  (* calcule the origin of the ray *)
  let origin = Vect.make 0. 0. (Camera.viewdist (Scene.camera scene)) in
  (* open ppm picture file *)
  let output_file = Ppm.openfile !Command.hsize !Command.vsize "output.ppm" in
  print_if_verbose "file output.ppm opened!";
  (* trace rays for each pixel in the output file *)
  for y = !Command.vsize  downto 1 do
    for x = -(!Command.hsize) / 2  to !Command.hsize / 2 -1  do
      let color = ref Color.black in
      (* calcule direction of the ray*)
      let dir = Vect.normalise( Vect.diff (Vect.make ((float_of_int x) +. 0.5) ((float_of_int y) -. 0.5) z) origin) in
      (* intersection of the ray with the first objet *)
      let (inter,index) = Scene.intersect origin dir scene in
      (* if no itersection found then color is black*)
      if (index = -1) then Ppm.put_next_pixel output_file (Color.to_bytes !color)
      (* la sphere intersepté *)
      else let sphere = List.nth (Scene.spheres scene) index in
        (* la texture de la sphere *)
        let texture = Sphere.texture sphere in
        (* propiétés de la texture de la sphere *)
        let color = Texture.color texture in
        let kd = Texture.kd texture in
        (* calcule la normale du point d'intersection *)
        let n = Vect.normalise (Vect.diff inter (Sphere.center sphere)) in
        (* calcule le supplement de couleur du au lumiéres sauf si un objet cache cette lumiére *)
        let color_sup = Scene.calcule_lighting scene n kd color inter in
        (* calcule la couleur avec l'equation du ray tracing *)
        let final_color = Color.may_overflow (Color.add (Color.shift ( kd *. ia) color) color_sup) in
        (* affiche la couleur *)
        Ppm.put_next_pixel output_file (Color.to_bytes final_color)
    done;
  done;
  print_if_verbose "color of each pixel calculated!";
  (* exit program *)
  exit 0

let () = 
  if not !Sys.interactive then main ()

