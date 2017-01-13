(* Scenario input file *)
let input_file = ref ""

(* mode verbose *)
let verbose = ref false

(* depth pour préciser le nombre max de rebonds lors du lancer de rayon*)
let depth = ref 10

(* hsize et vsize pour préciser largeur et hauteur de l'image *)
let hsize = ref 800
let vsize = ref 600

(*  pour préciser que l'on souhaite créer un film à n images au lieu d'une simple image *)
let anim = ref 0

(* pour choisir le nombre de processus crée pour realiser une video *)
let max_proc_video = ref 1

(* pour choisir le nombre de processus crée pour realiser une image *)
let max_proc_image = ref 1

(* lire les options en ligne de commande *)
let parse_command_line () = 
  begin
    let speclist = [
      ("-source",  Arg.Set_string input_file, "Scenario source file");
      ("-v",       Arg.Set verbose,           "Enables verbose mode");
      ("-depth",   Arg.Set_int depth,         "Sets maximum number of rebonds");
      ("-hsize",   Arg.Set_int hsize,         "Sets width picture");
      ("-vsize",   Arg.Set_int vsize,         "Sets height picture");
      ("-maxProcessForVideo",   Arg.Set_int max_proc_video,       "The maximum number of process used for creating video");
      ("-maxProcessForPicture", Arg.Set_int max_proc_image,       "The maximum number of process used for creating picture");
      ("-size",    Arg.Tuple ([Arg.Set_int hsize;Arg.Set_int vsize]),"Sets width and height picture");
      ("-anim",    Arg.Set_int anim,          "Create a movie with n pictures");
    ]
    in let usage_msg = "\nOptions available:"
    in Arg.parse speclist (fun _ -> ()) usage_msg;

    match (!verbose, !input_file) with 
    | true,"" -> print_endline ("Verbose mode: " ^ string_of_bool !verbose ^
                                "\nMax number of rebonds: " ^ string_of_int !depth ^
                                "\nMax number of process video: " ^ string_of_int !max_proc_video ^
                                "\nMax number of process picture: " ^ string_of_int !max_proc_image ^
                                "\nPicture size: " ^ string_of_int !hsize ^ " * " ^ string_of_int !vsize ^
                                "\nPicture number: " ^ string_of_int !anim);
                 print_endline ("\nyou must specify scenario source file with: -source <source>");
                 exit 1
    | true,_ -> print_endline ("Verbose mode: " ^ string_of_bool !verbose ^
                               "\nMax number of rebonds: " ^ string_of_int !depth ^
                               "\nMax number of process video: " ^ string_of_int !max_proc_video ^
                               "\nMax number of process picture: " ^ string_of_int !max_proc_image ^                            
                               "\nPicture size: " ^ string_of_int !hsize ^ " * " ^ string_of_int !vsize ^
                               "\nPicture number: " ^ string_of_int !anim);
				()
    | _,"" -> print_endline ("you must specify scenario source file with: -source <source>");
      exit 1
    | _, _ -> ()
  end