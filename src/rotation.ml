(* multiplication matricielle *)
let matrix_multiply m1 m2 =
  let rec mapn f lists =
    assert (lists <> []);
    if List.mem [] lists then
      []
    else
      f (List.map List.hd lists) :: mapn f (List.map List.tl lists) 
  in
  List.map
    (fun row ->
       mapn
         (fun column ->
            List.fold_left (+.) 0.
              (List.map2 ( *. ) row column))
         m2)
    m1


type t = {
  m : float list list;
}

let make rx ry rz = let
  mx = [ [ 1. ;    0.   ;      0.       ;];
         [ 0. ;   cos rx;  (-.(sin rx)) ;];
         [ 0. ;   sin rx;    cos rx     ;]; ]; in
  let
    my = [ [     cos ry   ;   0.  ; sin ry ;];
           [       0.     ;   1.  ;    0.  ;];
           [ (-.(sin ry)) ;   0.  ; cos ry ;] ]; in
  let
    mz = [ [ cos rz  ; (-.(sin rz)) ;   0.  ;];
           [ sin rz  ;    cos rz    ;   0.  ;];
           [    0.   ;      0.      ;   1.  ;] ]; in
  {m = matrix_multiply (matrix_multiply mx my ) mz }

let id = make 0. 0. 0.

let compose r1 r2 = { m = matrix_multiply r1.m r2.m; }

let apply rotation v = 
  let [[x]; [y]; [z];] =
    let v = [[Vect.vx v]; [Vect.vy v]; [Vect.vz v];] in
    matrix_multiply rotation.m v in
  Vect.make x y z

let print x = 
  let pp_my_image s =
    let rec rowToString r =
      match r with
      | [] -> ""
      | h :: [] -> string_of_float h
      | h :: t -> string_of_float h ^ ";" ^ (rowToString t) in

    let rec imageToString i =
      match i with
      | [] -> ""
      | h :: t -> "[" ^ (rowToString h) ^ "];\n" ^ (imageToString t) in
    print_string (imageToString s) in
  pp_my_image x.m
