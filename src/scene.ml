exception Failure of string

let my_min = function
  | [] -> raise(Failure "empty list")
  | x::xs -> List.fold_left min x xs
let rec func x lst c = match lst with
  | [] -> raise(Failure "Not Found")
  | hd::tl -> if (hd=x) then c else func x tl (c+1)
let find x lst = func x lst 0

type t = {
  spheres : Sphere.t list;
  boxes : Box.t list;
  planes : Plane.t list;
  camera : Camera.t;
}

let spheres s = s.spheres

let camera s = s.camera

let make spheres boxes planes camera= {spheres; boxes; planes; camera}

let intersect s d scene = 
  let dists = List.map (Sphere.distance s d) scene.spheres in 
  let min = my_min dists in
  (* let () = List.iter (Printf.printf "%f ") dists in *)
  if min = infinity then Vect.make infinity infinity infinity, -1 else
  let index = find min dists in
  Vect.add s (Vect.shift min d) , index
      
let create () = 
  let color0 = Color.make_255 239 54 26 in (* red *)
  let color1 = Color.make_255 100 54 26 in (* red *)
  let color2 = Color.make_255 0 54 0 in (* red *)
  let texture0 = Texture.make color0 0.6 1. 2. in
  let texture1 = Texture.make color1 0.6 1. 2. in
  let texture2 = Texture.make color2 0.6 1. 2. in
  let vecteur1 = Vect.make 0. 4000. 0. in
  let vecteur2 = Vect.make 4000. 4000. 0. in
  let vecteur3 = Vect.make 6000. 4000. 0. in
  let sphere1 = Sphere.make vecteur1 2000. texture0 in
  let sphere2 = Sphere.make vecteur2 500. texture1 in
  let sphere3 = Sphere.make vecteur3 700. texture2 in
  let camera = Camera.make 20000. 0.8 in 
  make [sphere1;sphere2;sphere3] [] [] camera


