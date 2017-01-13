exception Failure of string

let my_min = function
  | [] -> raise(Failure "empty list")
  | x::xs -> List.fold_left min x xs
let rec func x lst c = match lst with
  | [] -> raise(Failure "Not Found")
  | hd::tl -> if (hd=x) then c else func x tl (c+1)
let find x lst = func x lst 0

type t = {
  ambiant : float;
  camera : Camera.t;
  lights : Light.t list;
  spheres : Sphere.t list;
  boxes : Box.t list;
  planes : Plane.t list;
}

let make ambiant camera lights spheres boxes planes = {ambiant; camera; lights; spheres; boxes; planes; }

let ambiant s = s.ambiant
let camera s = s.camera
let lights s = s.lights
let spheres s = s.spheres
let planes s = s.planes

let intersect orig cam scene = 
  let spheres = scene.spheres in
  let boxes = scene.boxes in
  let planes = scene.planes in
  let distances_s = List.map (Sphere.distance orig cam) spheres in

  let distances_normal_b = List.map (Box.distance orig cam) boxes in 
  let distances_b = List.map (fst) distances_normal_b in 

  let distances_p = List.map (Plane.distance orig cam) planes in
  let min = my_min (distances_s @ distances_b @ distances_p) in
  if min = infinity then None else
    let index = find min (distances_s @ distances_b @ distances_p) in
    let point = Vect.add orig (Vect.shift min cam) in
    if index < List.length spheres then
      let sphere = List.nth spheres index  in
      let normal = Vect.normalise (Vect.diff point (Sphere.center sphere)) in
      let texture = Sphere.texture sphere in
      let color = Texture.color texture in
      let kd = Texture.kd texture in
      let ks = Texture.ks texture in
      let phong = Texture.n texture in
      Some (Hit.make cam point normal color kd ks phong)
    else if index < (List.length boxes) + (List.length spheres) then
      let index = index - List.length spheres in
      let box =  List.nth boxes index in
      let normal = match snd (List.nth distances_normal_b index) with 
        | None -> failwith "not possible"
        | Some v -> v
      in
      let texture = Box.texture box in
      let color = Texture.color texture in
      let kd = Texture.kd texture in
      let ks = Texture.ks texture in
      let phong = Texture.n texture in
      Some (Hit.make cam point normal color kd ks phong)
    else 
      let index = index - List.length spheres - List.length boxes in
      let plane = List.nth planes index in
      let normal =  Plane.normal plane in
      let texture = Plane.texture plane in
      let color = Texture.color texture in
      let kd = Texture.kd texture in
      let ks = Texture.ks texture in
      let phong = Texture.n texture in
      Some (Hit.make cam point normal color kd ks phong)




let calcule_lighting scene hit = 
  let color = Hit.color hit in
  let kd = Hit.kd hit in
  let ks = Hit.ks hit in
  let n = Hit.phong hit in
  let normal = Hit.normal hit in
  
  let point = Hit.point hit in
  let rec calcule_temp lights = 
    match lights with 
    | [] -> 0.0
    | light::rest -> 
      let prod = Vect.scalprod (Vect.opp (Light.direction light)) normal in
      if prod <= 0. then  calcule_temp rest else
        let hit1 = intersect point (Vect.opp (Light.direction light)) scene in
        if hit1 = None then 
          let bissect = Hit.bisecting_direction hit (Light.direction light) in
          let prod2 = (Vect.scalprod normal bissect) ** (float_of_int n) in 
          (kd *. prod *. (Light.intensity light)) +. (ks *. prod2 *. (Light.intensity light)) +. (calcule_temp rest) 
        else calcule_temp rest in 
  let temp = calcule_temp scene.lights in 
  Color.shift temp color

let rec ray_trace dir origin max scene =
  (* intersection of the ray with the first objet *)
  let hit = intersect origin dir scene in
  (* if no itersection found then color is black*)
  match hit with 
  | None -> Color.black
  | Some hit -> 
    let color = Hit.color hit in
    let kd = Hit.kd hit in
    let ks = Hit.ks hit in
    let point = Hit.point hit in
    (* calcule le supplement de couleur du au lumiéres sauf si un objet cache cette lumiére *)
    let color_sup = calcule_lighting scene hit in
    (* calcule intensité de la lumiére ambiante *)
    let ia = ambiant scene in
    (* calcule la couleur avec l'equation du ray tracing *)
    let overflowed_color = Color.add (Color.shift ( kd *. ia) color) color_sup in
    (* reflexion calculus *)
    if max > 0 then 
      let max = max -1 in
      (* construire le vecteur de reflexion *)
      let v_ref = Hit.refl hit in
      let overflowed_color = Color.add overflowed_color (Color.shift ks (ray_trace v_ref point max scene)) in
      Color.may_overflow overflowed_color else
      (* color overflow *)
      Color.may_overflow overflowed_color

let create time  = 
  let time = (float_of_int time) /.10. in
  let pi = 4.0 *. atan 1.0 in
  let earthr = 8000. in
  let moonr = 1500. in

  let camera = Camera.make 30000. 0.8 in 

  let ambient = 0.0 in

  let light = Light.make 0.7 in
  let light1 = Light.apply_rotation light (Rotation.make (pi /. 8.) 0. (pi /. 4.)) in
  let light2 = Light.apply_rotation light (Rotation.make (pi /. 8.) 0. (-.pi /. 4.)) in

  let texture = Texture.make (Color.make_255 200 200 200) 0.4 0.6 10 in
  let plane = Plane.make (-4100.) texture in
  let plane1 = Plane.apply_rotation plane (Rotation.make 0. 0. 0.) in

  let texture = Texture.make (Color.make_255 150 150 150) 0.4 0.6 2 in
  let box = Box.make (Vect.make (0.) 0. (-15000.)) 50000. 10000. 15. texture in
  let box1 = Box.apply_rotation box (Rotation.make 0. (pi /. 4.) 0.) in
  let box2 = Box.apply_rotation box (Rotation.make 0. (-.pi /. 4.) 0.) in
  
  let texture = Texture.make (Color.make_255 243 54 26) 1. 0.3 2 in
  let sphere1 = Sphere.make (Vect.make (0.) 0. 0.) 4000. texture in

  let texture = Texture.make (Color.make_255 114 172 216) 0.1 0.6 3 in
  let earth = Sphere.make (Vect.make (0.) 0. 0.) 1200. texture in
  let texture = Texture.make (Color.make_255 246 255 0) 1. 0.3 3 in
  let moon = Sphere.make (Vect.make (0.) 0. 0.) 200. texture in


  let moon = Sphere.apply_translation moon (Vect.make moonr 0. 0.) in
  let moon = Sphere.apply_rotation moon (Rotation.make 0. (6. *. pi *. time /. 100.) 0.) in

  let earth = Sphere.apply_translation earth (Vect.make earthr 0. 0.) in
  let earth = Sphere.apply_rotation earth (Rotation.make 0. (2. *. pi *. time /. 100.) 0.) in
  let moon = Sphere.apply_translation moon (Vect.make earthr 0. 0.) in
  let moon = Sphere.apply_rotation moon (Rotation.make 0. (2. *. pi *. time /. 100.) 0.) in

  make ambient camera [light1;light2] [sphere1; earth; moon;] [box1; box2;] [plane1;] 
