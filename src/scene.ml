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
      Some (Hit.make cam point normal color kd ks)
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
      Some (Hit.make cam point normal color kd ks)
    else 
      let index = index - List.length spheres - List.length boxes in
      let plane = List.nth planes index in
      let normal =  Plane.normal plane in
      let texture = Plane.texture plane in
      let color = Texture.color texture in
      let kd = Texture.kd texture in
      let ks = Texture.ks texture in
      Some (Hit.make cam point normal color kd ks)




let calcule_lighting scene hit = 
  let color = Hit.color hit in
  let kd = Hit.kd hit in
  let normal = Hit.normal hit in
  let point = Hit.point hit in
  let rec calcule_temp lights = 
    match lights with 
    | [] -> 0.0
    | light::rest -> 
      let prod = Vect.scalprod (Vect.opp (Light.direction light)) normal in
      if prod <= 0. then  calcule_temp rest else
        let hit = intersect point (Vect.opp (Light.direction light)) scene in
        if hit = None then 
          prod *. Light.intensity light +. (calcule_temp rest) 
        else calcule_temp rest in 
  let temp = calcule_temp scene.lights in 
  Color.shift (temp *. kd) color

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

let create () = 
  let color0 = Color.make_255 239 54 26 in (* 239 54 26 red *)
  let color1 = Color.make_255 100 54 26 in 
  let color2 = Color.make_255 0 54 0 in 
  let color3 = Color.make_255 200 200 200 in 
  let texture0 = Texture.make color0 0.6 1. 2. in
  let texture1 = Texture.make color1 0.6 0. 2. in
  let texture2 = Texture.make color2 0.6 0. 2. in
  let texture3 = Texture.make color3 0. 1. 2. in
  let vecteur1 = Vect.make (-4000.) 2000. 0. in
  let vecteur2 = Vect.make 4000. 2000. (1000.) in
  let vecteur3 = Vect.make 6000. 2000. 0. in
  let sphere1 = Sphere.make vecteur1 2000. texture0 in
  let sphere2 = Sphere.make vecteur2 500. texture1 in
  let sphere3 = Sphere.make vecteur3 700. texture2 in

  let box1 = Box.make (Vect.make (0.) 6000. 3500.) 1000. 2000. 7000. texture0 in

  let normal1 = Vect.normalise (Vect.make (-1.) (0.) (1.)) in
  let plane1 = Plane.make normal1 (-5000.) texture3 in

   let normal2 = Vect.normalise (Vect.make (1.) (0.) (1.)) in
  let plane2 = Plane.make normal2 (-5000.) texture3 in

  let camera = Camera.make 20000. 0.8 in 
  let light1 = Light.make (Vect.normalise (Vect.make (-1.) (-0.) (-1.))) 1. in
  make 0.6 camera [light1] [] [box1;] [] 


