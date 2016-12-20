type t = {
  center : Vect.t;
  radius : float;
  texture: Texture.t;
}

let center s = s.center
let radius s = s.radius
let texture s = s.texture

let distance p s = 
  let d = Vect.dist p s.center in
  if d < s.radius then -.d  else d

let ray_sphere origine direction sphere =
  let v = Vect.diff sphere.center origine in
  let b = Vect.scalprod v direction in
  let d2 = (b *. b -. Vect.scalprod v v +. sphere.radius *. sphere.radius) in
  if d2 < 0. 
  then infinity
  else
    let d = sqrt d2 in
    let t1 = b -. d and t2 = b +. d in
    if t2 > 0. 
    then if t1 > 0. 
      then t1
      else 
        t2
    else
      infinity

let rec intersect origin direction (l, _ as hit) (sphere, scene) =
	match ray_sphere origin direction sphere, scene with
	| l', _ when l' >= l -> hit
	| l', [] -> l', Vect.normalise (Vect.diff (Vect.add origin (Vect.shift l' direction)) sphere.center)
	| _, scenes -> intersects origin direction hit scenes
	and intersects origin direction hit = function
	| [] -> hit
	| scene::scenes -> intersects origin direction (intersect origin direction hit scene) scenes

(* Constant ---------------------------------------------------*)
let zero = Vect.make 0. 0. 0.
let light = Vect.normalise (Vect.make 1. 3. (-2.)) and ss = 4
let delta = sqrt epsilon_float
(*-------------------------------------------------------------*)

let rec ray_trace direction scene =
	let l, n = intersect zero direction (infinity, zero) scene in
	let g = Vect.scalprod n light in
	if g <= 0. then 0. else
		let p = Vect.add (Vect.shift l direction) (Vect.shift delta n) in
		if fst (intersect p light (infinity, zero) scene) < infinity then 0. else g