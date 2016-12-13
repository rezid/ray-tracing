type p = {
	direction : Vect.t;
	center : Vect.t;
	distance : float;

	center_rev : Vect.t;
	distance_rev : float;

	demi_dist : float;

	texture : Texture.t;
}

type t = p list

let direction f = f.direction

let center f = f.center
let center_rev f = f.center_rev

let distance f = f.distance
let distance_rev f = f.distance_rev

let demi_dist f = f.demi_dist

let texture f = f.texture

let texture b =
	match b with (* TO DO *)
	| h::rest -> h.texture

let distance p b = 5. (* TO DO *)