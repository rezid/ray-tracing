type t = {
	direction : Vect.t;
	distance : float;
	texture : Texture.t;
}

let direction p = p.direction
let distance p = p.distance
let texture p = p.texture

let distance p plan =
	0.0 (* TO DO *)