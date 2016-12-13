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