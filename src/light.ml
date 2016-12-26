type t = {
	direction : Vect.t;
	intensity : float;
}

let make intensity = { direction = Vect.make 0. (-1.) 0.; intensity }

let direction l = l.direction

let intensity l = l.intensity 

let apply_rotation light rotation = 
	let direction = Rotation.apply rotation light.direction in
	{direction; intensity = light.intensity;}