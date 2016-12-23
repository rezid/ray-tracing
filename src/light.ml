type t = {
	direction : Vect.t;
	intensity : float;
}

let make direction intensity = { direction; intensity }

let direction l = l.direction

let intensity l = l.intensity 