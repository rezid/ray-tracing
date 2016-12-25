type t = {
	color : Color.t; 
	kd : float;
	ks : float;
	n : int;
}

let make color kd ks n = {color; kd; ks; n}

let color c = c.color
let kd c = c.kd
let ks c = c.ks
let n c = c.n