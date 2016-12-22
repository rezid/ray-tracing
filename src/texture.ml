type t = {
	color : Color.t; 
	kd : float;
	ks : float;
	n : float;
}

let make color kd ks n = {color; kd; ks; n}
