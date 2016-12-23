type t = {
	v : float;
	a : float;
}

let make v a = { v; a; }

let viewdist c = c.v

let angle c = c.a

let calcule_z c l = c.v -. (float_of_int(l) /. (2. *. tan(c.a *. 0.5)))