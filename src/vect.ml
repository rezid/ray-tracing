type t = {
	x : float;
	y : float;
	z : float;
}

let vx = fun v -> v.x
let vy = fun v -> v.y
let vz = fun v -> v.z
let make x y z = { x; y; z }
let xunit = { x = 1.; y = 0.; z = 0. }
let yunit = { x = 0.; y = 1.; z = 0. }
let zunit = { x = 0.; y = 0.; z = 1. }
let scalprod v1 v2 = v1.x *. v2.x +. v1.y *. v2.y +. v1.z *. v2.z
let norm v = sqrt (v.x *. v.x +. v.y *. v.y +. v.z *. v.z)
let add v1 v2 = { x = v1.x +. v2.x;  y = v1.y +. v2.y; z = v1.z +. v2.z }
let diff v1 v2 = { x = v1.x -. v2.x;  y = v1.y -. v2.y; z = v1.z -. v2.z }
let opp v = { x = -.v.x; y = -.v.y; z = -.v.z }
let shift v x = { x = x *. v.x; y = x *. v.y; z = x *. v.z }
let dist v1 v2 = norm (add v1 v2)
let normalise v = let alpha = norm v in { x = v.x /. alpha; y = v.y /. alpha; z = v.z /. alpha }
let dist2 v1 v2 = let d = dist v1 v2 in d *. d
let normalised_diff v1 v2 = norm (diff v1 v2)

let print { x; y; z} = 
	let s =
		Printf.sprintf "\n {\n  x = %f\n  y = %f\n  z = %f\n}\n" x y z
	in 
	print_string s
