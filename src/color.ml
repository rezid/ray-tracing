type t = {
	red : float;
	green : float;
	blue : float;
}

let make r g b = {red = r; green = g; blue = b}
let black = {red = 0.; green = 0.; blue = 0.}
let to_bytes color = (int_of_float (color.red *. 255.), int_of_float(color.green *. 255.), int_of_float(color.blue *. 255.))
let to_graphics color = let x, y, z = to_bytes color in Graphics.rgb x y z
let add c1 c2 = {red = c1.red +. c2.red; green = c1.green +. c2.green; blue = c1.blue +. c2.blue;}
let mult c1 c2 = {red = c1.red *. c2.red; green = c1.green *. c2.green; blue = c1.blue *. c2.blue;}
let shift s color = {red = s *. color.red; green = s *. color.green; blue = s *. color.blue;}
let print { red; green; blue} = 
	let s =
		Printf.sprintf "\n {\n  x = %f\n  y = %f\n  z = %f\n}\n" red green blue
	in 
	print_string s
