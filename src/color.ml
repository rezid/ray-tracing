type t = {
  red : float;
  green : float;
  blue : float;
}

let mo x = if x > 1. then 1. else x

let make r g b = {red = r; green = g; blue = b}
let make_255 r g b = {red = float_of_int (r) /. 255.; green = float_of_int(g) /. 255.; blue = float_of_int(b) /. 255.}
let black = {red = 0.; green = 0.; blue = 0.}
let to_bytes color = (int_of_float (color.red *. 255.), int_of_float(color.green *. 255.), int_of_float(color.blue *. 255.))
let to_graphics color = let x, y, z = to_bytes color in Graphics.rgb x y z
let add c1 c2 = {red = c1.red +. c2.red; green = c1.green +. c2.green; blue = c1.blue +. c2.blue;}
let mult c1 c2 = {red = c1.red *. c2.red; green = c1.green *. c2.green; blue = c1.blue *. c2.blue;}
let shift s color = {red = s *. color.red; green = s *. color.green; blue = s *. color.blue;}
let may_overflow c = {red = mo c.red; green = mo c.green; blue = mo c.blue}
let print {red; green; blue} = 
  let s =
    Printf.sprintf "\n {\n  red = %f\n  green = %f\n  blue = %f\n}\n" red green blue
  in 
  print_string s


