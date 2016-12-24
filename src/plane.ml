type t = {
  normal : Vect.t;
  distance : float;
  texture : Texture.t;
}

let make normal distance texture = { normal; distance; texture; }

let normal p = p.normal
let distance p = p.distance
let texture p = p.texture

let distance v_os v_d plan = 
  let v_n = plan.normal in
  let s_d = plan.distance in
  let s_temp1 = Vect.scalprod v_n v_os in
  if s_temp1 <= s_d then infinity else
    let s_temp2 = Vect.scalprod v_n v_d in 
    if s_temp2 >= 0. then infinity else 
      (s_d -. s_temp1) /. s_temp2