type t = {
  c : Vect.t;
  r : float;
  t: Texture.t;
}

let make c r t = { c; r; t; }

let center s = s.c
let radius s = s.r
let texture s = s.t

let distance s d sphere = 
  let v_sc = Vect.diff sphere.c s in
  let s_sa = Vect.scalprod v_sc d in
  if s_sa <= 0. then infinity else
  let s_sc = Vect.norm v_sc in
  let s_ac2 = s_sc *. s_sc -. s_sa *. s_sa in
  let s_ac = sqrt s_ac2 in
  if s_ac >= sphere.r then infinity else
  let s_ab = sqrt (sphere.r *. sphere.r -. s_ac2) in
  let s_sb = s_sa -. s_ab in
  if (s_sc < sphere.r) then infinity else s_sb