type t = {
  cam : Vect.t;
  point : Vect.t;
  normal : Vect.t;
  refl : Vect.t;
  color : Color.t;
  kd : float;
  ks : float;
  phong: int;
}

let make cam point normal color kd ks phong = 
  let refl = Vect.add (Vect.shift (2. *. (Vect.scalprod (Vect.opp cam) normal )) normal) cam in
  { cam; point; normal; refl; color; kd; ks; phong;}

let cam hit = hit.cam
let point hit = hit.point
let normal hit = hit.normal
let refl hit = hit.refl
let color hit = hit.color
let kd hit = hit.kd
let ks hit = hit.ks
let phong hit = hit.phong

let bisecting_direction hit light_direction =
  Vect.normalise (Vect.opp (Vect.add hit.cam light_direction))