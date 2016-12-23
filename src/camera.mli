(* Camera *)
(**********)

type t

(* The value of viewdist of Camera *)
val viewdist : t -> float

(* construire une camera *)
val make : float -> float -> t

(* The angle of Camera *)
val angle : t -> float

(* calcule the z coordonee of the screen
param:
	- c : camera
	- l : horizontal size of the screen *)
val calcule_z : t -> int -> float