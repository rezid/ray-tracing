(* Texture d'un objet *)

type t

(* Construction d'une texture, les arguments sont dans cette ordre:
   - une couleur de surface 
   - Kd: un coefficient de refexion diffuse entre 0 et 1
   - Ks: un coefficient de refexion spéculaire entre 0 et 1
   - n: un coefficient de Phong (un reel strictement positive). *)
val make : Color.t -> float -> float -> int -> t

(* get color *)
val color : t -> Color.t

(* get kd *)
val kd : t -> float

(* get ks *)
val ks : t -> float

(* get n *)
val n : t -> int