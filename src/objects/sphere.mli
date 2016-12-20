(* Sphéres dans l'espace tri-dimensionnel.*)

type t

(* centre d'une sphére *)
val center : t -> Vect.t

(* rayon d'une sphére *)
val radius : t -> float

(* texture d'une sphére *)
val texture : t -> Texture.t

(* la distance entre un point et le centre de la sphere 
   distance positive -> le point est a l'exterieur de la sphére
   distance negative -> le point est a l'interieur *)
val distance : Vect.t -> t -> float
