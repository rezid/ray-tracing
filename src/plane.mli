(* Plan dans l'espace tri-dimensionnel.*)

type t

(* vecteur unitaire normale d'un plan *)
val direction : t -> Vect.t

(* distance relative entre l'origine et le plan*)
val distance : t -> float

(* texture d'une sphére *)
val texture : t -> Texture.t

(* la distance relative entre un point et le centre de la sphere 
   distance positive -> le point est a l'exterieur de la sphére
   distance negative -> le point est a l'interieur *)
val distance : Vect.t -> t -> float
