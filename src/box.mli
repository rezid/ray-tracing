(* Boite dans l'espace tri-dimensionnel.*)

(* Une boite est une liste de face *)
type t

(* type representant une face et son opposée *)
type p 

(* vecteur unitaire normal d'une face *)
val center : p -> Vect.t

(* centre d'une face *)
val center : p -> Vect.t

(* centre de l'opposé de la face *)
val center_rev : p -> Vect.t

(* distance relative d'une face par rapport a l'origine *)
val distance : p -> float

(* distance relative de l'opposé d'une face par rapport a l'origine *)
val distance_rev : p -> float

(* demi distance entre deux faces opposées *)
val demi_dist : p -> float

(* texture d'une face *)
val texture : p -> Texture.t

(* texture d'une boite *)
val texture : t -> Texture.t

(* la distance entre un point et le centre de la boite 
   distance positive -> le point est a l'exterieur de la sphére
   distance negative -> le point est a l'interieur *)
val distance : Vect.t -> t -> float
