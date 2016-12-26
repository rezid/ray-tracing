(* Plan dans l'espace tri-dimensionnel.*)

type t

(* construction d'un plan *)
val make : Vect.t -> float -> Texture.t -> t

(* vecteur unitaire normale d'un plan *)
val normal : t -> Vect.t

(* distance relative entre l'origine et le plan*)
val dist : t -> float

(* texture d'une sphére *)
val texture : t -> Texture.t

(* distance parcourut par un rayon avant d'attendre le plan
parametre:
    s : l'origine du rayon
    d : direction du rayon
sortie:
	infinity : le rayon n'atteind jamais la sphere 
	d > 0 : le point est a l'interieur de la sphere 
	infinity : le point est a l'exterieur de la sphere *)
val distance : Vect.t -> Vect.t -> t -> float

val apply_rotation : t -> Rotation.t -> t