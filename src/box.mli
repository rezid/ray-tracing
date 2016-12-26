(* Boite dans l'espace tri-dimensionnel.*)

(* Une boite est une liste de face *)
type t

(* type representant une face et son opposée *)
type p 

(* construire une boite
    param:
    - c : centre
    - lx, ly et lz : largeur, hauteur et profendeur de la boite
    - texture de la boite *)
val make : Vect.t -> float -> float -> float -> Texture.t -> t

(* vecteur unitaire normal d'une face *)
val normal : p -> Vect.t

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

(* texture d'une boite *)
val texture : t -> Texture.t

(* faces d'une boite *)
val faces : t -> p list

(* distance parcourut par un rayon avant d'attendre le boite
parametre:
    s : l'origine du rayon
    d : direction du rayon
sortie:
	infinity : le rayon n'atteind jamais la boite 
	d > 0 : le point est a l'interieur de la boite 
	infinity : le point est a l'exterieur de la boite *)
val distance : Vect.t -> Vect.t -> t -> float * (Vect.t option)


val apply_rotation : t -> Rotation.t -> t