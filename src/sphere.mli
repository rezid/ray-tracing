(* Sphéres dans l'espace tri-dimensionnel.*)

type t

(* Construction d'une sphere. *)
val make : Vect.t -> float -> Texture.t -> t

(* centre d'une sphére *)
val center : t -> Vect.t

(* rayon d'une sphére *)
val radius : t -> float

(* texture d'une sphére *)
val texture : t -> Texture.t

(* distance parcourut par un rayon avant d'attendre la sphere
parametre:
    s : l'origine du rayon
    d : direction du rayon
sortie:
	infinity : le rayon n'atteind jamais la sphere 
	d > 0 : le point est a l'interieur de la sphere 
	infinity : le point est a l'exterieur de la sphere *)
val distance : Vect.t -> Vect.t -> t -> float