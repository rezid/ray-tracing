(* La scene qui contient tout. *)

type t

(* Construction d'une scene. *)
val make : float -> Camera.t -> Light.t list -> Sphere.t list -> Box.t list -> Plane.t list -> t

(* get the intensitié of th ambiant lumiére *)
val ambiant : t -> float

(* get the camera la scene *)
val camera : t -> Camera.t

(* get the lights of the scene *)
val lights : t -> Light.t list

(* list des spheres dans la scene *)
val spheres : t -> Sphere.t list

(* calcule l'intersection d'un rayon avec le premier objet trouver
   retourne le point d'intersection et l'index de l'objet *)
val intersect : Vect.t -> Vect.t -> t -> Vect.t * int

(* crée la scene a partir d'un arbre AST *)
val create : unit -> t

(* calcule la contribution des lumiéres sur la couleur du pixel *)
val calcule_lighting : t -> Vect.t -> float -> Color.t -> Vect.t -> Color.t

(* calcule la couleur *)
val ray_trace : Vect.t -> Vect.t -> int -> t -> Color.t