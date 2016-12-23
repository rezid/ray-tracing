(* La scene qui contient tout. *)

type t

(* Construction d'une scene. *)
val make : Sphere.t list -> Box.t list -> Plane.t list -> Camera.t -> t

(* list des spheres dans la scene *)
val spheres : t -> Sphere.t list

(* get the camera la scene *)
val camera : t -> Camera.t

(* calcule l'intersection d'un rayon avec le premier objet trouver
   retourne le point d'intersection et l'index de l'objet *)
val intersect : Vect.t -> Vect.t -> t -> Vect.t * int

(* crée la scene a partir d'un arbre AST *)
val create : unit -> t