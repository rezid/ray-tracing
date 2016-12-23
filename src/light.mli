(* Source de lumiére *)

type t

(* Construire une source de lumiére *)
val make : Vect.t -> float -> t

(* direction de la lumiére *)
val direction : t -> Vect.t

(* intencité de la lumiére *)
val intensity : t -> float