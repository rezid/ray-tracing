type t

val make : Vect.t -> Vect.t -> Vect.t -> Color.t -> float -> float -> int -> t

val cam : t -> Vect.t
val point : t -> Vect.t
val normal : t -> Vect.t
val refl : t -> Vect.t
val color : t -> Color.t
val kd : t -> float
val ks : t -> float
val phong : t -> int

val bisecting_direction : t-> Vect.t -> Vect.t