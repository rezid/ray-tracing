type t

val make : Vect.t -> Vect.t -> Vect.t -> Color.t -> float -> float -> t

val cam : t -> Vect.t
val point : t -> Vect.t
val normal : t -> Vect.t
val refl : t -> Vect.t
val color : t -> Color.t
val kd : t -> float
val ks : t -> float

val bisecting_direction : t-> Vect.t -> Vect.t