type p = {
  normal : Vect.t;
  center : Vect.t;
  distance : float;
  center_rev : Vect.t;
  distance_rev : float;
  demi_dist : float;
}

type t = {
  faces : p list;
  texture : Texture.t;
}

let make c lx ly lz texture = 
  let face1 = {
    normal = Vect.make 0. 0. 1.;
    center = Vect.add c (Vect.make 0. 0. (lz /. 2.));
    distance = Vect.vz c +. (lz /. 2.) ;
    center_rev = Vect.diff c (Vect.make 0. 0. (lz /. 2.));
    distance_rev = (lz /. 2.) -.  Vect.vz c;
    demi_dist = lz /. 2.;
  } in
  let face2 = {
    normal = Vect.make 1. 0. 0.;
    center = Vect.add c (Vect.make (lx /. 2.) 0. 0.);
    distance = Vect.vx c +. (lx /. 2.) ;
    center_rev = Vect.diff c (Vect.make (lx /. 2.) 0. 0.);
    distance_rev = (lx /. 2.) -. Vect.vx c;
    demi_dist = lx /. 2.;
  } in
  let face3 = {
    normal = Vect.make 0. 1. 0.;
    center = Vect.add c (Vect.make 0. (ly /. 2.) 0.);
    distance = Vect.vy c +. (ly /. 2.) ;
    center_rev = Vect.diff c (Vect.make 0. (ly /. 2.) 0.);
    distance_rev = (ly /. 2.) -. Vect.vy c;
    demi_dist = ly /. 2.;
  } in {
    faces = [face1;face2;face3];
    texture;
  }

let normal f = f.normal
let center f = f.center
let center_rev f = f.center_rev
let distance f = f.distance
let distance_rev f = f.distance_rev
let demi_dist f = f.demi_dist

let faces boite = boite.faces
let texture boite = boite.texture

let distance v_orig v_dir boite =
  let n1 = (List.nth boite.faces 0).normal in
  let d1 = (List.nth boite.faces 0).distance in
  let d1_rev = (List.nth boite.faces 0).distance_rev in
  let l1 = (List.nth boite.faces 0).demi_dist in
  let c1 = (List.nth boite.faces 0).center in
  let c1_rev = (List.nth boite.faces 0).center_rev in


  let n2 = (List.nth boite.faces 1).normal in
  let d2 = (List.nth boite.faces 1).distance in
  let d2_rev = (List.nth boite.faces 1).distance_rev in
  let l2 = (List.nth boite.faces 1).demi_dist in
  let c2 = (List.nth boite.faces 1).center in
  let c2_rev = (List.nth boite.faces 1).center_rev in

  let n3 = (List.nth boite.faces 2).normal in
  let d3 = (List.nth boite.faces 2).distance in
  let d3_rev = (List.nth boite.faces 2).distance_rev in
  let l3 = (List.nth boite.faces 2).demi_dist in
  let c3 = (List.nth boite.faces 2).center in
  let c3_rev = (List.nth boite.faces 2).center_rev in  

  let texture = boite.texture in

  let rec calcule_temp faces =
    match faces with 
    | [] -> (infinity, None)
    | face::rest -> 

      let v_n1, v_n2, v_n3, v_c1, v_c2, s_d1, s_d2, s_l2, s_l3 = 
        match List.length faces with
        | 1 -> n3, n1, n2, c3, c3_rev, d3, d3_rev, l1, l2
        | 2 -> n2, n3, n1, c2, c2_rev, d2, d2_rev, l3, l1
        | 3 -> n1, n2, n3, c1, c1_rev, d1, d1_rev, l2, l3
        | _ -> failwith "Face number of box incorrect (should be 6 = 3 * 2)."
      in

      let s_temp1 = Vect.scalprod v_dir v_n1 in

      let current = if s_temp1 > 0. then Some (Vect.opp v_n1,v_c2,s_d2) 
        else if s_temp1 < 0. then  Some (v_n1,v_c1,s_d1)
        else None in

      match current with
      | None -> calcule_temp rest
      | Some ( x, y, z ) -> 

        let v_n1 = x in 
        let v_c = y in
        let s_d1 = z in
        let plane = Plane.make v_n1 s_d1 texture in
        let dd = Plane.distance v_orig v_dir plane in
        let v_i = Vect.add v_orig (Vect.shift dd v_dir) in
        let v_ci = Vect.diff v_i v_c in

        let min1 = abs_float ( Vect.scalprod v_ci v_n2) in
        let min2 = abs_float ( Vect.scalprod v_ci v_n3) in

        if (min1 <= s_l2 && min2 <= s_l3 ) then (dd, Some v_n1)
        else calcule_temp rest
  in
  calcule_temp boite.faces


let apply_rotation box rotation =
  match box.faces with
  |[] -> failwith "Face number of box incorrect (should be 6 = 3 * 2)."
  | _::[] -> failwith "Face number of box incorrect (should be 6 = 3 * 2)."
  | _::_::[] -> failwith "Face number of box incorrect (should be 6 = 3 * 2)."
  | f1::f2::f3::[] -> let f1 = {
          normal = Rotation.apply rotation f1.normal;
          center = Rotation.apply rotation f1.center;
          distance = f1.distance;
          center_rev = Rotation.apply rotation f1.center_rev;
          distance_rev = f1.distance_rev;
          demi_dist = f1.demi_dist;
        } in
        let f2 = {
          normal = Rotation.apply rotation f2.normal;
          center = Rotation.apply rotation f2.center;
          distance = f2.distance;
          center_rev = Rotation.apply rotation f2.center_rev;
          distance_rev = f2.distance_rev;
          demi_dist = f2.demi_dist;
        } in
        let f3 = {
          normal = Rotation.apply rotation f3.normal;
          center = Rotation.apply rotation f3.center;
          distance = f3.distance;
          center_rev = Rotation.apply rotation f3.center_rev;
          distance_rev = f3.distance_rev;
          demi_dist = f3.demi_dist;
        } in
        {faces = [f1;f2;f3]; texture = box.texture;}
  | _ -> failwith "Face number of box incorrect (should be 6 = 3 * 2)."