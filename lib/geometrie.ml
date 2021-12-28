(** Objets primitifs standards pour le calcul géométrique en trois dimensions. *)


(** Un point en trois dimensions. *)
module Point =
    struct
        type t = { x: float; y: float; z:float; }

        let origine = { x = 0.; y = 0.; z = 0.; }

    end

(** Un point en trois dimensions. *)
type point = Point.t

(** Un élément d'un objet coloré. *)
type triangle = point * point * point * Graphics.color


(** La couleur par défaut d'un objet parsé depuis un fichier OBJ. *)
let couleur_par_defaut = Graphics.blue

let triangle_nul = (Point.origine, Point.origine, Point.origine, couleur_par_defaut)

(** Un objet au sens de l'algorithme du peintre, est composé de surfaces triangulaires colorées. *)
type objet = triangle array


(**
Calcul vectoriel de base.
Il faut ajouter `let open Vec in` pour utiliser les opérateurs spéciaux.
*)
module Vec =
    struct

        open Point        

        type t = { vx: float; vy: float; vz:float; }

        let nul = { vx = 0.; vy = 0.; vz = 0.; }

        let of_points point1 point2 =
            {
                vx = point2.x -. point1.x;
                vy = point2.y -. point1.y;
                vz = point2.z -. point1.z;    
            }
        
        let of_point point = of_points point Point.origine

        (** Translate un point par un vecteur. *)
        let ( @-> ) vec point =
            {
                x = point.x +. vec.vx;
                y = point.y +. vec.vy;
                z = point.z +. vec.vz;    
            }
        
        (** Produit d'un vecteur par un scalaire, malheureusement non commutatif :-(. *)
        let ( @*. ) vec l =
            {
                vx = l *. vec.vx;
                vy = l *. vec.vy;
                vz = l *. vec.vz;    
            }
        
        (** Addition vectorielle. *)
        let ( @+ ) vec1 vec2 =
            {
                vx = vec1.vx +. vec2.vx;
                vy = vec1.vy +. vec2.vy;
                vz = vec1.vz +. vec2.vz;    
            }
        
        (** Soustraction vectorielle. *)
        let ( @- ) vec1 vec2 =
            {
                vx = vec1.vx -. vec2.vx;
                vy = vec1.vy -. vec2.vy;
                vz = vec1.vz -. vec2.vz;    
            }

        (** Produit scalaire. *)
        let ( @. ) vec1 vec2 =
            vec1.vx *. vec2.vx +. vec1.vy *. vec2.vy +. vec1.vz *. vec2.vz
        
        
        let norme vec = sqrt (vec @. vec)

        (** Vecteur positivement colinéaire et de norme 1. *)
        let unitaire vec =
            let n = norme vec in
            {
                vx = vec.vx /. n;
                vy = vec.vy /. n;
                vz = vec.vz /. n;
            }
        
        let de_norme vec norme =
            let {vx = vx; vy = vy; vz = vz } = unitaire vec in
            {
                vx = norme *. vx;
                vy = norme *. vy;
                vz = norme *. vz;
            }

        (** Produit vectoriel. *)
        let ( @^ ) vec1 vec2 =
            {
                vx = vec1.vy *. vec2.vz -. vec1.vz *. vec2.vy;
                vy = vec1.vz *. vec2.vx -. vec1.vx *. vec2.vz;
                vz = vec1.vx *. vec2.vy -. vec1.vy *. vec2.vx;
            }
        
        let rotation_ox vec angle =
            let c = cos angle
            and s = sin angle in
            {
                vx = vec.vx;
                vy = c *. vec.vy -. s *. vec.vz;
                vz = s *. vec.vy +. c *. vec.vz;
            }

        let rotation_oy vec angle =
            let c = cos angle
            and s = sin angle in
            {
                vx = c *. vec.vx +. s *. vec.vz;
                vy = vec.vy;
                vz = (-1.) *. s *. vec.vx +. c *. vec.vz;
            }
    end


(** Un vecteur en trois dimensions. *)
type vecteur = Vec.t


module Base =
    struct

        (** Un base en trois dimensions. *)
        type base = vecteur * vecteur * vecteur

        let rotation_ox bs angle =
            let i, j, k = bs in
            (
                Vec.rotation_ox i angle,
                Vec.rotation_ox j angle,
                Vec.rotation_ox k angle
            )


        let rotation_oy bs angle =
            let i, j, k = bs in
            (
                Vec.rotation_oy i angle,
                Vec.rotation_oy j angle,
                Vec.rotation_oy k angle
            )
        
        (** Projette un point en coordonnées absolues en coordonnées d'une base. *)
        let projection bs point =
            let open Point in
            let open Vec in
            let vec = Vec.of_point point in
            let i, j, k = bs in
            {
                x = vec @. i;
                y = vec @. j;
                z = vec @. k;
            }
    end   



(**
On considère le repère orthonormé direct du labyrinthe :
        z                   Ceci est un exemple de labyrinthe vu de haut.
        ^                   Les murs du labyrinthe sont dans le deuxième quadrant.
        |                   L'axe (Oy) pointe vers le haut.
        |                   L'axe (Oz) positif pointe vers le fond.
x <____y(.)_ _ _            L'observateur est dans l'infini de (Oz) négatif.
        |O|     |           Le joueur O est dans la cellule (0, 0).
        |_ _| | |           Le point d'arrivée X est (3, 3).
        | |   | |
        |_ _|_|x|        
*)
module Objet =
    struct

        (** Translate tous les points de l'objet par un vecteur et préserve l'ordre et la couleur. *)
        let translate_objet vec objet =
            let open Vec in
            let tr point = vec @-> point in
            let nouveau = Array.copy objet in
            for i = 0 to Array.length objet - 1 do
                let (point1, point2, point3, couleur) = objet.(i) in
                nouveau.(i) <- (tr point1, tr point2, tr point3, couleur);
            done;
            nouveau

        (** Un parallélogramme formé de points (donnés en arguments dans le sens du tracé.) *)
        let parallelogramme point1 point2 point3 point4 ~precision ~couleur =
            if precision = 1 then
                [| (point1, point2, point3, couleur); (point1, point3, point4, couleur) |]
            else
                begin
                    let open Vec in
                    let vec_i = (Vec.of_points point1 point2) @*. 1. /. (float_of_int precision) in
                    let vec_j = (Vec.of_points point1 point4) @*. 1. /. (float_of_int precision) in
                    let unite = [|
                        (point1, vec_i @-> point1, vec_j @-> point1, couleur);
                        (vec_i @-> vec_j @-> point1, vec_i @-> point1, vec_j @-> point1, couleur)
                    |] in
                    let objet = ref [| |] in
                    for i = 0 to precision-1 do
                        for j = 0 to precision-1 do
                            let v = (vec_i @*. float_of_int i) @+ (vec_j @*. float_of_int j) in
                            objet := Array.append !objet (translate_objet v unite);
                        done;
                    done;
                    !objet
                end
        
        (**
        Crée une case sur le plan (Oxz) dont un sommet est à l'origine
        et son sommet opposé est dans le sens négatif de (Ox) et (Oz).
        *)
        let case cote ~precision ~couleur =
            let open Vec in
            let vecx = { vx = ~-.cote; vy = 0.; vz = 0. } in
            let vecz = { vx = 0.; vy = 0.; vz = ~-.cote } in
            let point1 = Point.origine in
            let point2 = vecx @-> point1 in
            let point3 = vecz @-> point2 in
            let point4 = vecz @-> point1 in
            parallelogramme point1 point2 point3 point4  ~precision ~couleur

        (**
        Crée une boule composée de triangles centrée sur l'origine.
        FIXME: les triangles sont plus concentrés sur un pôle
        et les carrés sont trop petits.
        *)
        let boule rayon ~precision ~couleur =
            let open Vec in
            let open Point in
            let pi = Float.pi in
            let n_triangles = 2 * precision * precision in
            let cote_triangle = 2. *. rayon *. sqrt (pi /. (float_of_int (n_triangles/2))) in
            let triangles = Array.make n_triangles triangle_nul in
            for i_theta = 1 to precision do
                for i_phi = 1 to precision do
                    let fp = float_of_int precision in
                    let theta = pi /. (float_of_int i_theta /. fp)
                    and phi = 2. *. pi /. (float_of_int i_phi /. fp) in
                    let rho = rayon *. sin theta in
                    let x = rho *. cos phi
                    and y = rho *. sin phi
                    and z = rayon *. cos theta in
                    let m = { x = x; y = y; z = z} in
                    let om = Vec.of_point m
                    (* v est un vecteur "pas trop colinéaire à om" *)
                    and v = if abs_float (theta -. pi/.2.) < pi/.4. then
                        { vx = 0.; vy = 0.; vz = 1.; }
                    else
                        { vx = 1.; vy = 1.; vz = 0.; }
                    in
                    (* i et j sont sur un plan normal à om. *)
                    let i = Vec.de_norme (om @^ v) cote_triangle in
                    let j = Vec.de_norme (om @^ i) cote_triangle in
                    let point = m in
                    let point2 = i @-> point
                    and point3 = j @-> point in
                    let point1 = i @-> point3 in
                    let k = precision*(i_theta-1) + i_phi-1 in
                    triangles.(k) <- (point, point2, point3, couleur);
                    triangles.(k + n_triangles/2) <- (point1, point2, point3, couleur);
                done;
            done;
            triangles
        

        (**
        Crée le mur vertical de cellule (0, -1) en haut à gauche.
        *)
        let mur_vertical longueur largeur hauteur ~couleur =
            let open Point in
            let open Vec in
            let vecx = { vx = ~-.largeur; vy = 0.; vz = 0. } in
            let vecy = { vx = 0.; vy = hauteur; vz = 0. } in
            let vecz = { vx = 0.; vy = 0.; vz = ~-.longueur } in
            let dl = largeur /. 2. in
            let point1 = { x = ~-.dl; y = 0.; z = 0. }
            and point2 = { x = dl; y = 0.; z = 0. }
            and point3 = { x = dl; y = hauteur; z = 0. }
            and point4 = { x = ~-.dl; y = hauteur; z = 0. } in
            let point5 = vecz @-> point1
            and point6 = vecz @-> point2
            and point7 = vecz @-> point3 in
            let face1 = parallelogramme point1 point2 point3 point4 ~couleur ~precision:1 in
            let face2 = translate_objet vecz face1 in
            let face3 = parallelogramme point1 point2 point6 point5 ~couleur ~precision:1 in
            let face4 = translate_objet vecy face3 in
            let face5 = parallelogramme point2 point3 point7 point6 ~couleur ~precision:1 in
            let face6 = translate_objet vecx face5 in
            Array.concat [face1; face2; face3; face4; face5; face6]
        
        (**
        Crée le mur horizontal de cellule (-1, 0) en haut à gauche.
        *)
        let mur_horizontal longueur largeur hauteur ~couleur =
            let open Point in
            let open Vec in
            let vecx = { vx = ~-.longueur; vy = 0.; vz = 0. } in
            let vecy = { vx = 0.; vy = hauteur; vz = 0. } in
            let vecz = { vx = 0.; vy = 0.; vz = ~-.largeur } in
            let dl = largeur /. 2. in
            let point1 = { x = 0.; y = 0.; z = ~-.dl -.longueur }
            and point2 = { x = 0.; y = 0.; z = dl -.longueur }
            and point3 = { x = 0.; y = hauteur; z = dl -.longueur }
            and point4 = { x = 0.; y = hauteur; z = ~-.dl -.longueur } in
            let point5 = vecx @-> point1
            and point6 = vecx @-> point2
            and point7 = vecx @-> point3 in
            let face1 = parallelogramme point1 point2 point3 point4 ~couleur ~precision:1 in
            let face2 = translate_objet vecx face1 in
            let face3 = parallelogramme point1 point2 point6 point5 ~couleur ~precision:1 in
            let face4 = translate_objet vecy face3 in
            let face5 = parallelogramme point2 point3 point7 point6 ~couleur ~precision:1 in
            let face6 = translate_objet vecz face5 in
            Array.concat [face1; face2; face3; face4; face5; face6]
        

            
    end