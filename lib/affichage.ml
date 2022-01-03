(** Affichage d'objets à l'aide de l'algorithme du peintre.  *)


(* Tri par insertion *)
let tri_clef clef tableau =
    let valeurs = Array.map clef tableau in
    for i = 1 to (Array.length tableau) - 1 do
        let x = tableau.(i)
        and v = valeurs.(i) in
        let j = ref (i - 1) in
        while !j >= 0 && valeurs.(!j) > v do
        		tableau.(!j + 1) <- tableau.(!j);
        		valeurs.(!j + 1) <- valeurs.(!j);
            j := !j - 1;
        done;
        tableau.(!j + 1) <- x;
        valeurs.(!j + 1) <- v;
    done

let to_rgb color =
    (color lsr 16, (color lsr 8) land 0xff, color land 0xff)

(** Un repère échelonné et éclairé. *)
class espace ~x0 ~y0 ~zoom =

    let open Geometrie in
    let open Point in
    let open Vec in

    object (self)

        val mutable x0 : float = x0
        val mutable y0 : float = y0

        val mutable zoom : float = zoom

        val mutable direction_lumiere = {vx = 0.2; vy = -0.3; vz = 1.}

        val mutable base = (
            {vx = 1.; vy = 0.; vz = 0.},
            {vx = 0.; vy = 1.; vz = 0.},
            {vx = 0.; vy = 0.; vz = 1.}
        )

        method translation_axe_ox dx =
            x0 <- x0 +. dx

        method translation_axe_oy dy =
            y0 <- y0 +. dy

        method rotation_base_ox angle =
            base <- Geometrie.Base.rotation_ox base angle

        method rotation_base_oy angle =
            base <- Geometrie.Base.rotation_oy base angle
        
        method zoom_vue coef =
            zoom <- coef *. zoom
        
        (** Projette un point de l'espace sur l'écran. *)
        method projette2d point zoom =
            let x = x0 +. zoom *. point.x
            and y = y0 +. zoom *. point.y
            in (int_of_float x, int_of_float y)
        
        (** Projette un point de l'espace en coordonnées absolues dans la base orientée de l'espace. *)
        method projette_dans_base point =
            Geometrie.Base.projection base point

        (** La coordonnée en profondeur d'un point projeté dans la base de l'espace. *)
        method cote point = (self#projette_dans_base point).z

        method cote_moyenne point1 point2 point3 =
            ((self#cote point1) +. (self#cote point2) +. (self#cote point3)) /. 3.
        
        (** Trie des triangles projetés dans la base par leur cote moyenne. *)
        method ordonne_triangles triangles =
            let par_cote (p1, p2, p3, _couleur) = self#cote_moyenne p1 p2 p3
            in tri_clef par_cote triangles

        (** Affiche un triangle éclairé et projeté dans l'espace puis sur l'écran. *)
        method trace_triangle (point1, point2, point3, couleur) =
            let open Vec in
            let i = Vec.of_points point1 point2
            and j = Vec.of_points point1 point3 in
            let normale = Vec.unitaire (i @^ j) in
            let v = abs_float (normale @. direction_lumiere) in
            
            let echelle v x =
                int_of_float (v *. (float_of_int x))
            and projection point =
                self#projette2d (self#projette_dans_base point) zoom
            in
            let p1 = projection point1
            and p2 = projection point2
            and p3 = projection point3 in
            let (r, g, b) = to_rgb couleur in
            Graphics.set_color (Graphics.rgb (echelle v r) (echelle v g) (echelle v b));
            Graphics.fill_poly [| p1; p2; p3 |];
            Graphics.set_color (Graphics.rgb 0 0 0);
            Graphics.draw_poly [| p1; p2; p3 |]

        (* Affiche un objet sur l'écran. *)
        method algo_peintre triangles =
            self#ordonne_triangles triangles;
            let n = Array.length triangles in
            for i = 1 to n do
                self#trace_triangle triangles.(n - i);
            done
        
        method algo_peintre_sans_ordonner triangles =
            let n = Array.length triangles in
            for i = 1 to n do
                self#trace_triangle triangles.(n - i);
            done

        (** Il est important d'utiliser cette méthode pour le premier affichage d'un objet. *)
        method rafraichit objet =
            Graphics.auto_synchronize false;
            Graphics.clear_graph ();
            self#algo_peintre objet;
            Graphics.auto_synchronize true
        
        (** On peut utiliser cette méthode lorsque l'objet a déjà été ordonné. *)
        method redessine objet =
            Graphics.auto_synchronize false;
            Graphics.clear_graph ();
            self#algo_peintre_sans_ordonner objet;
            Graphics.auto_synchronize true
    end