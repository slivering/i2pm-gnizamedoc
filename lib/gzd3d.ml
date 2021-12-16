

(*
#use "topfind";;
#require "graphics";;
#require "str";;
*)



(* __________TYPES ET CONSTANTES GLOBALES__________ *)

module Geometrie =
    struct

        module Point =
            struct
                type point = { x: float; y: float; z:float; }

                let origine = { x = 0.; y = 0.; z = 0.; }

            end
        
        type triangle = Point.point * Point.point * Point.point

        type objet = triangle array

        open Point


        module Vec =
            struct
                type vecteur = { vx: float; vy: float; vz:float; }

                let nul = { vx = 0.; vy = 0.; vz = 0.; }

                let of_points point1 point2 = 
                    {
                        vx = point2.x -. point1.x;
                        vy = point2.y -. point1.y;
                        vz = point2.z -. point1.z;    
                    }
                
                let of_point point = of_points point Point.origine

                let produit_scalaire vec1 vec2 =
                    vec1.vx *. vec2.vx +. vec1.vy *. vec2.vy +. vec1.vz *. vec2.vz
                
                let norme vec = sqrt (produit_scalaire vec vec)

                let unitaire vec =
                    let n = norme vec in
                    {
                        vx = vec.vx /. n;
                        vy = vec.vy /. n;
                        vz = vec.vz /. n;
                    }       

                let produit_vectoriel vec1 vec2 =
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


        module Base =
            struct
                type base = Vec.vecteur * Vec.vecteur * Vec.vecteur

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
                
                let projection bs point =
                    let vec = Vec.of_point point in
                    let i, j, k = bs in
                    {
                        x = Vec.produit_scalaire vec i;
                        y = Vec.produit_scalaire vec j;
                        z = Vec.produit_scalaire vec k;
                    }
            end   

    end



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

module Td =
    struct

    type 'a tableau_dynamique = {
        mutable taille: int;
        mutable support: 'a array;
    }

    let cree valeur =
        {taille = 1; support = Array.make 1 valeur}

    let ajoute td valeur =
        let n = !td.taille in
        if Array.length !td.support = n then
            begin
                !td.support <- Array.append !td.support (Array.make n !td.support.(0));
                !td.support.(n) <- valeur;
            end
        else
            !td.support.(n) <- valeur;
        !td.taille <- 1 + n

    let element td indice = !td.support.(indice)
    end


open Geometrie
open Point
open Vec

class espace =
    object (self)

        val mutable x0 = 400.
        val mutable y0 = 300.

        val mutable zoom = 100.

        val mutable couleur_lumiere = (0, 0, 255)
        val mutable direction_lumiere = {vx = 0.; vy = 0.; vz = 1.}

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
        
        method projette2d point zoom =
            let x = x0 +. zoom *. point.x
            and y = y0 +. zoom *. point.y
            in (int_of_float x, int_of_float y)
        
        method projette_dans_base point =
            Geometrie.Base.projection base point

        method cote point = (self#projette_dans_base point).z

        method cote_moyenne point1 point2 point3 =
            ((self#cote point1) +. (self#cote point2) +. (self#cote point3)) /. 3.
        

        method ordonne_triangles triangles =
            let par_cote triangle =
                let p1, p2, p3 = triangle in self#cote_moyenne p1 p2 p3
            in tri_clef par_cote triangles

        method trace_triangle point1 point2 point3 =
            let i = Vec.of_points point1 point2
            and j = Vec.of_points point1 point3 in
            let normale = Vec.unitaire (Vec.produit_vectoriel i j) in
            let v = abs_float (Vec.produit_scalaire normale direction_lumiere) in
            
            let echelle v x =
                int_of_float (v *. (float_of_int x))
            and projection point =
                self#projette2d (self#projette_dans_base point) zoom
            in
            let p1 = projection point1
            and p2 = projection point2
            and p3 = projection point3 in
            let r, g, b = couleur_lumiere in
            Graphics.set_color (Graphics.rgb (echelle v r) (echelle v g) (echelle v b));
            Graphics.fill_poly [| p1; p2; p3 |];
            Graphics.set_color (Graphics.rgb 0 0 0);
            Graphics.draw_poly [| p1; p2; p3 |]
    
        method algo_peintre triangles =
            self#ordonne_triangles triangles;
            let n = Array.length triangles in
            for i = 1 to n do
                let p1, p2, p3 = triangles.(n - i) in
                self#trace_triangle p1 p2 p3;
            done
        
        method algo_peintre_sans_ordonner triangles =
            let n = Array.length triangles in
            for i = 1 to n do
                let p1, p2, p3 = triangles.(n - i) in
                self#trace_triangle p1 p2 p3;
            done


        (* Il est important d'utiliser cette méthode pour le premier affichage d'un objet. *)
        method rafraichit objet =
            Graphics.auto_synchronize false;
            Graphics.clear_graph ();
            self#algo_peintre objet;
            Graphics.auto_synchronize true
        
        (* On peut utiliser cette méthode lorsque l'objet a déjà été ordonné. *)
        method redessine objet =
            Graphics.auto_synchronize false;
            Graphics.clear_graph ();
            self#algo_peintre_sans_ordonner objet;
            Graphics.auto_synchronize true

        method controle objet =
            let v_rotation = ref 0.1
            and v_zoom = ref 1.5 in
            self#rotation_base_ox 0.5;
            self#rotation_base_oy 0.3;
            self#algo_peintre objet;
            let x_souris = ref (-1)
            and y_souris = ref (-1)
            and pressee_avant = ref false in
            while true do
                let evt = Graphics.wait_next_event [Graphics.Poll; Graphics.Key_pressed] in
                if evt.keypressed then
                    begin
                        match evt.key with
                            | 'h' -> self#rotation_base_ox !v_rotation
                            | 'f' -> self#rotation_base_ox ((-1.) *. !v_rotation)
                            | 't' -> self#rotation_base_oy !v_rotation
                            | 'g' -> self#rotation_base_oy ((-1.) *. !v_rotation)

                            | '>' -> self#zoom_vue !v_zoom
                            | '<' -> self#zoom_vue (1. /. !v_zoom)
                            | k -> Printf.printf "%c" k;
                        self#rafraichit objet;
                    end;
                if Graphics.button_down () then
                    begin
                        let x, y = Graphics.mouse_pos () in
                        if !pressee_avant then
                            begin
                                self#translation_axe_ox (float_of_int (x - !x_souris));
                                self#translation_axe_oy (float_of_int (y - !y_souris));
                                self#rafraichit objet;
                            end;
                        x_souris := x;
                        y_souris := y;
                        pressee_avant := true;
                    end
                else
                    pressee_avant := false;
                
            done

        method mouvement_auto objet =
            self#algo_peintre objet;
            let v_translation = ref 5.
            and v_rotation = ref 0.03
            and distance = ref 0. in
            self#rafraichit objet;
            while true do
                let v = !v_translation *. (log (3. +. !distance /. 30.)) *. sin (0.1 +. !distance /. 150.) in
                self#translation_axe_ox v;
                self#translation_axe_oy v;
                self#rotation_base_ox !v_rotation;
                self#rotation_base_oy !v_rotation;
                self#rafraichit objet;
                distance := !distance +. abs_float v;
                Unix.sleepf 0.01;
                if !distance >= 300. then
                    begin
                        distance := 0.;
                        v_translation := (-1.) *. !v_translation;
                        
                    end;
            done


    end



module Fichiers =
    struct

        let lecture_obj nom_fichier =
            let fichier = open_in nom_fichier in
            let points = ref (Td.cree Point.origine) in
            let triangles = ref (Td.cree (Point.origine, Point.origine, Point.origine)) in
            begin
            try
                while true do
                    let ligne = input_line fichier in
                    if (String.length ligne) > 2 then
                        begin
                            if ligne.[0] = 'v' && ligne.[1] = ' ' then
                                Scanf.sscanf ligne "v %f %f %f" (
                                    fun x y z ->
                                        Td.ajoute points {x = x; y = y; z = z}
                                )
                            else if ligne.[0] = 'f' && ligne.[1] = ' ' then
                            let parties = Array.of_list (Str.split (Str.regexp "[ \t]+") ligne)
                            and triangle = Array.make 3 Point.origine in
                            for i = 1 to 3 do
                                Scanf.sscanf parties.(i) "%d" (
                                    fun num_point ->
                                        triangle.(i - 1) <- Td.element points num_point
                                )
                            done;
                            Td.ajoute triangles (triangle.(0), triangle.(1), triangle.(2))
                        end
                done
            with
                End_of_file -> close_in fichier
            end;
            Array.sub !triangles.support 0 !triangles.taille
    end




module Langage =
    struct
        type instruction = string

        type programme = instruction list

        let calcule_score prog = 0

    end


module Jeu =
    struct
    (* OOP *)

        type point2d = int * int

        type mur = {
            point1: point2d;
            point2: point2d;
            hauteur: int;
        }

        type porte = {
            point1: point2d;
            point2: point2d;
            periode: int;
        }


        type trampoline = {
            point1: point2d;
            point2: point2d;
            periode: int;
        }


        type bonus = {
            point: point2d;
            force: int;
        }

        type feuille = {
            point: point2d;
            voisines: feuille list;
        }


        type arbre = {
            entree: feuille;
            sortie: feuille;
        }



        class joueur =
            object(self)
                val mutable x = 0
                val mutable y = 0
                val mutable z = 0
                val mutable batterie = 1.


                (* Crée une boule composée de triangles. *)
                method objet_boule rayon taille_triangles =
                    0
            end
        
        class labyrinthe =
            object(self)
                val mutable graphe: arbre
                val mutable murs_verticaux: (mur array) array
                val mutable murs_horizontaux: (mur array) array
                val mutable portes: porte array

                method cree_mur point1 point2 hauteur = 0
            end
        

        class etat_jeu =
            object(self)
                val mutable laby: labyrinthe
                val mutable niveau: int
                val mutable score: int

                (* Algorithme d'exploration exhaustive avec backtracking. *)
                method genere_labyrinthe largeur hauteur =
                    
                    let visitees = Array.make_matrix hauteur largeur
                    (* FIXME: bonne taille de tableau ? *)
                    and murs_verticaux = Array.make_matrix (hauteur - 1) (largeur - 1)
                    and murs_horizontaux = Array.make_matrix (hauteur - 1) (largeur - 1) in
                    for i = 0 to hauteur - 1 do
                        for j = 0 to largeur - 1 do
                            visitees.(i).(j) = false;
                        done
                    done;
                    
                    
                    let voisines (x, y) =
                        List.filter
                            (fun (i, j) -> i >= 0 && j >= 0 && i < largeur && j < hauteur)
                            [(x - 1, y); (x, y - 1); (x + 1, y); (x, y + 1)]
                    in
                    let enleve_mur (i1, j1) (i2, j2) =
                        murs_horizontaux.(i1).(j1) = false;
                        murs_verticaux.(i1).(j1) = false;
                    in

                    let n = largeur*hauteur - 1 in
                    let precedentes = Array.make n in
                    let i_courante = ref Random.int hauteur
                    and j_courante = Random.int largeur in
                    visitees.(i_courante).(j_courante) = true;
                    precedentes.(0) = (i_courante, j_courante);
                    let murs_enleves = ref 0
                    and indice_derniere_visitee = ref 0 in
                    while !murs_enleves < n do
                        match List.filter
                            (fun (i, j) -> not visitees.(i).(j))
                            voisines(i_courante, j_courante)
                        with
                            | [] -> decr indice_derniere_visitee
                            | non_visitees ->
                            begin
                                let (i, j) = List.nth non_visitees (Random.int (List.length non_visitees)) in

                                i_courante := i;
                                j_courante := j;
                                visitees.(i_courante).(j_courante) = true;
                                incr murs_enleves;
                                incr indice_derniere_visitee;
                            end

                    done;
                    0

                

            end

    end


