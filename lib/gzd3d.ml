(*
La librairie principale du jeu, GZD (Gnizamedoc).
Implémente le moteur graphique.
*)


module Algorithmes = Algorithmes
module Affichage = Affichage
module Fichiers = Fichiers
module Geometrie = Geometrie
module Langage = Langage
module Labyrinthe = Labyrinthe
module Lexer = Lexer
module Parser = Parser

(**
Transforme un programme sous forme de caractères en une expression.
Cette fonction est en dehors du module Langage pour éviter des dépendances circulaires.
*)
let analyse prog =
    let lexbuf = Lexing.from_string prog in
    Parser.main Lexer.lang lexbuf


type etat_entrees = {
    mutable direction: Labyrinthe.direction option;
    mutable deplacement: Labyrinthe.deplacement;
    mutable souris_x: int;
    mutable souris_y: int;
    mutable souris_pressee: bool;
    mutable rotation: bool;
}

(** Constantes graphiques du jeu. *)
module Jeu =
    struct

        open Geometrie

        let longueur3d_labyrinthe = 300.

        (** Taille (en 3d) du labyrinthe entier. *)
        let taille_labyrinthe = ref Labyrinthe.taille_initiale

        let hauteur_mur = 10.
        let largeur_mur = 5.
        let longueur_mur () = longueur3d_labyrinthe /. (float_of_int !taille_labyrinthe)

        let couleur_joueur = Graphics.blue
        let couleur_murs = Graphics.green
        let couleur_arrivee = Graphics.red

        (** Nombre de rafraichissements de la transition. *)
        let nombre_frames = 10

        (** Temps entre chaque rafraichissement. *)
        let delai = 1. /. 60.

        let rayon_joueur () = longueur_mur () /. 3.        

        (** Vecteur correspondant à un décalage vertical d'un mur. *)
        let vecteur_unitaire_vertical () =
            let open Vec in
            { vx = 0.; vy = 0.; vz = longueur_mur () }

        (** Vecteur correspondant à un décalage horizontal d'un mur. *)
        let vecteur_unitaire_horizontal () =
            let open Vec in
            { vx = ~-.(longueur_mur ()); vy = 0.; vz = 0. }
        
        (** Renvoie le vecteur position d'un point en haut à droite de la cellule de coordonnées (i, j). *)
        let vecteur_position (i, j) =
            let open Vec in
            let vec_v = vecteur_unitaire_vertical () in
            let vec_h = vecteur_unitaire_horizontal () in
            let vi = vec_v @*. (float_of_int i)
            and vj = vec_h @*. (float_of_int j) in
            vi @+ vj

        (** Renvoie le vecteur position d'un point au centre de la cellule de coordonnées (i, j). *)
        let vecteur_position_centre (i, j) =
            let open Vec in
            let vec_v = vecteur_unitaire_vertical () in
            let vec_h = vecteur_unitaire_horizontal () in
            let vi = vec_v @*. (-0.5 +. float_of_int i)
            and vj = vec_h @*. (0.5 +. float_of_int j) in
            vi @+ vj
        
        (**
        Renvoie le vecteur position d'un point
        entre les centres des cellules aux points point1 et point2
        déplacé au pourcentage t (avec 0 <= t <= 1).
        *)
        let vecteur_position_transition point1 point2 t =
            let open Vec in
            assert (0. <= t && t <= 1.);
            let vec1 = vecteur_position_centre point1
            and vec2 = vecteur_position_centre point2 in
            let vec_t = vec2 @- vec1 in
            vec1 @+ (vec_t @*. t)
        

        (** Boule à l'origine du repère, mémoïsée. *)
        let boule_initiale_joueur =
            let cache = Hashtbl.create 10 in
            fun () -> let rayon = rayon_joueur () in match Hashtbl.find_opt cache rayon with
                | None ->
                    (*Printf.eprintf "Génération d'une boule de rayon %f\n" rayon;*)
                    let boule = Objet.boule rayon ~precision:30 ~couleur:couleur_joueur in
                    Hashtbl.add cache rayon boule;
                    boule
                | Some boule -> boule

        (** L'objet du point d'arrivée placé à l'origine du repère. *)
        let case_origine_arrivee () =
            Objet.case (longueur_mur ()) ~couleur:couleur_arrivee ~precision:1
        
        
        (** Crée l'objet du joueur en transition vers un point. *)
        let cree_objet_joueur_transition depart dest t =
            let vec = vecteur_position_transition depart dest t in
            Objet.translate_objet vec (boule_initiale_joueur ())

    end


(** Une classe qui contrôle l'état du jeu, l'affichage graphique et les entrées. *)
class jeu =

    let open Geometrie in
    object (self)
        inherit Labyrinthe.evolutif as super

        val mutable espace = new Affichage.espace ~x0:400. ~y0:600. ~zoom:3.

        val mutable entrees = {
            direction = None;
            deplacement = Labyrinthe.MouvementNormal;
            souris_x = -1;
            souris_y = -1;
            souris_pressee = false;
            rotation = false;
        }


        (** Crée l'objet du joueur, mémoïsé. *)
        method cree_objet_joueur () =            
            let vec = Jeu.vecteur_position_centre point_joueur in
            Objet.translate_objet vec (Jeu.boule_initiale_joueur ())
        
        (** Crée l'objet du point de l'arrivée. *)
        method cree_objet_arrivee () =
            let vec = Jeu.vecteur_position point_arrivee in
            Objet.translate_objet vec (Jeu.case_origine_arrivee ())
        

        val mutable cree_objet_labyrinthe_cache = Hashtbl.create 10

        (**
        Crée l'objet composé des murs du labyrinthe, mémoïsé.
        On suppose que pour une même valeur de `niveau`, l'état du labyrinthe ne change pas.
        *)
        method cree_objet_labyrinthe () =
            match Hashtbl.find_opt cree_objet_labyrinthe_cache self#niveau with
                | Some laby -> laby
                | None ->
                    (*Printf.eprintf "Génération du labyrinthe au niveau %d\n" self#niveau;*)
                    let open Vec in
                    let n = self#taille
                    and mur_vertical = Objet.mur_vertical
                        (Jeu.longueur_mur ())
                        Jeu.largeur_mur
                        Jeu.hauteur_mur
                        ~couleur:Jeu.couleur_murs
                    and mur_horizontal = Objet.mur_horizontal
                        (Jeu.longueur_mur ())
                        Jeu.largeur_mur
                        Jeu.hauteur_mur
                        ~couleur:Jeu.couleur_murs in
                    let vec_v = Jeu.vecteur_unitaire_vertical ()
                    and vec_h = Jeu.vecteur_unitaire_horizontal ()
                    and mur_v = ref mur_vertical
                    and mur_h = ref mur_horizontal
                    and objet = ref [||] in

                    (* Première rangée (extérieure) de murs horizontaux. *)
                    for _j = 1 to n do
                        objet := Array.append !objet !mur_h;
                        mur_h := Objet.translate_objet vec_h !mur_h;
                    done;
                    (* Pour chaque rangée : *)
                    for i = 0 to n-1 do
                        let fi = float_of_int i in
                        (* Premier mur vertical extérieur à gauche, rangée i. *)
                        mur_v := Objet.translate_objet (vec_v @*. fi) mur_vertical;
                        (* Premier mur horizontal à gauche, en bas de la rangée i. *)
                        mur_h := Objet.translate_objet (vec_v @*. (fi +. 1.)) mur_horizontal;
                        objet := Array.append !objet !mur_v;
                        (* Pour chaque colonne : *)
                        for j = 0 to n-1 do
                            mur_v := Objet.translate_objet vec_h !mur_v;

                            if i < n-1 then (
                                if super#mur (i, j) Horizontal then
                                    objet := Array.append !objet !mur_h
                            ) else
                                (* Dernière rangée de murs horizontaux en bas, colonne j. *)
                                objet := Array.append !objet !mur_h;
                            
                            if j < n-1 then (
                                if super#mur (i, j) Vertical then
                                    objet := Array.append !objet !mur_v
                            ) else
                                (* Dernier mur vertical à droite, rangée i. *)
                                objet := Array.append !objet !mur_v;
                            
                            mur_h := Objet.translate_objet vec_h !mur_h;
                        done;
                    done;
                    Hashtbl.add cree_objet_labyrinthe_cache self#niveau !objet;
                    !objet

        
        (** Ouvre la fenêtre et initialise le jeu. *)
        method! initialise () =
            Jeu.taille_labyrinthe := super#taille;
            super#initialise ();
            (*super#affiche ();*)

            let k = Float.pi /. 8. in
            espace#rotation_base_ox k;
            espace#rotation_base_oy k;

            Graphics.open_graph "";
            Graphics.resize_window 1600 900;

        (** Affiche la scène et modifie le titre de la fenêtre. *)
        method! affiche () =
            let laby = self#cree_objet_labyrinthe ()
            and joueur = self#cree_objet_joueur ()
            and arrivee = self#cree_objet_arrivee () in
            let objet = Array.concat [laby; joueur; arrivee] in
            espace#rafraichit objet;
            let title = Printf.sprintf
                "Niveau : %d____Énergie_dépensée : %d/%d____Coût lors de ce niveau: %d"
                niveau conso_totale Labyrinthe.cout_maximum conso_niveau in
            Graphics.set_window_title title;
        
        (** Affiche une image du joueur en transition. *)
        method affiche_transition_joueur depart dest t =
            let laby = self#cree_objet_labyrinthe ()
            and joueur = Jeu.cree_objet_joueur_transition depart dest t
            and arrivee = self#cree_objet_arrivee () in
            espace#rafraichit (Array.concat [laby; joueur; arrivee]);
        
        (** Affiche toutes les images de la transition du joueur en mouvement vers une destination. *)
        method effectue_transition_joueur depart dest num_frames =
            (* On n'effectue pas la transition pour des labyrinthes compliqués. *)
            if niveau < 5 then
                for i = 1 to num_frames do
                    let t = (float_of_int i) /. (float_of_int num_frames) in
                    self#affiche_transition_joueur depart dest t;
                    Unix.sleepf Jeu.delai;
                done;
        
        method relache_clavier () =
            entrees.deplacement <- Labyrinthe.MouvementNormal;
            entrees.direction <- None;
            entrees.rotation <- false;
        
        (** Réagit aux entrées clavier et à la souris pour déplacer le joueur. *)
        method detecte_entrees () =
            let open Labyrinthe in
            if Graphics.key_pressed () then
            begin
                let key = Graphics.read_key () in
                (match key with
                    | 'w' -> entrees.deplacement <- MouvementRapide
                    | 'x' -> entrees.deplacement <- Teleportation
                    | _   -> ());
                if entrees.direction = None then
                (* Nouvelle touche directionnelle enfoncée *)
                begin
                    (match key with
                        | 'z' -> entrees.direction <- Some Haut
                        | 's' -> entrees.direction <- Some Bas
                        | 'd' -> entrees.direction <- Some Droite
                        | 'q' -> entrees.direction <- Some Gauche
                        | _   -> ());
                    match entrees.direction with
                        | Some dir -> ignore (self#effectue_deplacement entrees.deplacement dir)
                        | None -> ()

                end;
            end
            else
                self#relache_clavier ();
            
        
        (**
        Réagit aux mouvements de la souris et aux entrées clavier pour
        déplacer et mettre en rotation le labyrinthe.
        *)
        method modifie_vue () =
            if Graphics.key_pressed () then
                begin
                if Graphics.read_key () = ' ' then
                    entrees.rotation <- true
                end
            else
                entrees.rotation <- false;
            if Graphics.button_down () then
                begin
                    let x, y = Graphics.mouse_pos () in
                    if entrees.souris_pressee then
                        begin
                            espace#translation_axe_ox (float_of_int (x - entrees.souris_x));
                            espace#translation_axe_oy (float_of_int (y - entrees.souris_y));
                        end;
                    entrees.souris_x <- x;
                    entrees.souris_y <- y;
                    entrees.souris_pressee <- true;
                end
            else
                begin
                    let x, y = Graphics.mouse_pos () in
                    if entrees.rotation then
                    begin
                        (* Rotation "sur l'axe (Ox) négatif" autour de l'axe (Oy) *)
                        espace#rotation_base_oy (float_of_int (entrees.souris_x - x) /. 500.);
                        (* Rotation "sur l'axe (Oy) positif" autour de l'axe (Ox) *)
                        espace#rotation_base_ox (float_of_int (y - entrees.souris_y) /. 1000.);
                    end;
                    entrees.souris_x <- x;
                    entrees.souris_y <- y;
                    entrees.souris_pressee <- false;
                end
        

        (**
        Affiche l'animation pour un déplacement du joueur d'un certain type dans une direction.
        Retourne les valeurs de retour des fonctions correspondante de evolutif (converties en int).
        Cette fonction est du type Langage.fonction_deplacement.
        *)
        method effectue_deplacement depl dir =
            let open Labyrinthe in
            let ret = match depl with
                | MouvementNormal ->
                begin
                    let depart = point_joueur in
                    if super#deplace_joueur dir then
                        begin
                            let dest = point_joueur in
                            self#effectue_transition_joueur depart dest Jeu.nombre_frames;
                            1
                        end
                    else
                        0
                end
                | MouvementRapide ->
                begin
                    let depart = point_joueur in
                    let n = super#deplace_rapidement_joueur dir in
                    if n > 0 then
                        begin
                            let dest = point_joueur in
                            self#effectue_transition_joueur depart dest Jeu.nombre_frames;
                        end;
                    n
                end
                | Teleportation ->
                begin
                    let depart = point_joueur in
                    if super#teleporte_joueur dir then
                        begin
                            let dest = point_joueur in
                            self#effectue_transition_joueur depart dest (Jeu.nombre_frames / 2);
                            1
                        end
                    else
                        0
                end
            in
            (*super#affiche ();*)
            ret
        
        method! augmente_niveau () =
            super#augmente_niveau ();
            Jeu.taille_labyrinthe := super#taille;
        
        (** Boucle principale du mode joueur. *)
        method boucle_principale () =
            while not (super#fini ()) do
                self#detecte_entrees ();
                self#modifie_vue ();
                self#affiche ();
                Unix.sleepf Jeu.delai;
                if super#niveau_fini () then
                    self#augmente_niveau ();
            done;
        
        (** Boucle principale du mode programmeur. *)
        method boucle_principale_mode_programmeur () =
            self#affiche ();
            Unix.sleep 1;
            ignore (Fichiers.Programme.lance_editeur ());
            
            (*
            while not (Fichiers.Programme.sauvegarde ()) do
                self#modifie_vue ();
                self#affiche ();
                Unix.sleepf Jeu.delai;
            done;
            *)
            
            (*Printf.eprintf "Analyze du programme...";*)
            let programme = Fichiers.Programme.lit_fichier () in
            try
                let fonc_depl = self#effectue_deplacement in
                Langage.execute (ref (self :> Labyrinthe.evolutif)) (analyse programme) ~fonc_depl;
                if super#niveau_fini () then
                    begin
                        self#augmente_niveau ();
                        self#boucle_principale_mode_programmeur ();
                    end
                else
                    begin
                        Printf.eprintf "L'arrivée n'a pas été atteinte !\n";
                    end
            with
                | Stdlib.Parsing.Parse_error ->
                    Printf.eprintf "Erreur de compilation\n";
                    raise Stdlib.Parsing.Parse_error
                | exc ->
                    Printf.eprintf "Erreur d'exécution\n";
                    raise exc

    end