(** Implémentation de la logique du jeu. *)


let taille_initiale = 5
let cout_maximum = 1000
let cout_total_maximum = 10000



(**
Un point, ou une cellule, est un emplacement du labyrinthe.
Les cellules ont pour coordonnées (i, j)
avec i la coordonnée verticale (ligne) et j horizontale (colonne).
Le point (0, 0) est en haut à gauche du labyrinthe.
*)
type point2d = int * int

type chemin = point2d list

(** Un graphe orienté acyclique qui décrit le labyrinthe. *)
type graphe = {
    points: chemin;
    branches: graphe list option;
}

(**
Les murs d'un labyrinthe sont repérés par un cellule adjacente.
Les murs verticaux sont à droite de la cellule, et les murs horizontaux sont en bas.
*)
type mur = Horizontal | Vertical

(** Les cellules connexes à un point sont dans quatre directions. *)
type direction = Bas | Haut | Droite | Gauche

type deplacement = MouvementNormal | MouvementRapide | Teleportation


let deplacement_of_direction = function
    | Bas -> (1, 0)
    | Haut -> (-1, 0)
    | Droite -> (0, 1)
    | Gauche -> (0, -1)

let decale_cellule point dir =
    let (i, j) = point
    and (di, dj) = deplacement_of_direction dir in
    (i + di, j + dj)


(**
Un labyrinthe est un tableau de cellules contenant des murs horizontaux et verticaux.
Par défaut le labyrinthe est rempli de murs (plus pratique pour l'algorithme de génération).
On ne stocke pas les murs extérieurs.
Un labyrinthe valide a pour taille minimale 2x2.
*)
class t hauteur_init largeur_init =
    object (self)
        val mutable hauteur : int = hauteur_init
        val mutable largeur : int = largeur_init
        val mutable murs_verticaux = Array.make_matrix hauteur_init (largeur_init-1) true
        val mutable murs_horizontaux = Array.make_matrix (hauteur_init-1) largeur_init true

        method largeur = largeur
        method hauteur = hauteur

        (* Génère automatiquement le labyrinthe. *)
        method initialise () =
            murs_verticaux <- Array.make_matrix hauteur (largeur-1) true;
            murs_horizontaux <- Array.make_matrix (hauteur-1) largeur true;
            self#genere_auto ();

        method est_dans_grille (i, j) = i >= 0 && j >= 0 && i < hauteur && j < largeur

        method mur (i, j) = function
            | Vertical -> murs_verticaux.(i).(j)
            | Horizontal -> murs_horizontaux.(i).(j)


        method private modifie_murs_verticaux f point1 point2 = match point1, point2 with
            | (i1, j1), (i2, j2) when j1 = j2 ->
                for i = i1 to i2 do
                    let b = murs_verticaux.(i).(j1) in
                    murs_verticaux.(i).(j1) <- f b
                done
            | _ -> failwith "points mal alignés";
        
        method private modifie_murs_horizontaux f point1 point2 = match point1, point2 with
            | (i1, j1), (i2, j2) when i1 = i2 ->
                for j = j1 to j2 do
                    let b = murs_horizontaux.(i1).(j) in
                    murs_horizontaux.(i1).(j) <- f b
                done
            | _ -> failwith "points mal alignés";
        
        method private modifie_mur_vertical f point = self#modifie_murs_verticaux f point point
        
        method private modifie_mur_horizontal f point = self#modifie_murs_horizontaux f point point
        
        method private ajoute_mur_vertical point = self#modifie_mur_vertical (fun _ -> true) point

        method private enleve_mur_vertical point = self#modifie_mur_vertical (fun _ -> false) point

        method private ajoute_mur_horizontal point = self#modifie_mur_horizontal (fun _ -> true) point

        method private enleve_mur_horizontal point = self#modifie_mur_horizontal (fun _ -> false) point

        method private ajoute_murs_verticaux point = self#modifie_murs_verticaux (fun _ -> true) point

        method private enleve_murs_verticaux point = self#modifie_murs_verticaux (fun _ -> false) point


        (**
        Génère aléatoirement les murs du labyrinthe, s'il est initialement rempli de murs.
        C'est un algorithme d'exploration exhaustive avec backtracking.
        *)
        method private genere_auto () =
            Random.init (int_of_float (1000000000. *. Unix.time ()));
            let visitees = Array.make_matrix hauteur largeur false in
            (* Les cellules voisines dans le labyrinthe. *)
            let voisines x y =
                List.filter
                    self#est_dans_grille
                    [(x-1, y); (x, y-1); (x+1, y); (x, y+1)]
            in
            (* Le nombre de murs total à enlever. *)
            let n = hauteur*largeur - 1 in
            let precedentes = Stack.create () in
            let i_courante = ref (Random.int hauteur)
            and j_courante = ref (Random.int largeur) in
            visitees.(!i_courante).(!j_courante) <- true;
            Stack.push (!i_courante, !j_courante) precedentes;
            let murs_enleves = ref 0 in
            while !murs_enleves < n do
                match List.filter
                    (fun (i, j) -> not visitees.(i).(j))
                    (voisines !i_courante !j_courante)
                with
                    (* Si toutes les voisines sont visitées, on revient sur nos pas. *)
                    | [] ->
                    begin
                        (* On a nécessairement visitées des cellules précédemment. *)
                        ignore (Stack.pop precedentes);
                        let (i, j) = Stack.top precedentes in
                        i_courante := i;
                        j_courante := j;
                    end
                    | non_visitees ->
                    begin
                        (* On choisit une cellule voisine non visitée au hasard. *)
                        let (i, j) = List.nth non_visitees (Random.int (List.length non_visitees)) in
                        (* On enlève le mur entre la cellule courante et sa voisine. *)
                        if i <> !i_courante then
                            self#enleve_mur_horizontal (min i !i_courante, j)
                        else if j <> !j_courante then
                            self#enleve_mur_vertical (i, min j !j_courante);
                        (* On se déplace vers la cellule voisine. *)
                        i_courante := i;
                        j_courante := j;
                        visitees.(!i_courante).(!j_courante) <- true;
                        incr murs_enleves;
                        Stack.push (!i_courante, !j_courante) precedentes;
                    end
            done;
        
        method voisine point dir = let (i, j) = point in match dir with
            | Bas -> i < hauteur-1 && not (self#mur point Horizontal)
            | Haut -> i > 0 && not (self#mur ((i-1), j) Horizontal) 
            | Droite -> j < largeur-1 && not (self#mur point Vertical)
            | Gauche -> j > 0 && not (self#mur (i, (j-1)) Vertical)


        method affiche_func f =
            let horizontal = String.init (2*largeur) (fun k -> " _".[k mod 2]) in
            Printf.eprintf "%s\n" horizontal;
            for i = 0 to hauteur-1 do
                Printf.eprintf "|";
                for j = 0 to largeur-1 do
                    if i < hauteur-1 then 
                        Printf.eprintf "%s" (f i j murs_horizontaux.(i).(j))
                    else
                        Printf.eprintf "%s" (f i j true);
                    if j < largeur-1 then
                        if murs_verticaux.(i).(j) = true then
                            Printf.eprintf "|"
                        else
                            Printf.eprintf " ";
                done;
                Printf.eprintf "|\n";
            done;
        
        method affiche () =
            self#affiche_func (fun _ _ mur_hor -> if mur_hor then "_" else " ");
    end




(**
Stocke le labyrinthe, la position du joueur et le point d'arrivée.
Tout déplacement du joueur ne doit pas le faire sortir du labyrinthe.
*)
class vivant hauteur largeur =
    object (self)
        val mutable point_joueur = (0, 0)
        val mutable point_arrivee = (hauteur - 1, largeur - 1)

        inherit t hauteur largeur as super

        method point_joueur = point_joueur
        method point_arrivee = point_arrivee

        method pose_joueur point = point_joueur <- point
        method pose_arrivee point = point_arrivee <- point

        (** Génère le labyrinthe et met en place le joueur et l'arrivée. *)
        method! initialise () =
            super#initialise ();
            self#pose_joueur (0, 0);
            self#pose_arrivee (hauteur - 1, largeur - 1);

            let max_en_profondeur (c1, p1) (c2, p2) = if p1 > p2 then (c1, p1) else (c2, p2)
            in
            let rec feuille_plus_profonde gr = match gr.branches with
                | None -> (List.hd (List.rev gr.points), 0)
                | Some branches ->
                    let feuilles_profondes = List.map feuille_plus_profonde branches in
                    let (c, p) = List.fold_left max_en_profondeur ((0, 0), 0) feuilles_profondes in
                    (c, 1 + p)
            in
            let gr = self#graphe () in
            match gr.branches with
                | None -> self#pose_arrivee (List.hd (List.rev gr.points))
                | Some (branche1::branche2::_) ->
                        let (f1, _) = feuille_plus_profonde branche1
                        and (f2, _) = feuille_plus_profonde branche2 in
                        self#pose_joueur f1;
                        self#pose_arrivee f2;
                | _ -> failwith "Mauvais graphe"

        method peut_deplacer_joueur dir = super#voisine point_joueur dir
        
        method deplace_joueur dir =
            if self#peut_deplacer_joueur dir then
                let (i, j) = point_joueur
                and (di, dj) = deplacement_of_direction dir in
                point_joueur <- (i + di, j + dj);
                Printf.eprintf "Déplacement (%d %d) -> (%d %d) \n" i j (i+di) (j+dj);
                true
            else
                false
        
        method deplace_rapidement_joueur dir =
            let n =  ref 0 in
            let (i1, j1) = point_joueur in
            while self#peut_deplacer_joueur dir do   
                let (i, j) = point_joueur
                and (di, dj) = deplacement_of_direction dir in
                point_joueur <- (i + di, j + dj);
                incr n;
            done;
            let (i2, j2) = point_joueur in
            if !n > 0 then
                Printf.eprintf "Déplacement rapide (%d %d) -> (%d %d) \n" i1 j1 i2 j2;
            !n
        
        method teleporte_joueur dir =
            let (i, j) = point_joueur in
            if match dir with
                | Bas -> i < hauteur - 1
                | Haut -> i > 0
                | Droite -> j < largeur - 1
                | Gauche -> j > 0
            then
            begin
                point_joueur <- decale_cellule point_joueur dir;
                let (i2, j2) = point_joueur in
                Printf.eprintf "Téléportation (%d %d) -> (%d %d) \n" i j i2 j2;
                true
            end
            else
                false
        
        method niveau_fini () = point_joueur = point_arrivee

        method graphe () =
            let deja_vu = Array.make_matrix hauteur largeur false in
            let rec graphe_depuis point =
                let (i, j) = point in
                deja_vu.(i).(j) <- true;
                (* La liste des directions dans lesquelles on peut se déplacer. *)
                let directions = List.filter (super#voisine point) [Haut; Bas; Droite; Gauche] in
                (* Les cellules voisines non séparées par un mur. *)
                let voisines = List.map (decale_cellule point) directions in
                (* Graphe acyclique : on ne revient pas en arrière. *)
                match List.filter (fun (i2, j2) -> not deja_vu.(i2).(j2)) voisines with
                    | [] -> (* Le point est en bout de branche. *)
                        {
                            points = [point];
                            branches = None;
                        }
                    | [voisine] -> (* L'unique cellule voisine continue la branche. *)
                        let graphe_voisine = graphe_depuis voisine in
                        {
                            points = point :: graphe_voisine.points;
                            branches = graphe_voisine.branches;
                        }
                    | voisines -> (* Le graphe se subdivise en plusieurs branches. *)
                        {
                            points = [point];
                            branches = Some (List.map graphe_depuis voisines);
                        }
            in
            graphe_depuis point_joueur
        
        method! affiche () =
            super#affiche_func (fun i j mur_hor -> match (i, j) with
                | coords when coords = point_joueur -> if mur_hor then "O̲" else "O" 
                | coords when coords = point_arrivee -> if mur_hor then "X̲" else "X"
                | _ -> if mur_hor then "_" else " ")
    end



(**
Stocke le niveau, l'énergie dépensée par le joueur depuis le début, et le labyrinthe.
*)
class evolutif =
    object
        inherit vivant taille_initiale taille_initiale as super

        val mutable niveau = 1
        val mutable conso_totale = 0
        val mutable conso_niveau = 0
        val mutable nombre_teleportations = 0

        method taille = largeur
        method niveau = niveau
        method conso_totale = conso_totale
        method conso_niveau = conso_niveau

        
        method! initialise () =
            largeur <- taille_initiale;
            hauteur <- taille_initiale;
            super#initialise ()
        

        method! deplace_joueur dir =
            let ok = super#deplace_joueur dir in
            if ok then
                conso_niveau <- conso_niveau + 1;
            super#affiche ();
            ok
        
        method! deplace_rapidement_joueur dir =
            let n = super#deplace_rapidement_joueur dir in
            conso_niveau <- conso_niveau + (n + 1) / 2;
            super#affiche ();
            n
        
        method! teleporte_joueur dir =
            if super#teleporte_joueur dir then (
                nombre_teleportations <- nombre_teleportations + 1;
                super#affiche ();
                true
            ) else
                false
        
        
        method augmente_niveau () =
            conso_niveau <- conso_niveau * (1 lsl nombre_teleportations) / niveau;
            conso_totale <- conso_totale + conso_niveau;
            conso_niveau <- 0;
            nombre_teleportations <- 0;

            niveau <- niveau + 1;
            largeur <- largeur + 1;
            hauteur <- hauteur + 1;
            super#initialise ();
            Printf.eprintf "Passage au niveau %d !\n" niveau;
            super#affiche ();
        
        (*
        Le jeu est fini (perdu) lorsque l'énergie dépensée
        (depuis le début du jeu ou du niveau) a dépassé le maximum.
        *)
        method fini () = conso_totale > cout_total_maximum || conso_niveau > cout_maximum


        method! affiche () =
            super#affiche ();
            Printf.eprintf
                "Niveau : %d\nÉnergie_dépensée : %d/%d \nCoût lors de ce niveau: %d\n"
                niveau conso_totale cout_maximum conso_niveau;
        
end