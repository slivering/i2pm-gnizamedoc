(** Algorithmes de résolution de labyrinthe. *)



type resolution = Labyrinthe.evolutif -> string

let analyse prog =
    let lexbuf = Lexing.from_string prog in
    Parser.main Lexer.lang lexbuf


(** Détermine la direction à emprunter pour passer d'un point à un autre. *)
let direction_of_points (i1, j1) (i2, j2) =
    let open Labyrinthe in
    match (i2-i1, j2-j1) with
        | (-1, 0) -> Haut
        | (1, 0) -> Bas
        | (0, 1) -> Droite
        | (0, -1) -> Gauche
        | _ -> failwith "Cellules non connexes"

(**
Crée une liste de directions que le joueur doit emprunter pour parcourir un chemin.
*)
let directions_of_chemin points =
    let rec aux points precedent = match points with
        | [] -> []
        | point::autres -> (direction_of_points precedent point) :: aux autres point
    in
    aux (List.tl points) (List.hd points)

(**
Génère un programme à partir d'une liste de directions
dans lesquelles le joueur doit se déplacer.
*)
let programme_of_directions dirs =
    let rec aux dirs =
        let open Labyrinthe in match dirs with
            | [] -> ""
            | dir::autres ->
                let code = (match dir with
                    | Haut -> 0
                    | Bas -> 1
                    | Droite -> 2
                    | Gauche -> 3) in
                (Printf.sprintf "(move %d)" code) ^ aux autres
    in
    Printf.sprintf "(do %s)" (aux dirs)

(* Utilise exclusivement des téléportations. *)
let triche laby =
    let prog = ref "(do " in
    for _ = 1 to laby#taille do
        prog := !prog ^ "(teleport 2) (teleport 1)"
    done;
    !prog ^ ")"


exception AucuneBranche

(*
Résout le labyrinthe par parcours de graphe.
FIXME: réussit de moins en moins avec des grands labyrinthes...
*)
let force_brute laby =
    let open Labyrinthe in

    let arrivee = laby#point_arrivee in

    (* Renvoie les premiers points de la liste jusqu'à l'arrivée, incluse. *)
    let rec extrait_avant_arrivee points = match points with
        | [] -> []
        | point::autres ->
            if point = arrivee then
                [arrivee]
            else
                point :: extrait_avant_arrivee autres
    in
    (* Donne le chemin à emprunter pour atteindre la sortie. *)
    let rec resout_graphe gr =
        let court_circuit = extrait_avant_arrivee gr.points in
        if court_circuit != [] && (List.hd (List.rev court_circuit)) = arrivee then
            (* Les premiers points du graphe mènent d'emblée à l'arrivée. *)
            (* La résolution du labyrinthe se termine toujours par un court-circuit. *)
            court_circuit
        else
            match gr.branches with
                (*
                Le chemin à suivre commence au moins par les premiers points du graphe.
                Il reste à essayer chaquer branche.
                *)
                | Some (branche::suivantes) -> gr.points @ essaie branche suivantes
                | _ -> raise AucuneBranche
    and essaie branche suivantes =
        (* On essaie la première branche. *)
        try
            (* On essaie la première branche. *)
            resout_graphe branche
        with
            AucuneBranche -> (
                match suivantes with
                    | [] -> failwith "Labyrinthe impossible"
                    | suivante::autres -> essaie suivante autres)
    
    in
    let gr = laby#graphe () in
    let chem = resout_graphe gr in
    let dirs = directions_of_chemin chem in
    programme_of_directions dirs