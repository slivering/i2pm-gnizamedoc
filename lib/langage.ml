(** Utilitaires pour l'analyse et l'exécution d'un programme OKalmaze. *)


(**
OKalmaze est centré autour des expressions.
Une expression est un entier, un accès à une variable ou une fonction prédéfinie.
*)
type expression = Constante of int | Variable of string | Fonction of string * expression list

(*
type programme = expression
type condition = expression
type arguments = expression list
*)

(** La liste des arguments de la fonction `do` est un bloc d'instruction *)
type bloc = expression list

(** Toute expression non triviale est une fonction prédéfinie. *)
type fonction =
    | Set of string * expression
    | Get of string

    | Peek of expression
    | PeekAt of expression * expression * expression
    | Move of expression
    | FastMove of expression
    | Teleport of expression

    | And of expression * expression
    | Or of expression * expression
    | Not of expression

    | Gt of expression * expression
    | Ge of expression * expression
    | Lt of expression * expression
    | Le of expression * expression
    | Eq of expression * expression

    | Add of expression * expression
    | Sub of expression * expression
    | Mul of expression * expression
    | Div of expression * expression
    | Mod of expression * expression

    | Do of bloc
    | If of expression * expression
    | IfElse of expression * expression * expression
    | While of expression * expression


exception ErreurSyntaxique
exception FonctionInconnue of string
exception ArgumentsInvalides
exception VariableNonDefinie of string
exception ModificationConstante of string
exception DivisionParZero
exception CoordonneesInvalides of int * int
exception DirectionInvalide of int


let string_of_expression expr =
    let est_simple expr = match expr with
        Fonction (_, _) -> false
        | _ -> true
    in
    let rec string_of_expression_niveau niveau expr =
        let pad = (String.make niveau '\t') in match expr with
        | Constante n -> string_of_int n
        | Variable v -> v
        | Fonction (f, args) ->
            let sep = if List.for_all (est_simple) args then
                " "
            else
                "\n"
            in
                let args_str = List.map (string_of_expression_niveau (niveau+1)) args in
                Printf.sprintf "%s(%s%s%s)" pad f sep (String.concat sep args_str)
            
    in
    string_of_expression_niveau 0 expr

let compile_fonction nom arguments =
    match nom with
        | "set" -> (match arguments with
            | [Variable var; expr] -> Set (var, expr)
            | _ -> raise ArgumentsInvalides)
        | "get" -> (match arguments with
            | [Variable var] -> Get var
            | _ -> raise ArgumentsInvalides)
        
        | "peek" -> (match arguments with
            | [dir] -> Peek dir
            | _ -> raise ArgumentsInvalides)
        | "peekat" -> (match arguments with
            | [i; j; dir] -> PeekAt (i, j, dir)
            | _ -> raise ArgumentsInvalides)
        | "move" -> (match arguments with
            | [dir] -> Move dir
            | _ -> raise ArgumentsInvalides)
        | "fastmove" -> (match arguments with
            | [dir] -> FastMove dir
            | _ -> raise ArgumentsInvalides)
        | "teleport" -> (match arguments with
            | [dir] -> Teleport dir
            | _ -> raise ArgumentsInvalides)
        
        | "and" -> (match arguments with
            | [expr1; expr2] -> And (expr1, expr2)
            | _ -> raise ArgumentsInvalides)
        | "or" -> (match arguments with
            | [expr1; expr2] -> Or (expr1, expr2)
            | _ -> raise ArgumentsInvalides)
        | "not" -> (match arguments with
            | [expr] -> Not expr
            | _ -> raise ArgumentsInvalides)
        
        | "gt" -> (match arguments with
            | [expr1; expr2] -> Gt (expr1, expr2)
            | _ -> raise ArgumentsInvalides)
        | "ge" -> (match arguments with
            | [expr1; expr2] -> Ge (expr1, expr2)
            | _ -> raise ArgumentsInvalides)
        | "lt" -> (match arguments with
            | [expr1; expr2] -> Lt (expr1, expr2)
            | _ -> raise ArgumentsInvalides)
        | "le" -> (match arguments with
            | [expr1; expr2] -> Le (expr1, expr2)
            | _ -> raise ArgumentsInvalides)
        | "eq" -> (match arguments with
            | [expr1; expr2] -> Eq (expr1, expr2)
            | _ -> raise ArgumentsInvalides)

        | "add" -> (match arguments with
            | [expr1; expr2] -> Add (expr1, expr2)
            | _ -> raise ArgumentsInvalides)
        | "sub" -> (match arguments with
            | [expr1; expr2] -> Sub (expr1, expr2)
            | _ -> raise ArgumentsInvalides)
        | "mul" -> (match arguments with
            | [expr1; expr2] -> Mul (expr1, expr2)
            | _ -> raise ArgumentsInvalides)
        | "div" -> (match arguments with
            | [expr1; expr2] -> Div (expr1, expr2)
            | _ -> raise ArgumentsInvalides)
        | "mod" -> (match arguments with
            | [expr1; expr2] -> Mod (expr1, expr2)
            | _ -> raise ArgumentsInvalides)
        
        | "do" -> (match arguments with
            | [] -> raise ArgumentsInvalides
            | args -> Do args)
        | "if" -> (match arguments with
            | [cdt; expr] -> If (cdt, expr)
            | _ -> raise ArgumentsInvalides)
        | "ifelse" -> (match arguments with
            | [cdt; expr1; expr2] -> IfElse (cdt, expr1, expr2)
            | _ -> raise ArgumentsInvalides)
        | "while" -> (match arguments with
            | [cdt; expr] -> While (cdt, expr)
            | _ -> raise ArgumentsInvalides)
        | f -> raise (FonctionInconnue f)



(**
Cette fonction donne une interface à la classe `jeu`
afin de séparer l'implémentation d'exécution du programme
de l'implémentation graphique.
*)
type fonction_deplacement = Labyrinthe.deplacement -> Labyrinthe.direction -> int


(**

Exécute un programme compilé ("tokenized").
La fonction de déplacement est optionnelle, elle permet de remplacer
les fonctions de déplacement de la classe `vivant` par le fonctions graphiques de la classe `jeu`.
Le contexte est optionnel permet de fournir des variables prédéfinies au programme.
Par défaut le contexte initial est vide.
*)
let execute (laby: Labyrinthe.evolutif ref) ?fonc_depl ?contexte prog =
    let open Labyrinthe in
    let ctx = match contexte with
        | Some table -> table
        | None -> Hashtbl.create 10
    and f_depl = match fonc_depl with
        | Some f -> f
        | None -> (fun depl dir -> match depl with
            | MouvementNormal -> Bool.to_int ((!laby)#deplace_joueur dir)
            | MouvementRapide -> (!laby)#deplace_rapidement_joueur dir
            | Teleportation -> Bool.to_int ((!laby)#teleporte_joueur dir)
        )
    in

    let rec evalue expr = match expr with
        | Constante n -> n
        | Variable var -> (match var with
            | "I_PLAYER" -> let (i, _) = (!laby)#point_joueur in i
            | "J_PLAYER" -> let (_, j) = (!laby)#point_joueur in j
            | "HEIGHT"  -> (!laby)#hauteur
            | "WIDTH" -> (!laby)#largeur
            | "I_GOAL" -> let (i, _) = (!laby)#point_arrivee in i
            | "J_GOAL" -> let (_, j) = (!laby)#point_arrivee in j
            | "LEVEL_CONSUMPTION" -> (!laby)#conso_niveau
            | "TOTAL_CONSUMPTION" -> (!laby)#conso_totale
            | "UP" -> 0
            | "DOWN" -> 1
            | "RIGHT" -> 2
            | "LEFT" -> 3
            | _ -> (match Hashtbl.find_opt ctx var with
                | Some v -> v
                | None -> raise (VariableNonDefinie var)
                ))
        | Fonction (nom, args) ->
        begin
            match compile_fonction nom args with
                | Set (var, expr) ->
                    (match var with
                    | "I_PLAYER"
                        | "J_PLAYER"
                        | "HEIGHT"
                        | "WIDTH"
                        | "I_GOAL"
                        | "J_GOAL"
                        | "LEVEL_CONSUMPTION"
                        | "TOTAL_CONSUMPTION"
                        | "UP"
                        | "DOWN"
                        | "RIGHT"
                        | "LEFT" -> raise (ModificationConstante var)
                    | _ -> ());
                    let v = evalue expr in
                    Hashtbl.replace ctx var v;
                    v
                | Get var -> evalue (Variable var)
                | Peek expr ->
                    let dir = evalue_direction expr in
                    Bool.to_int ((!laby)#peut_deplacer_joueur dir)
                | PeekAt (expr1, expr2, expr3) ->
                    let i = evalue expr1 and j = evalue expr2 and dir = evalue_direction expr3 in
                    if (!laby)#est_dans_grille (i, j) then
                    begin
                        let point_joueur = (!laby)#point_joueur in
                        (!laby)#pose_joueur (i, j);
                        let ok = (!laby)#peut_deplacer_joueur dir in
                        (!laby)#pose_joueur point_joueur;
                        Bool.to_int ok
                    end
                    else
                        raise (CoordonneesInvalides (i, j))
                | Move expr ->
                    let dir = evalue_direction expr in
                    f_depl MouvementNormal dir
                | FastMove expr ->
                    let dir = evalue_direction expr in
                    f_depl MouvementRapide dir
                | Teleport expr ->
                    let dir = evalue_direction expr in
                    f_depl Teleportation dir

                | And (expr1, expr2) -> Bool.to_int ((evalue_bool expr1) && (evalue_bool expr2))
                | Or (expr1, expr2) -> Bool.to_int ((evalue_bool expr1) || (evalue_bool expr2))
                | Not expr -> Bool.to_int (not (evalue_bool expr))

                | Gt (expr1, expr2) -> Bool.to_int ((evalue expr1) > (evalue expr2))
                | Ge (expr1, expr2) -> Bool.to_int ((evalue expr1) >= (evalue expr2))
                | Lt (expr1, expr2) -> Bool.to_int ((evalue expr1) < (evalue expr2))
                | Le (expr1, expr2) -> Bool.to_int ((evalue expr1) <= (evalue expr2))
                | Eq (expr1, expr2) -> Bool.to_int ((evalue expr1) = (evalue expr2))

                | Add (expr1, expr2) -> (evalue expr1) + (evalue expr2)
                | Sub (expr1, expr2) -> (evalue expr1) - (evalue expr2)
                | Mul (expr1, expr2) -> (evalue expr1) * (evalue expr2)
                | Div (expr1, expr2) -> (match evalue expr2 with
                    | 0 -> raise DivisionParZero
                    | n -> (evalue expr1) / n)
                | Mod (expr1, expr2) -> (match evalue expr2 with
                    | 0 -> raise DivisionParZero
                    | n -> (evalue expr1) mod n)

                | Do bloc -> (match bloc with
                    | [] -> raise ArgumentsInvalides
                    | [expr] -> evalue expr
                    | expr::exprs -> ignore (evalue expr); evalue (Fonction ("do", exprs)))
                | If (expr1, expr2) ->
                    if evalue_bool expr1 then evalue expr2 else 0
                | IfElse (expr1, expr2, expr3) ->
                    if evalue_bool expr1 then evalue expr2 else evalue expr3
                | While (expr1, expr2) ->
                    while evalue_bool expr1 do ignore (evalue expr2) done; 0   
        end
    and evalue_direction expr = match (evalue expr) with
        | 0 -> Haut
        | 1 -> Bas
        | 2 -> Droite
        | 3 -> Gauche
        | dir -> raise (DirectionInvalide dir)
    and evalue_bool expr = ((evalue expr) <> 0)
    in
    ignore (evalue prog)


let calcule_score prog = List.length prog
