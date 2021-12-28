(** Lecture de fichiers 3D au format OBJ. *)


(** Un tableau dynamique. *)
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


(** Lit un fichier en format OBJ et retourne un objet (tableau de triangles). *)
let lecture_obj nom_fichier =
    let open Geometrie in
    let open Geometrie.Point in
    let fichier = open_in nom_fichier in
    let points = ref (Td.cree Point.origine) in
    let triangles = ref (Td.cree triangle_nul) in
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
                    Td.ajoute triangles (triangle.(0), triangle.(1), triangle.(2), couleur_par_defaut)
                end
        done
    with
        End_of_file -> close_in fichier
    end;
    Array.sub !triangles.support 0 !triangles.taille



(** Fonctions de manipulation du fichier qui contient le programme. *)
module Programme =
    struct

    let fichier_programme = "/tmp/i2pm-gnizamedoc_program.okamlmaze"
    let fichier_verrou = "/tmp/i2pm-gnizamedoc.lock"

    let commande = Printf.sprintf "./lib/code-editor %s %s" fichier_programme fichier_verrou

    (**
    Lance l'éditeur externe dans le terminal en arrière-plan,
    et renvoie son status de terminaison sous forme de promesse.
    *)
    let lance_editeur () =
        Unix.system commande
    
    (** Indique si le fichier a été sauvegardé. *)
    let sauvegarde () =
        Sys.file_exists fichier_verrou
    
    (**
    Lit dans le fichier puis renvoie le programme sous forme de chaîne de caractèprocessus.
    *)
    let lit_fichier () =
        let ch = open_in fichier_programme in
        try
            let s = really_input_string ch (in_channel_length ch) in
            close_in ch;
            s
        with exc ->
            close_in_noerr ch;
            raise exc
            
    end