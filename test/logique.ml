
open Gzd3d.Labyrinthe

let rec extrait_cellules graphe = match graphe.branches with
    | None -> graphe.points
    | Some branches -> graphe.points @ List.concat_map extrait_cellules branches


let () =
    let laby = new evolutif in
    laby#initialise ();
    for _ = 1 to 10 do
        laby#augmente_niveau ();
        let taille = laby#taille in
        let graphe = laby#graphe () in
        assert (List.length (extrait_cellules graphe) = taille * taille);
        laby#affiche ();
    done;