open Gzd3d

let controle espace objet =
    let v_rotation = ref 0.02
    and v_zoom = ref 1.5 in
    espace#algo_peintre objet;
    let x_souris = ref (-1)
    and y_souris = ref (-1)
    and pressee_avant = ref false in
    while true do
        let evt = Graphics.wait_next_event [Graphics.Poll; Graphics.Key_pressed] in
        if evt.keypressed then
            begin
                match evt.key with
                    | 'h' -> espace#rotation_base_ox !v_rotation
                    | 'f' -> espace#rotation_base_ox ((-1.) *. !v_rotation)
                    | 't' -> espace#rotation_base_oy !v_rotation
                    | 'g' -> espace#rotation_base_oy ((-1.) *. !v_rotation)

                    | '>' -> espace#zoom_vue !v_zoom
                    | '<' -> espace#zoom_vue (1. /. !v_zoom)
                    | k -> Printf.eprintf "%c" k;
                espace#rafraichit objet;
            end;
        if Graphics.button_down () then
            begin
                let x, y = Graphics.mouse_pos () in
                if !pressee_avant then
                    begin
                        espace#translation_axe_ox (float_of_int (x - !x_souris));
                        espace#translation_axe_oy (float_of_int (y - !y_souris));
                        espace#rafraichit objet;
                    end;
                x_souris := x;
                y_souris := y;
                pressee_avant := true;
            end
        else
            pressee_avant := false;
        
    done

let mouvement_auto espace objet =
    espace#algo_peintre objet;
    let v_translation = ref 1.
    and v_rotation = ref 0.001
    and distance = ref 0. in
    espace#rafraichit objet;
    while true do
        let v = !v_translation *. (log (3. +. !distance /. 30.)) *. sin (0.1 +. !distance /. 150.) in
        espace#translation_axe_ox v;
        espace#translation_axe_oy v;
        espace#rotation_base_ox !v_rotation;
        espace#rotation_base_oy !v_rotation;
        espace#rafraichit objet;
        distance := !distance +. abs_float v;
        Unix.sleepf 0.01;
        if !distance >= 300. then
            begin
                distance := 0.;
                v_translation := (-1.) *. !v_translation;
                
            end;
    done


let cube =
    let open Geometrie.Point in
    let a = {x = 0.; y = 0.; z = 0.}
    and b = {x = 1.; y = 0.; z = 0.}
    and c = {x = 1.; y = 1.; z = 0.}
    and d = {x = 0.; y = 1.; z = 0.}
    and e = {x = 0.; y = 0.; z = 1.}
    and f = {x = 1.; y = 0.; z = 1.}
    and g = {x = 1.; y = 1.; z = 1.}
    and h = {x = 0.; y = 1.; z = 1.}
    and z = Geometrie.couleur_par_defaut in
    [|
        (a, b, d, z); (b, c, d, z);
        (e, f, h, z); (f, g, h, z);
        (b, f, c, z); (f, g, c, z);
        (a, e, d, z); (e, h, d, z);
        (a, b, e, z); (b, f, e, z);
        (d, c, h, z); (c, g, h, z);
    |]

let test_cube () = 
    Graphics.open_graph "";
    Graphics.resize_window 800 600;
    let objet = cube in
    let espace = new Affichage.espace ~x0:400. ~y0:300. ~zoom:100. in
    espace#rotation_base_ox 0.5;
    espace#rotation_base_oy 0.3;
    mouvement_auto espace objet

let test_obj () = 
    Graphics.open_graph "";
    Graphics.resize_window 800 600;
    let objet = Fichiers.lecture_obj "./assets/ourson.obj" in
    let espace = new Affichage.espace ~x0:400. ~y0:300. ~zoom:50. in
    mouvement_auto espace objet

let test_boule () = 
    Graphics.open_graph "";
    Graphics.resize_window 800 600;
    let objet = Geometrie.Objet.boule 200. ~precision:50 ~couleur:Graphics.red in 
    let espace = new Affichage.espace ~x0:400. ~y0:300. ~zoom:1. in
    mouvement_auto espace objet

let test_murs () = 
    Graphics.open_graph "";
    Graphics.resize_window 800 600;
    let mur1 = Geometrie.Objet.mur_vertical 50. 5. 10. ~couleur:Graphics.yellow
    and mur2 = Geometrie.Objet.mur_horizontal 50. 5. 10. ~couleur:Graphics.blue in
    let objet = Array.append mur1 mur2 in
    let espace = new Affichage.espace ~x0:400. ~y0:300. ~zoom:10. in
    mouvement_auto espace objet

let () = 
    test_murs ();