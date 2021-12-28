open Gzd3d

let analyse prog =
    let lexbuf = Lexing.from_string prog in
    Parser.main Lexer.lang lexbuf


(*
Test d'algorithme sans résolution du labyrinthe.
*)
let test_headless () =
    let laby = new Labyrinthe.evolutif in
    let fibonacci = "
(do
    (set a 1)
    (set b 1)
    (set i 0)
    (while (lt i n) (do
        (set tmp b)
        (set b (add a b))
        (set a tmp)
        (set i (add i 1))
        )
    )
)" in
    let ctx = Hashtbl.create 10 in
    Hashtbl.add ctx "n" 10;
    print_endline fibonacci;
    let prog = analyse fibonacci in
    print_endline (Langage.string_of_expression prog);
    Langage.execute (ref laby) ~contexte:ctx prog;
    (* Le 12ème nombre de Fibonacci est 144 *)
    assert (Hashtbl.find ctx "b" = 144)

let test_triche () =
    let laby = new Labyrinthe.evolutif in
    laby#initialise ();
    let prog = analyse (Algorithmes.triche laby) in
    Langage.execute (ref laby) prog;
    assert (laby#niveau_fini ())

let test_force_brute () =
    let laby = new Labyrinthe.evolutif in
    laby#initialise ();
    let prog = analyse (Algorithmes.force_brute laby) in
    Langage.execute (ref laby) prog;
    assert (laby#niveau_fini ())

let test_graphique () =
    let j = new jeu in
    j#initialise ();
    for _ = 0 to 2 do
        j#augmente_niveau ()
    done;
    let prog = analyse (Algorithmes.force_brute ((j :> Labyrinthe.evolutif))) in
    let fonc_depl = j#effectue_deplacement in
    Langage.execute (ref (j :> Labyrinthe.evolutif)) prog ~fonc_depl;
    assert (j#niveau_fini ())

let () =
    test_headless ();
    test_triche ();
    test_force_brute ();
    test_graphique ();