let () =
    let jeu = new Gzd3d.jeu in
    jeu#initialise ();
    jeu#boucle_principale ();