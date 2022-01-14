# i2pm-gnizamedoc

## Synopsis

Votre robot-boule est enfermé dans un labyrinthe ! Malheureusement, sa batterie est limitée.
Il doit consommer le moins d'énergie possible. Serez vous capable de le faire sortir ?

## Présentation générale

Gnizamedoc est un jeu de labyrinthe 3D où le personnage est une boule.
L'utilisateur peut déplacer manuellement ou programmatiquement le personnage
afin d'atteindre le point d'arrivée avec le moins de déplacements possibles.
La taille du labyrinthe augmente à chaque niveau.

___

## Contrôle du labyrinthe

Maintenez la souris pressée pour déplacer le labyrinthe avec la souris.
Maintenez espace pour mettre le labyrinthe en rotation avec la souris.

## Mode joueur

Vous contrôlez manuellement le personnage.
Le but est de minimiser le coût énergétique total des déplacements.
Pressez les touches directionnelles Z, Q, S, D.
Les commandes possibles sont :
- déplacement directionnel : appuyez simplement sur une touche directionnelle (ajoute 10 au coût)

- déplacement directionnel jusqu'au mur : appuyez aussi sur W (ajoute 5 fois le nombre de cellules parcourues au coût)

- déplacement directionnel mode téléportation : appuyez aussi sur X (multiplie le coût après la fin du niveau par 5)

Les déplacements ne peuvent pas faire sortir le personnage du labyrinthe.

Le jeu est fini lorsque la consommation du niveau excède 1000 ou que l'énergie dépensée depuis le début dépasse 10000.

## Mode programmeur
À chaque niveau un éditeur de texte est ouvert, il vous faudra entrer et sauvegarder le programme.
Vous utilisez le langage interne "OKalmaze" pour déplacer votre joueur.
OKalmaze est un langage interprété inspiré de l'assembleur et de LISP.

### Expressions, variables et fonctions

Un programme est composé récursivement d'expressions, dont les délimiteurs sont les parenthèses et les espaces.
Une expression est d'une des formes suivantes :
- `(f expr1 expr2 ... exprn)` où `f` est une fonction prédéfinie, et `expr1`, `expr2`... sont soit des expressions.

- `val` où `val` est une constante sous forme d'entier signé.

- `var` où `var` est un identifiant de variable (de la forme `[a-zA-Z0-9_][a-zA-Z0-9_]*` non numérique).
Toutes les fonctions prédéfinies ont au moins un argument et renvoient une valeur.
Il n'y a pas de distinction entre instruction, procédure et fonction.
Donner le mauvais nombre d'arguments à une fonction est une erreur de compilation.

Les variables contiennent des entiers signés. Les fonctions suivantes permettent de les manipuler :
- `(set var expr)` assigne la valeur de retour de `expr` dans `var` et renvoie `expr`

- `(get var)` renvoie la valeur de `var`. Accéder à une variable non définie est une erreur d'exécution.

Les variables globales sont :
- `I_PLAYER`, `J_PLAYER` : les coordonnées du joueur

- `HEIGHT`, `WIDTH` : la taille du labyrinthe

- `I_GOAL`, `J_GOAL` : les coordonnées d'arrivée

- `LEVEL_CONSUMPTION` : Le coût énergétique déplacements effectués par le joueur depuis le début du niveau.

- `TOTAL_CONSUMPTION` : Le coût énergétique total des déplacements effectués par le joueur.

Tenter de les modifier est une erreur d'exécution.

Une direction `d` est un entier entre 0 et 3 dans les fonctions de déplacement.
Donner une direction invalide est une erreur d'exécution.
Les constantes directionnelles sont définies pour l'ergonomie :
- `UP = 0`
- `DOWN = 1`
- `RIGHT = 2`
- `LEFT = 3`

Tenter de les modifier est une erreur d'exécution.

Les fonctions de déplacement sont les suivantes :
`(peek dir)` Renvoie `1` si le joueur peut se déplacer dans la direction `dir` (`LEVEL_CONSUMPTION += 1`)

`(peekat i j dir)` -> Renvoie `1` si le joueur était en `(i, j)` et pouvait se déplacer dans la direction `dir` (`LEVEL_CONSUMPTION += 5`). Lève une erreur d'exécution si `(i, j)` sont des coordonnées invalides.

`(move dir)` : Déplace le joueur dans la direction `dir`, renvoie `(peek dir)` (`LEVEL_CONSUMPTION += 10`)

`(fastmove dir)` : Déplace le joueur dans la direction `dir` n fois jusqu'au mur et renvoie `n` (`LEVEL_CONSUMPTION += 5*n`)

`(teleport dir)` : Déplace le joueur dans la direction `dir` même si un mur bloque. Renvoie `0` si le joueur était sur le bord. (`LEVEL_CONSUMPTION *= 2`)

### Flot de contrôle

Les booléens d'OKalmaze ont le même type que les entiers.
Les fonctions booléennes renvoient `0` ou `1`.
L'évaluation booléenne d'un entier n repose sur l'équivalence `n = true <-> n <> 0`.

Les opérateurs logiques sont les suivants :
- `(and expr1 expr2)`

- `(or expr1 expr2)`

- `(not expr)`

Les relations booléennes de comparaison sont les suivantes :
- `(gt expr1 expr2)`

- `(lt expr1 expr2)`

- `(ge expr1 expr2)`

- `(le expr1 expr2)`

- `(eq expr1 expr2)`

Les fonctions arithmétiques sont les suivantes :
- `(add expr1 expr2)`

- `(sub expr1 expr2)`

- `(mul expr1 expr2)`

- `(div expr1 expr2)` (peut déclencher une exception si division par zéro)

- `(mod expr1 expr2)` (peut déclencher une exception si division par zéro)

La syntaxe du flot de contrôle s'effectue avec des fonctions.
- `(do instr1 instr2 ... instrn)` : exécute (évalue) les expressions `instr1` `instr2` ... et renvoie `instrn`.

  Remarquez que le programme est constitué d'une **unique** expression. La fonction `do` permet donc
  d'évaluer séquentiellement des fonctions.
  
- `(while expr instr)` : tant que `expr <> 0` évalue `instr`, puis renvoie `0`
- `(if expr instr)` : si `expr <> 0` exécute et renvoie `instr`
- `(ifelse expr instr1 instr2)` : si `expr <> 0` exécute `instr1`, sinon exécute `instr2`; renvoie `expr`

### Exemples

- Le plus simple programme :
```lisp
0
```

- Calcul de la suite de Fibonacci :
```lisp
(do
  (set a 1)
  (set b 1)
  (set i 0)
  (while (gt i 10) (do
    (set sum (add a b))
    (set a b)
    (set b sum)
    ))
  )
```

- Mouvement pseudo-aléatoire :
```lisp
(do
  (set i 23)
  (while 1) (do
    (set i (add i 1))
    (if (eq (mod i 5) 0)
      (set i (sub i 3)))
    (set i (mul 2 i))
    (set i (add (mod i 3) i))
    (set dir i)
    (set dir (mod dir 4))
    (if (lt dir 0)
      (set dir (mul -1 dir))
      )
    (move dir)
    )
  )
)
```

### Terminaison

Le programme est automatiquement terminé lorsque le joueur atteint l'arrivée (qui entraîne le passage au niveau suivant).
Si une erreur de syntaxe ou d'exécution est levée, ou `LEVEL_CONSUMPTION > 1000` ou `TOTAL_CONSUMPTION > 10000`,
le jeu est terminé.


## Compilation
Vous devez avoir installé OCaml, Dune (optionnellement `vscode`) sur un système de type Unix.
La commande de l'éditeur du programme est donné par la variable shell `$EDITOR` si elle est définie, sinon `code -w`.

**ATTENTION** :
- La commande de l'éditeur doit être bloquante jusqu'à ce que le fichier est enregistré ou l'éditeur fermé.
- `nano` lit ne fonctionne pas bien en arrière-plan et n'est donc pas un éditeur compatible.

### Mode joueur
```bash
dune build
dune exec ./bin/mode_joueur/main.exe
```

### Mode programmeur
```bash
dune build
dune exec ./bin/mode_programmeur/main.exe #lance vscode par défaut, échoue si la commande `code` n'est pas installée
EDITOR=kate dune exec ./bin/mode_programmeur/main.exe #pour utiliser `kate` comme éditeur de texte
```


## Plan de l'implémentation

Ce projet fournit un programme, mais aussi des implémentations réutilisables.

- La logique du jeu : le module `labyrinthe.ml` implémente les principales abstractions.

- La géométrie : le module `geometrie.ml` fournit quelques structures et fonctions mathématiques
génériques et réutilisables.

- Le rendu : le module `affichage.ml` fournit une classe qui permet d'afficher n'importe quelle liste
de triangles à l'aide du module `geometrie.ml`.

- La manipulation de fichiers : le module `fichiers.ml` permet la lecture de fichiers OBJ
ainsi qu'une partie de l'implémentation de l'éditeur de texte.

- Le langage et ses algorithmes : Le langage interne du jeu, OKalmaze (quoique quasi Turing-complet)
est implémenté dans `lexer.mll`, `parser.mly` et `langage.ml` et des exemples d'exploitations sont fournis dans `algorithmes.ml`. Nécessite les modules externes OCamlLex et Menhir.

- Le programme principal est codé par `gzd3d.ml`.

## Limitations et pistes d'améliorations

### Bugs
#### Graphics ne trouve pas la police d'écriture correcte
Ce genre d'erreur est difficile à éviter car l'implémentation de cette fonctionnalité de Graphics
dépend de la plateforme. Par défaut, la méthode `Gzd3d.jeu#affiche_stats` utilise la police
Deja Vu Sans Mono Medium (`-misc-dejavu sans mono-medium-r-normal--24-0-0-0-m-0-iso8859-1`).

Pour corriger le bug, enlevez cette ligne (mais l'affichage de la police sera inadéquat).

### Performance
Il est possible que le rendu du labyrinthe ralentisse pour des niveaux élevés.
Comme la plupart des objets ne se chevauchent pas, l'implémentation de l'algorithme du peintre
n'est pas moins efficace que les autres (par ailleurs le nombre de triangles qui composent le joueur
est rapidement dépassé par celui des murs).

Cela est (en grande partie) dû à l'implémentation de l'algorithme du peintre qui effectue
un remplissage séquentiel des triangles sans exploiter de parallélisme matériel.
Il me paraît malheureusement difficile d'optimiser davantage la performance.

### Esthétique
En bref : pour ne pas alourdir l'implémentation, le jeu n'est pas très beau.

Le joueur est une boule dont la surface est triangulée à l'aide de coordonnées sphériques.
La répartition des boules est inhomogène pour des raisons non éclaircies.

Le rendu du labyrinthe est extrêmement simple, les effets d'ombrage s'effectuent par triangle,
ce qui ne produit pas de beaux résultats. Cependant il n'est pas viable d'augmenter le nombre
de triangles sans sacrifier complètement la performance du jeu.

Quelques informations sont affichées textuellement sur l'écran, la qualité de l'affichage
varie selon l'implémentation de Graphics.


### Algorithmes de résolution
Les bases du langage OKalmaze et de la logique du jeu sont implémentés dans leurs modules respectifs,
d'autres algorithmes que ceux proposés peuvent être implémentés (directement dans le langage interne,
ou codés en OCaml directement pour plus de flexibilité).