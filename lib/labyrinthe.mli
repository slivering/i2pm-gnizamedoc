val taille_initiale : int
val cout_maximum : int
val cout_total_maximum : int



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


val deplacement_of_direction : direction -> point2d

val decale_cellule : point2d -> direction -> point2d



(**
Un labyrinthe est un tableau de cellules contenant des murs horizontaux et verticaux.
Par défaut le labyrinthe est rempli de murs (plus pratique pour l'algorithme de génération).
On ne stocke pas les murs extérieurs.
Un labyrinthe valide a pour taille minimale 2x2.
*)
class type t =
    object
        val mutable hauteur : int
        val mutable largeur : int
        val mutable murs_horizontaux : bool array array
        val mutable murs_verticaux : bool array array

        method largeur : int
        method hauteur : int

        (** Met en place le labyrinthe pour le premier niveau. *)
        method initialise : unit -> unit

        (** Indique si la cellule de coordonnées (i, j) est dans le labyrinthe. *)
        method est_dans_grille : int * int -> bool
        
        (** Indique s'il existe un mur vertical ou horizontal en des coordonnées. *)
        method mur : int * int -> mur -> bool

        (**
        Indique si la cellule voisine à un point dans une direction
        est dans le labyrinthe et n'est pas séparée par un mur.
        *)
        method voisine : point2d -> direction -> bool

        (**
        Renvoie true si la voisine d'une cellule dans une direction
        existe et n'en est pas séparée par un mur.
        *)
        method voisine : int * int -> direction -> bool


        method private modifie_murs_verticaux : (bool -> bool) -> point2d -> point2d -> unit
        
        method private modifie_murs_horizontaux : (bool -> bool) -> point2d -> point2d -> unit
        
        method private modifie_mur_vertical : (bool -> bool) -> point2d -> unit
        
        method private modifie_mur_horizontal : (bool -> bool) -> point2d -> unit
        
        method private ajoute_mur_vertical : point2d -> unit

        method private enleve_mur_vertical : point2d -> unit

        method private ajoute_mur_horizontal : point2d -> unit

        method private enleve_mur_horizontal : point2d -> unit

        method private ajoute_murs_verticaux : point2d -> point2d -> unit

        method private enleve_murs_verticaux : point2d -> point2d -> unit


        (**
        Génère aléatoirement les murs du labyrinthe, s'il est initialement rempli de murs.
        C'est un algorithme d'exploration exhaustive avec backtracking.
        *)
        method private genere_auto : unit -> unit
        
        (**
        Prend en argument une fonction f, qui prend en argument deux coordonnées i et j
        et un booléen qui signale si il y a un mur en bas de la cellule (i, j).
        f renvoie une chaîne de caractères unicode de largeur 1
        correspondant à l'état de la cellule.
        *)
        method affiche_func : (int -> int -> bool -> string) -> unit

        method affiche : unit -> unit
end


(**
Stocke le labyrinthe, la position du joueur et le point d'arrivée.
Tout déplacement du joueur ne doit pas le faire sortir du labyrinthe.
*)
class vivant : int -> int ->
    object
        inherit t

        val mutable point_joueur : int * int
        val mutable point_arrivee : int * int

        method point_joueur : int * int
        method point_arrivee : int * int

        method pose_joueur : point2d -> unit
        method pose_arrivee : point2d -> unit

        (** Génère le labyrinthe et met en place le joueur et l'arrivée. *)
        method initialise : unit -> unit
        
        (**
        Crée le graphe orienté acyclique du labyrinthe.
        Si le labyrinthe est bien formé, le graphe comporte toutes les cellules.
        *)
        method graphe : unit -> graphe

        (** Indique si le joueur peut effectuer un déplacement standard dans une direction.  *)
        method peut_deplacer_joueur : direction -> bool

        (**
        Déplace le joueur dans une direction et indique si le déplacement était possible.
        Le déplacement standard coûte 1.
        *)
        method deplace_joueur : direction -> bool

        (**
        Déplace le joueur autant que possible et renvoie le nombre n de déplacements.
        Le déplacement rapide coûte n/2.
        *)
        method deplace_rapidement_joueur : direction -> int

        (**
        Déplace le joueur dans une direction sans être bloqué par les murs
        et indique si le déplacement a été effectué.
        Le coût des déplacements lors du niveau sera multiplié par 2.
        *)
        method teleporte_joueur : direction -> bool

        (** Indique si le joueur a atteint l'arrivée. Le niveau est alors fini. *)
        method niveau_fini : unit -> bool
    end



(**
Stocke le niveau, l'énergie dépensée par le joueur depuis le début, et le labyrinthe.
*)
class evolutif :
    object
        inherit vivant

        val mutable niveau : int
        val mutable conso_totale : int
        val mutable conso_niveau : int
        val mutable nombre_teleportations : int

        (** Le niveau du labyrinthe. *)
        method niveau : int

        (** Le nombre de lignes et le nombre de colonnes du labyrinthe. *)
        method taille : int

        (** L'énergie totale dépensée. *)
        method conso_totale : int

        (** L'énergie dépensée depuis le début du niveau. *)
        method conso_niveau : int

        (** Change le niveau et réinitialise l'état du labyrinthe. *)
        method augmente_niveau : unit -> unit
        
        (*
        Le jeu est fini (perdu) lorsque l'énergie dépensée
        (depuis le début du jeu ou du niveau) a dépassé le maximum.
        *)
        method fini : unit -> bool
end