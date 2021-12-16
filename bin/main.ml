open Gzd3d.Geometrie
open Gzd3d.Geometrie.Point
open Gzd3d.Geometrie.Vec


let cube =
    let a = {x = 0.; y = 0.; z = 0.}
    and b = {x = 1.; y = 0.; z = 0.}
    and c = {x = 1.; y = 1.; z = 0.}
    and d = {x = 0.; y = 1.; z = 0.}
    and e = {x = 0.; y = 0.; z = 1.}
    and f = {x = 1.; y = 0.; z = 1.}
    and g = {x = 1.; y = 1.; z = 1.}
    and h = {x = 0.; y = 1.; z = 1.}
    in
    [|
        (a, b, d); (b, c, d);
        (e, f, h); (f, g, h);
        (b, f, c); (f, g, c);
        (a, e, d); (e, h, d);
        (a, b, e); (b, f, e);
        (d, c, h); (c, g, h);
    |];;
Graphics.open_graph "";;
Graphics.resize_window 800 600;;
let objet = Gzd3d.Fichiers.lecture_obj "../teapot.obj" in
let instance = new Gzd3d.espace in 
instance#mouvement_auto objet;;