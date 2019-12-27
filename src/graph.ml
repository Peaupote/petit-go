open Error

type vertex_status = Visited | NotVisited | InProcess

type vertex = {
    label : string;
    mutable adj : vertex list;
    mutable status : vertex_status }

type graph = vertex list

let vertex label = { label = label; adj = []; status = NotVisited }
let find g l = List.find (fun v -> v.label = l) g
let add_node g v_label u_label =
  let v = find g v_label in
  let u = find g u_label in
  v.adj <- u :: v.adj

let rec pp lst g x =
  let rec lookup lst = function
    | [] -> lst
    | y :: _ when y.status = InProcess -> cycle_struct (y.label :: lst)
    | y :: tl when y.status = Visited -> lookup lst tl
    | y :: tl -> lookup (pp lst g y) tl
  in
  x.status <- InProcess;
  let res = x.label :: (lookup lst x.adj) in
  x.status <- Visited;
  res

let topological_sort g =
  let aux lst v = if v.status = NotVisited then pp lst g v else lst in
  List.fold_left aux [] g
