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

let rec pp g x =
  x.status <- InProcess;
  let ret = List.exists (fun y -> y.status = InProcess || pp g y) x.adj in
  x.status <- Visited;
  ret

let has_cycle g = List.exists (fun v -> v.status = NotVisited && pp g v) g
