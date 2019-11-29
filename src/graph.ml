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
  let rec lookup = function
    | [] -> None
    | y :: _ when y.status = InProcess -> Some (y.label, y.label :: [])
    | y :: tl when y.status = Visited -> lookup tl
    | y :: tl ->
       match pp g y with
       | Some (z, cycle) when z = x.label -> Some (z, x.label :: y.label :: cycle)
       | Some (z, cycle) -> Some (z, y.label :: cycle)
       | None -> lookup tl

  in
  x.status <- InProcess;
  let res = lookup x.adj in
  x.status <- Visited;
  res

let has_cycle g =
  let rec aux = function
    | [] -> None
    | x :: tl when x.status = Visited -> aux tl
    | x :: tl -> match pp g x with
                | None -> aux tl
                | c -> c
  in
  match aux g with
  | Some (_, c) -> Some c
  | None -> None
