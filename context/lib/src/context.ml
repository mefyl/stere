include Context_intf

module Default = struct
  let map = ref Hmap.empty
  let get k = Hmap.find k !map

  let[@inline] with_map m cb =
    let old = !map in
    let () = map := m in
    Fun.protect ~finally:(fun () -> map := old) cb

  let with_binding k v cb =
    let new_context = Hmap.add k v !map in
    with_map new_context @@ fun _context -> cb ()

  let without_binding k cb =
    let new_context = Hmap.rem k !map in
    with_map new_context @@ fun _context -> cb ()
end

let storage = ref (module Default : S)
let create_key () = Hmap.Key.create ()

let get k =
  let (module Store) = !storage in
  Store.get k

let with_binding k v cb =
  let (module Store) = !storage in
  Store.with_binding k v cb

let without_binding k cb =
  let (module Store) = !storage in
  Store.without_binding k cb

let set_provider storage_new =
  if storage_new == !storage then () else storage := storage_new
