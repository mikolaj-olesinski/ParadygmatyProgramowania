module type MEMORY =
sig
  type memory

  val init : int -> memory
  val get : memory -> int -> int option
  val set : memory -> int -> int -> memory
  val dump : memory -> int list
end

module ArrayMemory : MEMORY =
struct
  type memory = int array

  let init size = Array.make size 0

  let get mem index =
    if index < 0 || index >= Array.length mem then
      None
    else
      Some mem.(index)

  let set mem index value =
    if index >= 0 && index < Array.length mem then
      mem.(index) <- value;
    mem

  let dump mem = Array.to_list mem
end


module RAMMachine (MemoryImpl : MEMORY) =
 struct
  type memory = MemoryImpl.memory
  type instruction = Load of int * int | Add of int * int * int | Sub of int * int * int

  let init size instructions =
    let memory = MemoryImpl.init size in
    (memory, instructions)

  let dump (memory, _) = MemoryImpl.dump memory

end
