module type MEMORY =
sig
  type memory
  val init : int -> memory
  val get : memory -> int -> int option
  val set : memory -> int -> int option -> unit
  val dump : memory -> int option list
end;;

module ArrayMemory : MEMORY =
struct
  type memory = int option array
  let init n = Array.make n None
  let get mem index = mem.(index)
  let set mem index value = mem.(index) <- value
  let dump mem = Array.to_list mem
end

module RamMachine  = functor (MemoryModule : MEMORY) ->
struct
  type instruction = Load of int * int | Add of int * int * int | Sub of int * int * int
  type machine = { memory: MemoryModule.memory; mutable instructions: instruction list }

  let init size instructions =
    { memory = MemoryModule.init size; instructions = instructions }

  let step machine =
    match machine.instructions with
    | [] -> ()
    | instruction :: rest ->
        match instruction with
        | Load (index, value) ->
            MemoryModule.set machine.memory index (Some value);
            machine.instructions <- rest
        | Add (index, x, y) ->
            (match (MemoryModule.get machine.memory x, MemoryModule.get machine.memory y) with
             | (val1, val2) ->
                 (match (val1, val2) with
                  | (Some v1, Some v2) ->
                      MemoryModule.set machine.memory index (Some (v1 + v2));
                      machine.instructions <- rest
                  | _ -> ()
                 )
            )
        | Sub (index, x, y) ->
            (match (MemoryModule.get machine.memory x, MemoryModule.get machine.memory y) with
             | (val1, val2) ->
                 (match (val1, val2) with
                  | (Some v1, Some v2) ->
                      MemoryModule.set machine.memory index (Some (v1 - v2));
                      machine.instructions <- rest
                  | _ -> ()
                 )
            )

  let dump machine = MemoryModule.dump machine.memory;
end;;

module MyRamMachine = RamMachine(ArrayMemory);;
let initial_machine = MyRamMachine.init 10 [Load(1, 7); Load(2, 3)];;
MyRamMachine.step initial_machine;;
MyRamMachine.dump initial_machine;;
MyRamMachine.step initial_machine;;
MyRamMachine.dump initial_machine;;
