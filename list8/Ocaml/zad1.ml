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


module RamMachine (MemoryModule : MEMORY) =
struct
  type instruction = Load of int*int | Add of int*int*int | Sub of int*int*int
  type machine = { memory: MemoryModule.memory; instructions: instruction list }

  let init size instructions =
    { memory = MemoryModule.init size; instructions }

  let rec step machine =
    match machine.instructions with
    | [] -> None
    | instruction :: rest ->
        match instruction with
        | Load (d, v) ->
            MemoryModule.set machine.memory d (Some v);
            Some { machine with instructions = rest }
        | Add (d, a1, a2) ->
            let v1 = MemoryModule.get machine.memory a1 in
            let v2 = MemoryModule.get machine.memory a2 in
            (match (v1, v2) with
             | (Some x, Some y) ->
                 MemoryModule.set machine.memory d (Some (x + y));
                 Some { machine with instructions = rest }
             | _ -> None)
        | Sub (d, a1, a2) ->
            let v1 = MemoryModule.get machine.memory a1 in
            let v2 = MemoryModule.get machine.memory a2 in
            (match (v1, v2) with
             | (Some x, Some y) ->
                 MemoryModule.set machine.memory d (Some (x - y));
                 Some { machine with instructions = rest }
             | _ -> None)



  let dump machine = MemoryModule.dump machine.memory;
end;;

module MyRamMachine = RamMachine(ArrayMemory);;
let initial_machine = MyRamMachine.init 10 [Load(1, 7); Load(2, 3)];;
MyRamMachine.step initial_machine;;
MyRamMachine.dump initial_machine;;
MyRamMachine.step initial_machine;;
MyRamMachine.dump initial_machine;;
