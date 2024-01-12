module type MEMORY =
sig
  type memory
  val init : int -> memory
  val get : memory -> int -> int option
  val set : memory -> int -> int option -> unit
  val dump : memory -> int option list
end

module ArrayMemory : MEMORY =
struct
  type memory = int option array
  let init n = Array.make n None
  let get mem index =
    if index < 0 then mem.(0)
    else mem.(index)

  let set mem index value =
    if index < 0 then mem.(0) <- value
    else mem.(index) <- value

  let dump mem = Array.to_list mem
end;;


module ListMemory : MEMORY =
struct
  type memory = int option list ref
  let init n = ref (List.init n (fun _ -> None))

  let rec get mem index =
    if index < 0 then get mem 0
    else
      match !mem with
      | [] -> None
      | x :: xs -> if index = 0 then x else get (ref xs) (index - 1)

  let rec set mem index value =
    let rec aux current_index lst =
      match lst with
      | [] -> []
      | x :: xs ->
          if current_index = index then
            value :: xs
          else
            x :: aux (current_index + 1) xs
    in
    if index < 0 then set mem 0 value
    else mem := aux 0 !mem

  let dump mem = !mem
end ;;


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

(*
module MyRamMachine = RamMachine(ArrayMemory);;
let initial_machine = MyRamMachine.init 10 [Load(1, 7); Load(2, 3); Add(3, 1, 2); Sub(4, 1, 2)];;
MyRamMachine.dump initial_machine;;
MyRamMachine.step initial_machine;;
MyRamMachine.dump initial_machine;;
MyRamMachine.step initial_machine;;
MyRamMachine.dump initial_machine;;
MyRamMachine.step initial_machine;;
MyRamMachine.dump initial_machine;;
MyRamMachine.step initial_machine;;
MyRamMachine.dump initial_machine;;
*)

module MyRamMachineList = RamMachine(ListMemory);;
let initial_machine_list = MyRamMachineList.init 10 [Load(1, 7); Load(2, 3); Add(3, 1, 2); Sub(4, 1, 2);];;
MyRamMachineList.dump initial_machine_list;;
MyRamMachineList.step initial_machine_list;;
MyRamMachineList.dump initial_machine_list;;
MyRamMachineList.step initial_machine_list;;
MyRamMachineList.dump initial_machine_list;;
MyRamMachineList.step initial_machine_list;;
MyRamMachineList.dump initial_machine_list;;
MyRamMachineList.step initial_machine_list;;
MyRamMachineList.dump initial_machine_list;;

