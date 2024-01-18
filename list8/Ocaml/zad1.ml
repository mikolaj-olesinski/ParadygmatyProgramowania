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
  type memory = (int option ref) list ref
  let init n = ref (List.init n (fun _ -> ref None))

  let rec get mem index =
    if index < 0 then get mem 0
    else
      match !mem with
      | [] -> raise (Invalid_argument "Index out of bounds")
      | x :: xs -> if index = 0 then !x else get (ref xs) (index - 1)

  let rec set mem index value =
    match !mem with
    | [] -> raise (Invalid_argument "Index out of bounds")
    | x :: xs ->
        if index = 0 then x := value
        else set (ref xs) (index - 1) value

  let dump mem = List.map (!) !mem
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
             | (Some val1, Some val2) ->
                 MemoryModule.set machine.memory index (Some (val1 + val2));
             | _ -> ()
            );
            machine.instructions <- rest
        | Sub (index, x, y) ->
            (match (MemoryModule.get machine.memory x, MemoryModule.get machine.memory y) with
             | (Some val1, Some val2) ->
                 MemoryModule.set machine.memory index (Some (val1 - val2));
             | _ -> ()
            );
            machine.instructions <- rest


  let dump machine = MemoryModule.dump machine.memory;
end;;


module MyRamMachine = RamMachine(ArrayMemory);;
let initial_machine = MyRamMachine.init 10 [Load(1, 7); Load(2, 3); Add(3, 1, 2); Sub(4, 1, 2);Add(3, 4, 4);Add(100,100,100);];;
MyRamMachine.dump initial_machine;;
MyRamMachine.step initial_machine;;
MyRamMachine.dump initial_machine;;
MyRamMachine.step initial_machine;;
MyRamMachine.dump initial_machine;;
MyRamMachine.step initial_machine;;
MyRamMachine.dump initial_machine;;
MyRamMachine.step initial_machine;;
MyRamMachine.dump initial_machine;;
MyRamMachine.step initial_machine;;
MyRamMachine.dump initial_machine;;
MyRamMachine.step initial_machine;;
MyRamMachine.dump initial_machine;;


module MyRamMachineList = RamMachine(ListMemory);;
let initial_machine_list = MyRamMachineList.init 10 [Load(1, 7); Load(2, 3); Add(3, 1, 2); Sub(4, 1, 2);Add(3, 4, 4);Add(100,100,100);];;
MyRamMachineList.dump initial_machine_list;;
MyRamMachineList.step initial_machine_list;;
MyRamMachineList.dump initial_machine_list;;
MyRamMachineList.step initial_machine_list;;
MyRamMachineList.dump initial_machine_list;;
MyRamMachineList.step initial_machine_list;;
MyRamMachineList.dump initial_machine_list;;
MyRamMachineList.step initial_machine_list;;
MyRamMachineList.dump initial_machine_list;;
MyRamMachineList.step initial_machine_list;;
MyRamMachineList.dump initial_machine_list;;
MyRamMachineList.step initial_machine_list;;
MyRamMachineList.dump initial_machine_list;;

