(*
                         CS 51 Problem Set 6
                                Search

                            Puzzle Solving
 *)

(*======================================================================
Before working on this problem set, read the problem set 6 writeup in
the file `readme.pdf`. It provides context and crucial information for
completing the problems. In addition, make sure that you are familiar
with the problem set procedures in the document "Problem set
procedures for CS51".

You are allowed (and encouraged) to work with a partner on this
problem set. You are also allowed to work alone, if you prefer. See
https://cs51.io/procedures/pset-instructions/#working-with-a-partner
for further information on working with partners on problem sets.
======================================================================*)

(* This file contains the `PUZZLESOLVER` signature for modules that
solve particular puzzles, as well as a higher-order functor,
`MakePuzzleSolver`. A `PUZZLESOLVER` module solves the puzzle by
searching for a path from the initial state to the goal state.

The `MakePuzzleSolver` functor takes a `COLLECTION` functor and a
`PUZZLEDESCRIPTION` and returns a `PUZZLESOLVER`. The collection
specified by the functor is used to store the states that have been
reached so far. Thus, the ordering in which states are delivered by
the collection (using the `take` function) determines the order in
which the states are searched. A stack regime gives depth-first
search, a queue regime breadth-first search.

At the bottom of the file are definitions for depth-first search and
breadth-first search puzzle solvers. These are partially applied
versions of the `MakePuzzleSolver` functor that use certain
collections to engender different search methods.

This file makes use of the `Set` and `Collections` module, as well as the
`PuzzleDescription` module (which it opens). 
 *)

open Puzzledescription ;;
open Mazes;;

(* PUZZLESOLVER -- a module signature that provides for solving puzzles
   and graphically drawing the results *)

module type PUZZLESOLVER =
  sig
    (* CantReachGoal -- Exception raised by solver when no solution can
       be found *)
    exception CantReachGoal

    (* state -- The possible puzzle states *)
    type state
    (* move -- The possible moves *)
    type move

    (* solve () -- Returns a solution to the puzzle as a pair containing
       a list of moves and a list of states. The moves, when executed
       starting in the initial state, result in a goal state. A list
       of all of the states visited in the solution process in any order 
       is provided as the returned state list. *)
    val solve : unit -> move list * state list
    (* draw states moves -- Graphically renders a solution given by
       the sequence of `moves` that was discovered through visiting
       the `states` *)
    val draw : state list -> move list -> unit
    (* print_state state -- Prints a representation of `state` on the
       standard output *)
    val print_state : state -> unit
  end

(* MakePuzzleSolver -- a higher-order functor that generates puzzle
   solvers, with type

     (functor (sig type t end -> COLLECTION)) 
           -> PUZZLEDESCRIPTION 
           -> PUZZLESOLVER

   A functor that given a functor from an element type to a
   `COLLECTION`, as well as a `PUZZLEDESCRIPTION`, returns a full
   `PUZZLESOLVER` module.

   The functor `MakeCollection` is used for generating the collection
   for storing pending states that have yet to be searched. Using
   different collection regimes -- stacks (`MakeStackList`), queues
   (`MakeQueueList`, `MakeQueueStack`), etc. -- leads to different
   search regimes -- depth-first, breadth-first, etc.
 *)
(*module MakePuzzleSolver
         (MakeCollection
            : functor (Element : sig type t end) ->
                      (Collections.COLLECTION with type elt = Element.t))
         (Puzzle : PUZZLEDESCRIPTION)
       : (PUZZLESOLVER with type state = Puzzle.state
                        and type move = Puzzle.move) =
  struct
    failwith "MakePuzzleSolver not implemented"
  end ;;*)

 (* module MakePuzzleSolver
  (MakeCollection
     : functor (Element : sig type t end) ->
               (Collections.COLLECTION with type elt = Element.t))
  (Puzzle : PUZZLEDESCRIPTION)
: (PUZZLESOLVER with type state = Puzzle.state
                 and type move = Puzzle.move) =
struct
module Collection = MakeCollection (Puzzle)

exception CantReachGoal

type state = Puzzle.state
type move = Puzzle.move

let rec solve () =
let rec solve' visited_states current_states moves =
 if List.exists Puzzle.is_goal current_states then
   (moves, visited_states)
 else if Collection.is_empty current_states then
   raise CantReachGoal
 else
let print_state (state : state) =
Puzzle.draw_state state
end ;;*)
(*module RightWallSolver (Puzzle : PUZZLEDESCRIPTION) : PUZZLESOLVER with type state = Puzzle.state and type move = Puzzle.move =
   struct
     exception CantReachGoal
     
     type state = Puzzle.state
     type move = Puzzle.move
 
     (* Define the four cardinal directions *)
     type direction = Up | Down | Left | Right
 
     (* draw -- Graphically renders a solution given by the sequence of moves *)
     let draw _ _ = print_endline "Visualization not implemented for RightWallSolver"
 
     (* print_state -- Prints a representation of the state on the standard output *)
     let print_state state = Puzzle.print_state state
   end*)
   module RandomSolver (Puzzle : PUZZLEDESCRIPTION) : PUZZLESOLVER with type state = Puzzle.state and type move = Puzzle.move =
    struct
      exception CantReachGoal
    
      type state = Puzzle.state
      type move = Puzzle.move
      
      (* Define the dimensions and maze within the RandomSolver module *)
  
      (* Define the directions *)
      type direction = Up | Down | Left | Right
      
      (* let random_move (playerPos : state) : move =
        let move_to_fun (m : direction) : ((int * int) -> (int * int)) =
          match m with
          | Up -> (fun (i, j) -> i - 1, j)
          | Down -> (fun (i, j) -> i + 1, j)
          | Left -> (fun (i, j) -> i, j - 1)
          | Right -> (fun (i, j) -> i, j + 1)
        in
        let validate_pos (i, j) : bool = 
          let w, h = 15, 15 in
          i >= 0 && i < h && j >= 0 && j < w
        in
        let potential_moves =
          [Up; Down; Left; Right]
          |> List.filter (fun move ->
                          let new_pos = move_to_fun move playerPos in
                          validate_pos new_pos &&
                          match maze.(fst new_pos).(snd new_pos) with
                          | Wall -> false
                          | _ -> true)
        in
        List.nth potential_moves (Random.int (List.length potential_moves)) *)
  
      let rec solve () =
        (* let rec make_random_guess state path visited =
          if Puzzle.is_goal state || List.length visited > 1000 then
            List.rev path, List.rev visited
          else begin
            let rand_move = random_move state in
            let new_state = Puzzle.move state rand_move in
            make_random_guess new_state (rand_move :: path) (new_state :: visited)
          end
        in
        let initial_state = Puzzle.initial_state in
        make_random_guess initial_state [] [initial_state] *)
        raise CantReachGoal ;;
  
      let draw (states : state list) (moves : move list) : unit =
        let rec draw' states moves =
          match states, moves with
          | [], [] -> ()
          | state :: states', move :: moves' ->
              Puzzle.draw_state state;
              print_endline ("Move: " ^ (Puzzle.move_to_string move));
              draw' states' moves'
          | _, _ -> failwith "Invalid state or move list lengths"
        in
        draw' states moves
  
      let print_state state = Puzzle.print_state state
    end
            
  

(* DFSSolver and BFSSolver -- Higher-order functors that take in a
   PUZZLEDESCRIPTION, and will return puzzles that are solved with DFS and
   BFS, respectively. The fast bfs solver uses a better implementation
   of queues for speed. *)
module DFSSolver = MakePuzzleSolver (Collections.MakeStackList) ;;
module BFSSolver = MakePuzzleSolver (Collections.MakeQueueList) ;;
module FastBFSSolver = MakePuzzleSolver (Collections.MakeQueueStack) ;;
