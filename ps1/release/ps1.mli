(** This module contains the Rock, Paper, Scissors exercises for CS
    3110 Problem Set 1, Spring 2014. The following functions outline
    the fundamental game operations for the Rock, Paper, Scissors
    game. *)



(** [rps_round a b] accepts two symbols, [a] and [b], and determines the winner.
    "rock" beats "scissors", "scissors" beats "paper", and "paper" beats "rock".

    @param [a] [b] One of the choices ["rock"], ["paper"] or ["scissors"]

    @return One of -1, 0, 1, 4 . -1 means [a] won, 1 means [b] won,
            0 indicates a tie.  4 indicates an invalid input. *)
val rps_round : string -> string -> int

(** Values of type [move] are moves that players can make in a
    rock-paper-scissors game. *)
type move = Rock | Paper | Scissors

(** The [result] type is used to specify the outcome of an individual round of
    the game. *)
type result = AWin | BWin | Draw

(** The function [rps_round_enum] adheres to the specification of [rps_round]
    but using the [move] and [result] types rather than [string] and [int]. *)
val rps_round_enum : move -> move -> result

(** An alternate implementation of the [rps_round_enum] specification *)
val rps_round_nested_match : move -> move -> result

(** An alternate implementation of the [rps_round_enum] specification *)
val rps_round_single_match : move -> move -> result

(** An alternate implementation of the [rps_round_enum] specification *)
val rps_round_with_helper : move -> move -> result



(** [all_pairs xs] returns the cartesian product of the list [xs] with itself.
    More precisely, for each [x] and [y], [(x,y)] is in [all_pairs xs] if and
    only if [x] and [y] are in [xs]. *)
val all_pairs : 'a list -> ('a * 'a) list

(** The [test_rps_eq] function takes two variant implementations of [rps_round]
    and returns true if they agree on all possible inputs.  *)
val test_rps_eq :
    (move -> move -> result) -> (move -> move -> result) -> bool

(** [test_all_rps fs] returns [true] if [List.mem f fs] and [List.mem g fs]
    implies that for all pairs [move1, move2] of type [move]
    {[f move1 move2 = g move1 move2]} *)
val test_all_rps : (move -> move -> result) list -> bool



(** A [history] is a list of moves that a player's opponent has made in a game
    of rock-paper-scissors.  The most recent move is stored at the head. *)
type history = move list

(** A [player] is a function that produces a [move] given a [history] of
    the opponent's moves. *)
type player = history -> move

(** The [always_rock] player always returns [Rock]. *)
val always_rock : player

(** The player represented by [beats_last] always plays the move that
    beats the previous move made by the opponent. *)
val beats_last : player

(** [always_plays x] returns a player that always plays the move [x] *)
val always_plays : move -> player

(** [rps_game a b] repeatedly runs rock-paper-scissors rounds until either [a]
    or [b] wins.

    @return [true] if [a] wins, [false] otherwise *)
val rps_game : player -> player -> bool



(** [pair_filter compare xs] breaks [xs] into adjacent pairs and applies
    [compare] to the components of each pair.  Whichever value is returned by
    [compare] is included in the output list.  If the input list [xs] has
    odd length then the last element will also be included in the output. *)
val pair_filter : ('a -> 'a -> 'a) -> 'a list -> 'a list

(** [tournament compare l] repeatedly applies [pair_filter compare] to [l]
    until only a single element remains. If [x] is the remaining element then
    [tournament] returns [Some x]. If [l] is empty then [tournament] returns
    [None]. *)
val tournament : ('a -> 'a -> 'a) -> 'a list -> 'a option
