(** Scoring and session tracking for the nested-bracket puzzle game.

    A [session] record captures all scoring state for one play-through.
    Every update function is pure -- it returns a new [session] without
    modifying the original, making sessions easy to test and compose. *)

(** A session record tracking all scoring state for one game. *)
type session = {
  score : int;  (** current cumulative score *)
  correct_count : int;  (** number of correct guesses so far *)
  wrong_count : int;  (** number of wrong guesses so far *)
  hint_count : int;  (** number of hints used so far *)
  streak : int;  (** current consecutive-correct streak *)
  max_streak : int;  (** highest streak reached this session *)
  pending_penalty : int;
      (** unpaid wrong/hint/skip penalty accumulated when the score floor (0)
          would have absorbed a deduction. The next correct guess pays this
          off before banking points, so penalties on a zero score still cost
          the player against future earnings. *)
  skip_count : int;
      (** number of puzzles the player skipped (surrendered) this session. *)
}

(** Variants of hint the player can request. Each kind has its own price; see
    [hint_cost]. [Reveal] solves the chip outright and is the most expensive. *)
type hint_kind =
  | First_letter
  | Length
  | Word_count
  | Reveal

(** A read-only end-of-game snapshot produced by [make_summary]. *)
type summary = {
  final_score : int;
  accuracy : float;
  grade : string;
  max_streak : int;
  hints_used : int;
}

(** Points deducted from the score per wrong guess. *)
val wrong_penalty : int

(** Points deducted from the score per [First_letter] hint used. Equal to
    [hint_cost First_letter]; kept as a separate value so existing callers
    that don't care about hint variety can ignore [hint_kind]. *)
val hint_penalty : int

(** Points deducted from the score per skipped puzzle. *)
val skip_penalty : int

(** [hint_cost kind] is the per-hint cost for [kind]. Cheaper hints reveal
    less; [Reveal] solves the chip outright and is the most expensive. *)
val hint_cost : hint_kind -> int

(** [make_session ()] returns a fresh session with all counters set to zero. *)
val make_session : unit -> session

(** [score_for_node difficulty depth] is the base point value for solving a
    node at tree [depth] in a puzzle of [difficulty]. Each depth level adds 5
    bonus points. Unrecognised difficulty strings default to "easy". *)
val score_for_node : string -> int -> int

(** [node_depth root target] returns [Some d] where [d] is the zero-indexed
    depth of [target] in the subtree rooted at [root] (root = 0), or [None]
    if [target] is not found. Uses physical equality (==). *)
val node_depth : Types.node -> Types.node -> int option

(** [total_attempts s] is [s.correct_count + s.wrong_count]. Hints are not
    counted as answer attempts. *)
val total_attempts : session -> int

(** [apply_correct s difficulty depth] records a correct answer. Awards
    [score_for_node difficulty depth] points plus a 15-point streak bonus
    whenever the updated streak is a nonzero multiple of 3. *)
val apply_correct : session -> string -> int -> session

(** [apply_correct_from_puzzle s p n] is [apply_correct] with depth computed
    automatically from puzzle [p]'s tree. Defaults to depth 0 if [n] is not
    found in the tree. *)
val apply_correct_from_puzzle : session -> Types.puzzle -> Types.node -> session

(** [apply_wrong s] records a wrong guess. Deducts [wrong_penalty] points
    (floored at 0) and resets the current streak to 0. [max_streak] unchanged. *)
val apply_wrong : session -> session

(** [apply_hint s] records a [First_letter] hint. Equivalent to
    [apply_hint_of_kind s First_letter]. The current streak is NOT reset by
    a hint. *)
val apply_hint : session -> session

(** [apply_hint_of_kind s kind] records a hint of the given [kind], deducting
    [hint_cost kind] (floored at 0; any unpaid amount is rolled into
    [pending_penalty]). The current streak is NOT reset. *)
val apply_hint_of_kind : session -> hint_kind -> session

(** [apply_skip s] records that the player skipped (surrendered) the current
    puzzle. Deducts [skip_penalty] (floored at 0; unpaid amount goes into
    [pending_penalty]), resets the current streak to 0, and increments
    [skip_count]. [max_streak] is unchanged. *)
val apply_skip : session -> session

(** [time_bonus elapsed_seconds] returns bonus points for fast completion:
    50 pts for under 60 s, 30 for under 120 s, 15 for under 180 s, 0 otherwise.
    Negative values return 0. *)
val time_bonus : int -> int

(** [apply_time_bonus s elapsed_seconds] adds [time_bonus elapsed_seconds] to
    the session score. Intended to be called once when the puzzle is won. *)
val apply_time_bonus : session -> int -> session

(** [accuracy s] is the fraction of guesses that were correct, in [0.0, 1.0].
    Returns [0.0] when no guesses have been made. *)
val accuracy : session -> float

(** [is_perfect s] is [true] iff no wrong guesses, no hints, and no skips
    have been recorded. A fresh session with zero guesses is considered
    perfect. *)
val is_perfect : session -> bool

(** [grade s] maps [s] to a letter grade. "S" for a perfect session; then
    "A" for accuracy >= 0.90, "B" for >= 0.75, "C" for >= 0.50, "D" below. *)
val grade : session -> string

(** [make_summary s] produces a [summary] snapshot of [s]. Call once the
    puzzle is won. *)
val make_summary : session -> summary
