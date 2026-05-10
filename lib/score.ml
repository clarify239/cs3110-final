open Types

type session = {
  score : int;
  correct_count : int;
  wrong_count : int;
  hint_count : int;
  streak : int;
  max_streak : int;
  pending_penalty : int;
}

type summary = {
  final_score : int;
  accuracy : float;
  grade : string;
  max_streak : int;
  hints_used : int;
}

let wrong_penalty = 5
let hint_penalty = 3

let make_session () : session =
  {
    score = 0;
    correct_count = 0;
    wrong_count = 0;
    hint_count = 0;
    streak = 0;
    max_streak = 0;
    pending_penalty = 0;
  }

let base_points (difficulty : string) : int =
  match difficulty with
  | "easy" -> 10
  | "medium" -> 20
  | "hard" -> 35
  | _ -> 10

let streak_bonus (streak : int) : int =
  if streak > 0 && streak mod 3 = 0 then 15 else 0

let score_for_node (difficulty : string) (depth : int) : int =
  base_points difficulty + (depth * 5)

let rec node_depth (root : node) (target : node) : int option =
  if root == target then Some 0
  else
    List.fold_left
      (fun acc child ->
        match acc with
        | Some _ -> acc
        | None -> Option.map (fun d -> d + 1) (node_depth child target))
      None root.children

let total_attempts (s : session) : int = s.correct_count + s.wrong_count

let apply_correct (s : session) (difficulty : string) (depth : int) : session =
  let pts = score_for_node difficulty depth in
  let new_streak = s.streak + 1 in
  let bonus = streak_bonus new_streak in
  let gross = pts + bonus in
  (* Pay any pending penalty (from earlier wrongs/hints that hit the 0-floor)
     before banking the remaining points. *)
  let pay = min gross s.pending_penalty in
  let net = gross - pay in
  {
    score = s.score + net;
    correct_count = s.correct_count + 1;
    wrong_count = s.wrong_count;
    hint_count = s.hint_count;
    streak = new_streak;
    max_streak = max s.max_streak new_streak;
    pending_penalty = s.pending_penalty - pay;
  }

let apply_correct_from_puzzle (s : session) (p : puzzle) (n : node) : session =
  let depth = Option.value ~default:0 (node_depth p.root n) in
  apply_correct s p.difficulty depth

let apply_wrong (s : session) : session =
  let pay = min s.score wrong_penalty in
  {
    score = s.score - pay;
    correct_count = s.correct_count;
    wrong_count = s.wrong_count + 1;
    hint_count = s.hint_count;
    streak = 0;
    max_streak = s.max_streak;
    pending_penalty = s.pending_penalty + (wrong_penalty - pay);
  }

let apply_hint (s : session) : session =
  let pay = min s.score hint_penalty in
  {
    score = s.score - pay;
    correct_count = s.correct_count;
    wrong_count = s.wrong_count;
    hint_count = s.hint_count + 1;
    streak = s.streak;
    max_streak = s.max_streak;
    pending_penalty = s.pending_penalty + (hint_penalty - pay);
  }

let time_bonus (elapsed_seconds : int) : int =
  if elapsed_seconds < 0 then 0
  else if elapsed_seconds < 60 then 50
  else if elapsed_seconds < 120 then 30
  else if elapsed_seconds < 180 then 15
  else 0

let apply_time_bonus (s : session) (elapsed_seconds : int) : session =
  { s with score = s.score + time_bonus elapsed_seconds }

let accuracy (s : session) : float =
  let total = total_attempts s in
  if total = 0 then 0.0
  else float_of_int s.correct_count /. float_of_int total

let is_perfect (s : session) : bool =
  s.wrong_count = 0 && s.hint_count = 0

let grade (s : session) : string =
  if is_perfect s then "S"
  else
    let acc = accuracy s in
    if acc >= 0.9 then "A"
    else if acc >= 0.75 then "B"
    else if acc >= 0.5 then "C"
    else "D"

let make_summary (s : session) : summary =
  {
    final_score = s.score;
    accuracy = accuracy s;
    grade = grade s;
    max_streak = s.max_streak;
    hints_used = s.hint_count;
  }
