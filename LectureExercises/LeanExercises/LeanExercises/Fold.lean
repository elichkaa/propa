-- from Literature/fold exercises 1.pdf

import Testing

def all {α} (p: α -> Bool) (lst: List α): Bool :=
  lst.foldr (fun hd acc => p hd && acc) true

def any {α} (p: α -> Bool) (lst: List α): Bool :=
  lst.foldr (fun hd acc => p hd || acc) false

#assert (all (. > 0) [1, 2, 3]) == true
#assert (all (. > 0) [-1, -2, -3]) == false
#assert (any (. > 0) [-1, -2, -3]) == false
#assert (any (. > 0) [1, -2, -3]) == true

def elem {α} [BEq α] (z: α) (lst: List α): Bool :=
  lst.foldr (fun hd acc => hd == z || acc) false

#assert (elem 1 [1, 2, 3]) == true
#assert (elem 0 [1, 2, 3]) == false

def concat {α} (lst1: List α) (lst2: List α): List α :=
  lst1.foldr (fun hd1 acc => hd1 :: acc) lst2

def concat_left {α} (lst1: List α) (lst2: List α): List α :=
  (lst1.foldl (fun acc hd => hd :: acc) []).reverse ++ lst2

#assert (concat [1, 2] [1, 2, 3]) == [1, 2, 1, 2, 3]
#assert (concat_left [1, 2] [1, 2, 3]) == [1, 2, 1, 2, 3]

def filter {α} (p: α -> Bool) (lst: List α): List α :=
  lst.foldr (fun hd acc => if p hd then hd :: acc else acc) []

#assert (filter (. > 0) [1, 2, 3]) == [1, 2, 3]
#assert (filter (. > 0) [-1, -2, -3]) == []

def takeWhile {α} (p: α -> Bool) (lst: List α): List α :=
  lst.foldr (fun hd acc => if p hd then hd :: acc else []) []

#assert (takeWhile (fun x => x > 5) [1, 2, 3]) == []
#assert (takeWhile (fun x => x > 1) [1, 2, 3]) == []

def id2 {α} [BEq α] (lst: List α): List α :=
  lst.foldr (fun hd acc => hd :: acc) []

#assert (id2 [1, 2, 3]) == [1, 2, 3]
#assert (id2 [1]) == [1]
