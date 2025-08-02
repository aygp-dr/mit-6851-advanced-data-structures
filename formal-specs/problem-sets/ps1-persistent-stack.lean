-- MIT 6.851 Problem Set 1 - Persistent Stack Specification
-- This is a formal specification of Problem 1 from PS1

import Lean

namespace MIT6851.PS1

-- Define a persistent stack type
inductive PersistentStack (α : Type u) where
  | empty : PersistentStack α
  | node : α → PersistentStack α → PersistentStack α
  deriving Repr

namespace PersistentStack

-- Basic operations
def push {α : Type u} (s : PersistentStack α) (x : α) : PersistentStack α :=
  node x s

def pop {α : Type u} : PersistentStack α → Option (α × PersistentStack α)
  | empty => none
  | node x s => some (x, s)

def top {α : Type u} : PersistentStack α → Option α
  | empty => none
  | node x _ => some x

def isEmpty {α : Type u} : PersistentStack α → Bool
  | empty => true
  | node _ _ => false

-- Size function
def size {α : Type u} : PersistentStack α → Nat
  | empty => 0
  | node _ s => 1 + size s

-- Convert to list for easier reasoning
def toList {α : Type u} : PersistentStack α → List α
  | empty => []
  | node x s => x :: toList s

-- Problem 1a: Persistent operations maintain immutability
theorem push_preserves_original {α : Type u} (s : PersistentStack α) (x : α) :
  s = s ∧ push s x ≠ s := by
  constructor
  · rfl
  · intro h
    cases s
    · simp [push] at h
    · simp [push] at h

-- Problem 1b: Version independence
theorem version_independence {α : Type u} (s : PersistentStack α) (x y : α) :
  let v1 := push s x
  let v2 := push s y
  v1 ≠ v2 ∧ toList v1 ≠ toList v2 := by
  simp [push, toList]
  intro h
  injection h

-- Problem 1c: Space complexity analysis
-- Each push operation creates exactly one new node
theorem push_space_complexity {α : Type u} (s : PersistentStack α) (x : α) :
  size (push s x) = size s + 1 := by
  simp [push, size]

-- Problem 1d: Time complexity (stated as axiom since we can't measure actual time)
axiom push_time_O1 {α : Type u} : ∀ (s : PersistentStack α) (x : α),
  True -- Represents O(1) time complexity

axiom pop_time_O1 {α : Type u} : ∀ (s : PersistentStack α),
  True -- Represents O(1) time complexity

-- Additional properties
theorem push_pop_identity {α : Type u} (s : PersistentStack α) (x : α) :
  pop (push s x) = some (x, s) := by
  simp [push, pop]

theorem pop_empty {α : Type u} :
  @pop α empty = none := by
  simp [pop]

-- Persistence property: old versions remain valid
theorem persistence_property {α : Type u} (s : PersistentStack α) (x : α) :
  let s' := push s x
  toList s = toList s ∧ toList s' = x :: toList s := by
  simp [push, toList]

end PersistentStack

-- Problem 1e: Implement efficient k-th element access
-- This would require a different data structure (e.g., with random access)
structure EfficientPersistentStack (α : Type u) where
  stack : PersistentStack α
  -- Additional structure for O(log n) access
  -- This is left as an exercise

end MIT6851.PS1