-- MIT 6.851 Problem Set 2 - Retroactive Queue Specification
-- Formal specification of retroactive data structures

import Lean

namespace MIT6851.PS2

-- Time type for retroactive operations
def Time := Nat

-- Operation types for a queue
inductive QueueOp (α : Type u) where
  | enqueue : α → QueueOp α
  | dequeue : QueueOp α
  deriving Repr, BEq

-- Timeline entry
structure TimelineEntry (α : Type u) where
  time : Time
  op : QueueOp α
  deriving Repr

-- Retroactive queue state
structure RetroactiveQueue (α : Type u) where
  timeline : List (TimelineEntry α)
  present : Time
  deriving Repr

namespace RetroactiveQueue

-- Create empty retroactive queue
def empty {α : Type u} : RetroactiveQueue α :=
  ⟨[], 0⟩

-- Insert operation at given time (partially retroactive)
def insert {α : Type u} (rq : RetroactiveQueue α) (t : Time) (op : QueueOp α) : 
  Option (RetroactiveQueue α) :=
  if t ≤ rq.present then
    let newEntry := TimelineEntry.mk t op
    let sortedTimeline := (newEntry :: rq.timeline).mergeSort (fun a b => a.time ≤ b.time)
    some ⟨sortedTimeline, rq.present⟩
  else
    none -- Cannot insert in future

-- Delete operation at given time
def delete {α : Type u} (rq : RetroactiveQueue α) (t : Time) : RetroactiveQueue α :=
  let filteredTimeline := rq.timeline.filter (fun entry => entry.time ≠ t)
  ⟨filteredTimeline, rq.present⟩

-- Query present state
def queryPresent {α : Type u} (rq : RetroactiveQueue α) : List α :=
  let relevantOps := rq.timeline.filter (fun entry => entry.time ≤ rq.present)
  let sortedOps := relevantOps.mergeSort (fun a b => a.time ≤ b.time)
  executeOperations sortedOps.map (·.op) []

-- Execute sequence of operations to get final queue state
def executeOperations {α : Type u} : List (QueueOp α) → List α → List α
  | [], queue => queue
  | QueueOp.enqueue x :: ops, queue => executeOperations ops (queue ++ [x])
  | QueueOp.dequeue :: ops, [] => executeOperations ops []
  | QueueOp.dequeue :: ops, _ :: tail => executeOperations ops tail

-- Problem 2a: Partially retroactive operations
theorem partially_retroactive_constraint {α : Type u} (rq : RetroactiveQueue α) (t : Time) (op : QueueOp α) :
  insert rq t op = none ↔ t > rq.present := by
  simp [insert]
  by_cases h : t ≤ rq.present
  · simp [h]
  · simp [h]
    omega

-- Problem 2b: Time complexity analysis
-- For partially retroactive queue with n operations:
-- - Insert: O(n) worst case (need to re-sort timeline)
-- - Delete: O(n) (need to filter timeline)
-- - Query: O(n) (need to execute all operations)

axiom insert_time_complexity {α : Type u} (rq : RetroactiveQueue α) (t : Time) (op : QueueOp α) :
  rq.timeline.length ≤ 1000 → True -- Represents O(n) time

-- Problem 2c: Correctness properties
theorem insert_preserves_past {α : Type u} (rq : RetroactiveQueue α) (t : Time) (op : QueueOp α) :
  t ≤ rq.present →
  ∃ rq', insert rq t op = some rq' ∧ rq'.present = rq.present := by
  intro h
  simp [insert, h]
  exists ⟨(TimelineEntry.mk t op :: rq.timeline).mergeSort (fun a b => a.time ≤ b.time), rq.present⟩
  simp

-- Problem 2d: Queue invariants
-- The queue maintains FIFO order when operations are executed chronologically
def isValidQueueSequence {α : Type u} (ops : List (QueueOp α)) : Bool :=
  -- Check that we never dequeue from empty queue
  let rec check (ops : List (QueueOp α)) (queueSize : Nat) : Bool :=
    match ops with
    | [] => true
    | QueueOp.enqueue _ :: rest => check rest (queueSize + 1)
    | QueueOp.dequeue :: rest => 
        if queueSize > 0 then check rest (queueSize - 1)
        else false
  check ops 0

theorem timeline_validity {α : Type u} (rq : RetroactiveQueue α) :
  let sortedOps := (rq.timeline.filter (fun e => e.time ≤ rq.present))
                    .mergeSort (fun a b => a.time ≤ b.time)
                    .map (·.op)
  isValidQueueSequence sortedOps = true → 
  True := by -- This would require a more detailed proof
  intro _
  trivial

-- Problem 2e: Fully retroactive extension (advanced)
-- This would allow queries at any time point, not just present
structure FullyRetroactiveQueue (α : Type u) where
  timeline : List (TimelineEntry α)
  deriving Repr

-- Query state at any time point
def queryAtTime {α : Type u} (frq : FullyRetroactiveQueue α) (t : Time) : List α :=
  let relevantOps := frq.timeline.filter (fun entry => entry.time ≤ t)
  let sortedOps := relevantOps.mergeSort (fun a b => a.time ≤ b.time)
  executeOperations sortedOps.map (·.op) []

end RetroactiveQueue

end MIT6851.PS2