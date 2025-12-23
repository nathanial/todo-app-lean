/-
  TodoApp.Actions.Todos - Todo CRUD actions
-/
import Loom
import Ledger
import TodoApp.Models
import TodoApp.Helpers
import TodoApp.Views.Todos

namespace TodoApp.Actions.Todos

open Loom
open Ledger
open TodoApp.Models
open TodoApp.Helpers

/-- List all todos for current user -/
def index : Action := fun ctx => do
  -- Get user ID from session
  match currentUserId ctx with
  | none => Action.redirect "/login" ctx
  | some userIdStr =>
    match userIdStr.toInt? with
    | none => Action.redirect "/login" ctx
    | some userId =>
      let todos := loadUserTodos ctx ⟨userId⟩
      let html := TodoApp.Views.Todos.renderIndex ctx todos
      Action.html html ctx

/-- Create a new todo -/
def create : Action := fun ctx => do
  let title := ctx.paramD "title" ""

  -- Validate input
  if title.isEmpty then
    let ctx := ctx.withFlash fun f => f.set "error" "Todo title is required"
    return ← Action.redirect "/todos" ctx

  -- Get user ID from session
  match currentUserId ctx with
  | none => Action.redirect "/login" ctx
  | some userIdStr =>
    match userIdStr.toInt? with
    | none => Action.redirect "/login" ctx
    | some userId =>
      match ctx.allocEntityId with
      | none =>
        let ctx := ctx.withFlash fun f => f.set "error" "Database not available"
        Action.redirect "/todos" ctx
      | some (todoId, ctx) =>
        let tx : Transaction := [
          .add todoId todoTitle (.string title),
          .add todoId todoCompleted (.bool false),
          .add todoId todoOwner (.ref ⟨userId⟩)
        ]
        match ← ctx.transact tx with
        | Except.ok ctx =>
          let ctx := ctx.withFlash fun f => f.set "success" "Todo added!"
          Action.redirect "/todos" ctx
        | Except.error e =>
          let ctx := ctx.withFlash fun f => f.set "error" s!"Failed to add todo: {e}"
          Action.redirect "/todos" ctx

/-- Toggle todo completion -/
def toggle : Action := fun ctx => do
  let todoIdStr := ctx.paramD "id" ""

  match todoIdStr.toInt? with
  | none =>
    let ctx := ctx.withFlash fun f => f.set "error" "Invalid todo"
    Action.redirect "/todos" ctx
  | some todoIdInt =>
    let todoId : EntityId := ⟨todoIdInt⟩

    -- Verify ownership
    match currentUserId ctx with
    | none => Action.redirect "/login" ctx
    | some userIdStr =>
      match userIdStr.toInt? with
      | none => Action.redirect "/login" ctx
      | some userId =>
        match ctx.database with
        | none =>
          let ctx := ctx.withFlash fun f => f.set "error" "Database not available"
          Action.redirect "/todos" ctx
        | some db =>
          -- Check owner
          match db.getOne todoId todoOwner with
          | some (.ref ownerId) =>
            if ownerId.id != userId then
              let ctx := ctx.withFlash fun f => f.set "error" "Not authorized"
              Action.redirect "/todos" ctx
            else
              -- Get current completion status
              let currentCompleted := match db.getOne todoId todoCompleted with
                | some (.bool b) => b
                | _ => false
              let newCompleted := !currentCompleted

              -- Update
              let tx : Transaction := [
                .retract todoId todoCompleted (.bool currentCompleted),
                .add todoId todoCompleted (.bool newCompleted)
              ]
              match ← ctx.transact tx with
              | Except.ok ctx =>
                let msg := if newCompleted then "Todo completed!" else "Todo marked as active"
                let ctx := ctx.withFlash fun f => f.set "success" msg
                Action.redirect "/todos" ctx
              | Except.error e =>
                let ctx := ctx.withFlash fun f => f.set "error" s!"Failed to update todo: {e}"
                Action.redirect "/todos" ctx
          | _ =>
            let ctx := ctx.withFlash fun f => f.set "error" "Todo not found"
            Action.redirect "/todos" ctx

/-- Delete a todo -/
def delete : Action := fun ctx => do
  let todoIdStr := ctx.paramD "id" ""

  match todoIdStr.toInt? with
  | none =>
    let ctx := ctx.withFlash fun f => f.set "error" "Invalid todo"
    Action.redirect "/todos" ctx
  | some todoIdInt =>
    let todoId : EntityId := ⟨todoIdInt⟩

    -- Verify ownership
    match currentUserId ctx with
    | none => Action.redirect "/login" ctx
    | some userIdStr =>
      match userIdStr.toInt? with
      | none => Action.redirect "/login" ctx
      | some userId =>
        match ctx.database with
        | none =>
          let ctx := ctx.withFlash fun f => f.set "error" "Database not available"
          Action.redirect "/todos" ctx
        | some db =>
          -- Check owner
          match db.getOne todoId todoOwner with
          | some (.ref ownerId) =>
            if ownerId.id != userId then
              let ctx := ctx.withFlash fun f => f.set "error" "Not authorized"
              Action.redirect "/todos" ctx
            else
              -- Get all current values to retract
              let titleVal := db.getOne todoId todoTitle
              let completedVal := db.getOne todoId todoCompleted
              let ownerVal := db.getOne todoId todoOwner

              -- Build retraction transaction
              let mut txOps : List TxOp := []
              if let some v := titleVal then
                txOps := .retract todoId todoTitle v :: txOps
              if let some v := completedVal then
                txOps := .retract todoId todoCompleted v :: txOps
              if let some v := ownerVal then
                txOps := .retract todoId todoOwner v :: txOps

              match ← ctx.transact txOps with
              | Except.ok ctx =>
                let ctx := ctx.withFlash fun f => f.set "success" "Todo deleted"
                Action.redirect "/todos" ctx
              | Except.error e =>
                let ctx := ctx.withFlash fun f => f.set "error" s!"Failed to delete todo: {e}"
                Action.redirect "/todos" ctx
          | _ =>
            let ctx := ctx.withFlash fun f => f.set "error" "Todo not found"
            Action.redirect "/todos" ctx

end TodoApp.Actions.Todos
