/-
  TodoApp.Actions.Auth - Authentication actions (login, register, logout)
-/
import Loom
import Ledger
import TodoApp.Models
import TodoApp.Helpers
import TodoApp.Views.Auth

namespace TodoApp.Actions.Auth

open Loom
open Ledger
open TodoApp.Models
open TodoApp.Helpers

/-- Show login form -/
def loginForm : Action := fun ctx => do
  -- Redirect if already logged in
  if isLoggedIn ctx then
    Action.redirect "/todos" ctx
  else
    let html := TodoApp.Views.Auth.renderLogin ctx
    Action.html html ctx

/-- Process login -/
def login : Action := fun ctx => do
  let email := ctx.paramD "email" ""
  let password := ctx.paramD "password" ""

  -- Validate input
  if email.isEmpty || password.isEmpty then
    let ctx := ctx.withFlash fun f => f.set "error" "Email and password are required"
    return ← Action.redirect "/login" ctx

  -- Find user by email
  match findUserByEmail ctx email with
  | none =>
    let ctx := ctx.withFlash fun f => f.set "error" "Invalid email or password"
    Action.redirect "/login" ctx
  | some userId =>
    -- Check password
    let storedHash := getAttrString ctx userId userPasswordHash
    let inputHash := hashPassword password ctx.config.secretKey
    match storedHash with
    | some hash =>
      if hash == inputHash then
        -- Get user name
        let userName := (getAttrString ctx userId userName).getD "User"
        -- Set session
        let ctx := ctx.withSession fun s =>
          s.set "user_id" (toString userId.id)
           |>.set "user_name" userName
        let ctx := ctx.withFlash fun f => f.set "success" s!"Welcome back, {userName}!"
        Action.redirect "/todos" ctx
      else
        let ctx := ctx.withFlash fun f => f.set "error" "Invalid email or password"
        Action.redirect "/login" ctx
    | none =>
      let ctx := ctx.withFlash fun f => f.set "error" "Invalid email or password"
      Action.redirect "/login" ctx

/-- Show register form -/
def registerForm : Action := fun ctx => do
  -- Redirect if already logged in
  if isLoggedIn ctx then
    Action.redirect "/todos" ctx
  else
    let html := TodoApp.Views.Auth.renderRegister ctx
    Action.html html ctx

/-- Process registration -/
def register : Action := fun ctx => do
  let name := ctx.paramD "name" ""
  let email := ctx.paramD "email" ""
  let password := ctx.paramD "password" ""

  -- Validate input
  if name.isEmpty || email.isEmpty || password.isEmpty then
    let ctx := ctx.withFlash fun f => f.set "error" "All fields are required"
    return ← Action.redirect "/register" ctx

  -- Check if email already exists
  match findUserByEmail ctx email with
  | some _ =>
    let ctx := ctx.withFlash fun f => f.set "error" "Email already registered"
    Action.redirect "/register" ctx
  | none =>
    -- Create user
    match ctx.allocEntityId with
    | none =>
      let ctx := ctx.withFlash fun f => f.set "error" "Database not available"
      Action.redirect "/register" ctx
    | some (userId, ctx) =>
      let passwordHash := hashPassword password ctx.config.secretKey
      let tx : Transaction := [
        .add userId userName (.string name),
        .add userId userEmail (.string email),
        .add userId userPasswordHash (.string passwordHash)
      ]
      match ← ctx.transact tx with
      | Except.ok ctx =>
        -- Log in the new user
        let ctx := ctx.withSession fun s =>
          s.set "user_id" (toString userId.id)
           |>.set "user_name" name
        let ctx := ctx.withFlash fun f => f.set "success" s!"Welcome, {name}! Your account has been created."
        Action.redirect "/todos" ctx
      | Except.error e =>
        let ctx := ctx.withFlash fun f => f.set "error" s!"Failed to create account: {e}"
        Action.redirect "/register" ctx

/-- Process logout -/
def logout : Action := fun ctx => do
  let ctx := ctx.withSession fun s => s.clear
  let ctx := ctx.withFlash fun f => f.set "info" "You have been logged out"
  Action.redirect "/" ctx

end TodoApp.Actions.Auth
