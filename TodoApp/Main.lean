/-
  TodoApp.Main - Application setup and entry point
-/
import Loom
import TodoApp.Helpers
import TodoApp.Actions.Home
import TodoApp.Actions.Auth
import TodoApp.Actions.Todos

namespace TodoApp

open Loom
open TodoApp.Helpers

/-- Application configuration -/
def config : AppConfig := {
  secretKey := "todo-app-secret-key-min-32-chars!!".toUTF8
  sessionCookieName := "todo_session"
  csrfFieldName := "_csrf"
  csrfEnabled := true
}

/-- Path to the JSONL journal file for persistence -/
def journalPath : System.FilePath := "data/todos.jsonl"

/-- Build the application with all routes using persistent database -/
def buildApp : App :=
  Loom.app config
    -- Middleware
    |>.use Middleware.logging
    |>.use Middleware.securityHeaders
    -- Public routes
    |>.get "/" "home" Actions.Home.index
    |>.get "/login" "login_form" Actions.Auth.loginForm
    |>.post "/login" "login" Actions.Auth.login
    |>.get "/register" "register_form" Actions.Auth.registerForm
    |>.post "/register" "register" Actions.Auth.register
    |>.get "/logout" "logout" Actions.Auth.logout
    -- Protected routes (auth check happens in actions)
    |>.get "/todos" "todos_index" Actions.Todos.index
    |>.post "/todos" "todos_create" Actions.Todos.create
    |>.post "/todos/:id/toggle" "todos_toggle" Actions.Todos.toggle
    |>.post "/todos/:id/delete" "todos_delete" Actions.Todos.delete
    -- Persistent database (auto-persists to JSONL)
    |>.withPersistentDatabase journalPath

/-- Main entry point (inside namespace) -/
def runApp : IO Unit := do
  -- Ensure data directory exists
  IO.FS.createDirAll "data"
  IO.println "Starting Todo App..."
  IO.println s!"Database: Persistent (journal at {journalPath})"
  let app := buildApp
  app.run "0.0.0.0" 3000

end TodoApp

/-- Top-level main entry point for executable -/
def main : IO Unit := TodoApp.runApp
