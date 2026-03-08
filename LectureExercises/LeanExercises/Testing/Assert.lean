-- Modified version of https://github.com/pnwamk/lean4-assert-command
import Lean.Elab.Command

open Lean
open Meta
open Lean.Elab
open Lean.Elab.Term
open Lean.Elab.Command

private def addAndCompile (value : Expr) (name : Name) : TermElabM Unit := do
  let type ← inferType value
  let decl := Declaration.defnDecl {
    name := name, levelParams := [], type := type,
    value := value, hints := ReducibilityHints.opaque,
    safety := DefinitionSafety.unsafe
  }
  Term.ensureNoUnassignedMVars decl
  Lean.addAndCompile decl

declare_syntax_cat comparator
syntax " == " : comparator

syntax (name := assert) "#assert " term:max " == " term : command

private def beqAndRepr {α} [BEq α] [Repr α] (actual expected : α) : (Bool × String × String) :=
  if actual == expected
  then (true, reprStr actual, reprStr expected)
  else (false, reprStr actual, reprStr expected)

private unsafe def elabAssertAux (tk actual expected : Syntax) : CommandElabM Unit := do
  let elabComp (actual expected : Syntax) (rNm : Name) : CommandElabM (Bool × String × String) := runTermElabM fun _ => do
    let env ← getEnv

    -- 1. Elaborate the LHS (actual) first, with no expected type
    let a ← Term.elabTerm actual none

    -- 2. Synthesize its metavariables and infer its type
    Term.synthesizeSyntheticMVarsNoPostponing
    let lhsType ← inferType a

    -- 3. Elaborate the RHS (expected), passing the LHS type as the *expected* type
    let e ← Term.elabTerm expected (some lhsType)

    -- 4. Synthesize any remaining metavariables (e.g., from the RHS)
    Term.synthesizeSyntheticMVarsNoPostponing

    -- 5. This check is still good practice to ensure full unification
    let e ← withRef expected (do ensureHasType (some lhsType) e)

    -- TODO use trySynthInstance for BEq and Repr constraint for lhsType so beqAndRepr does not appear in error messages
    let r ← mkAppM ``beqAndRepr #[a, e]
    try addAndCompile r rNm; evalConst (Bool × String × String) rNm finally setEnv env

  let (res, aStr, eStr) ← elabComp actual expected `_assertion
  if res
  then logInfoAt tk ("✅ " ++ aStr ++ " == " ++ eStr : String)
  else do
    logErrorAt tk ("❌ " ++ aStr ++ " ≠ " ++ eStr : String)

@[command_elab assert]
unsafe def elabAssert : CommandElab
  | `(#assert%$tk $actual:term == $expected:term) => elabAssertAux tk actual expected
  | _ => throwUnsupportedSyntax
