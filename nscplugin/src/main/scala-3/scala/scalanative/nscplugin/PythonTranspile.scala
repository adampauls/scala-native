package scala.meta.tests.parsers

import scala.meta._
import pythonparse.Ast

object PythonTranspile {

        def compile(tree: Tree): Ast = tree match {
        case Defn.Val(mods, pats, decltpe, rhs) =>
        require(mods.isEmpty, "mods unsupported")
        require(pats.forall(_.isInstanceOf[Pat.Var]))
        Ast.stmt.Assign(pats.map { case Pat.Var(Term.Name(ident)) => Ast.expr.Name(Ast.identifier(ident), Ast.expr_context.Load) }, compile(rhs))
        case Defn.Var(mods, pats, decltpe, maybeRhs) =>
        val idents = pats.map { case Pat.Var(Term.Name(ident)) => Ast.expr.Name(Ast.identifier(ident), Ast.expr_context.Load) }
        maybeRhs.map { rhs =>
        require(mods.isEmpty, "mods unsupported")
        require(pats.forall(_.isInstanceOf[Pat.Var]))
        Ast.stmt.Assign(idents, compile(rhs))
        }.getOrElse(Ast.expr.Tuple(idents, Ast.expr_context.Store))

        }

        def compile(term: Term): Ast.expr = term match {
        case Lit.Int(i) => Ast.expr.Num(i)
        }

        }
