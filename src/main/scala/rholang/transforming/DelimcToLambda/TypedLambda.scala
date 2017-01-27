package rholang.transforming.DelimcToLambda

import rholang.parsing.lambda.{Absyn => Untyped}


/**
  * Created by weeeeeew on 15/06/16.
  */
object TypedLambda {
  //Type mappings
  def typeName[Integer] : String           = "Integer"
  def typeName[Char]    : String           = "Char"
  def typeName[String]  : java.lang.String = "String"


  //Typed typed expressions
//  sealed trait TypedExpr[A] extends Untyped.TypedExpr

  case class ETyped[A](expr: Expr[A], typ: Type[A])
    extends Untyped.ETyped(expr, typ) // with TypedExpr[A]


  //Typed expressions
  sealed trait Expr[A] extends Untyped.Expr

  case class EAbs[A,B](varName: String, varType: Type[A], expr: ETyped[B])
    extends Untyped.EAbs(varName, varType, expr) with Expr[A => B]

  case class EApp[A,B,F](expr1: ETyped[F], expr2: ETyped[A])
    (implicit p: F =:= (A => B))
    extends Untyped.EApp(expr1, expr2) with Expr[B]

  case class EVar[A](name: String)
    extends Untyped.EVar(name) with Expr[A]

  case class EVal[A](value: Value[A])
    extends Untyped.EVal(value) with Expr[A]

  case class ETuple[A](tuple: Tuple[A])
    extends Untyped.ETuple(tuple) with Expr[A]

  case class EFirst2[A](expr: ETyped[(A,_)])
    extends Untyped.EFirst(expr) with Expr[A]

  case class EFirst3[A](expr: ETyped[(A,_,_)])
    extends Untyped.EFirst(expr) with Expr[A]

  case class ESecond2[A,B](expr: ETyped[A])
    (implicit p: A =:= (_,B))
    extends Untyped.ESecond(expr) with Expr[B]

  case class ESecond3[A](expr: ETyped[(_,A,_)])
    extends Untyped.ESecond(expr) with Expr[A]

  case class EThird3[A](expr: ETyped[(_,_,A)])
    extends Untyped.EThird(expr) with Expr[A]

  //Typed tuples
  sealed trait Tuple[+A] extends Untyped.Tuple

  case class Tuple2[A,B](exprA: ETyped[A], exprB: ETyped[B])
    extends Untyped.Tuple2(exprA, exprB) with Tuple[(A,B)]

  case class Tuple3[A,B,C](exprA: ETyped[A], exprB: ETyped[B], exprC: ETyped[C])
    extends Untyped.Tuple3(exprA, exprB, exprC) with Tuple[(A,B,C)]


  //Typed values
  sealed trait Value[A] extends Untyped.Value

  case class VInt(integer: Integer)
    extends Untyped.VInt(integer) with Value[Integer]

  case class VString(string: String)
    extends Untyped.VString(string) with Value[String]

  //Typed types
  sealed trait Type[+A] extends Untyped.Type

  case class TSimple[A]()
    extends Untyped.TSimple(typeName[A]) with Type[A]

  case class TTuple[A](tType: TType[A])
    extends Untyped.TTuple(tType) with Type[A]

  case class TFun[A,B](tFrom: Type[A], tTo: Type[B])
    extends Untyped.TFun(tFrom, tTo) with Type[A => B]


  //Typed tuple types
  sealed trait TType[A] extends Untyped.TType

  case class TType2[A,B](tA: Type[A], tB: Type[B])
    extends Untyped.TType2(tA, tB) with TType[(A,B)]

  case class TType3[A,B,C](tA: Type[A], tB: Type[B], tC: Type[C])
    extends Untyped.TType3(tA, tB, tC) with TType[(A,B,C)]


  def typeCheck(expr: Untyped.TypedExpr): ETyped[A] forSome {type A} = expr match {
    case e: Untyped.ETyped => ETyped(typeCheck(e.expr_), extractType(e.type_))
  }

  def typeCheck(expr: Untyped.Expr): Expr[A] forSome {type A} = expr match {
    case e: Untyped.EVar         => EVar(e.var_)
    case e: Untyped.EVal         => EVal(inferType(e.value_))
    case e: Untyped.EAbs         => EAbs(e.var_, extractType(e.type_), typeCheck(e.typedexpr_))
    case e: Untyped.EApp         => EApp(typeCheck(e.typedexpr_1), typeCheck(e.typedexpr_2))
    case e: Untyped.ETuple       => ETuple(typeCheck(e.tuple_))
  }

  private def inferType(value: Untyped.Value): Value[A] forSome {type A} = value match {
    case v: Untyped.VInt    => VInt(v.integer_)
    case v: Untyped.VString => VString(v.string_)
  }

  private def typeCheck(tuple: Untyped.Tuple): Tuple[A] forSome {type A} = tuple match {
    case t: Untyped.Tuple2 => Tuple2(typeCheck(t.typedexpr_1), typeCheck(t.typedexpr_2))
    case t: Untyped.Tuple3 => Tuple3(typeCheck(t.typedexpr_1), typeCheck(t.typedexpr_2), typeCheck(t.typedexpr_3))
  }

  private def extractType(typ: Untyped.Type): Type[A] forSome {type A} = typ match {
    case t: Untyped.TSimple => extractType(t)
    case t: Untyped.TTuple  => TTuple(extractType(t.ttype_))
    case t: Untyped.TFun    => TFun(extractType(t.type_1), extractType(t.type_2))
  }

  private def extractType(ttype: Untyped.TType): TType[A] forSome {type A} = ttype match {
    case t: Untyped.TType2 => TType2(extractType(t.type_1), extractType(t.type_2))
    case t: Untyped.TType3 => TType3(extractType(t.type_1), extractType(t.type_2), extractType(t.type_3))
  }

  private def extractType(typ: Untyped.TSimple): TSimple[A] forSome {type A} = typ.simpletype_ match {
    case "Integer" => TSimple[Integer]()
    case "String"  => TSimple[String]()
    case "Char"    => TSimple[Char]()
  }

  def applyAbs[A,B](abs: ETyped[A => B], value: ETyped[A]) : ETyped[B] = abs.typ match {
    case TFun(_, tTo) => ETyped(EApp(abs, value), tTo)
    case _            => sys.error("Typed abstraction does not have type TFun")
  }

  def typedInt(i: Integer) : ETyped[Integer] = ETyped(EVal(VInt(i)), TSimple()

  def snd2[A,B](expr: ETyped[(A,B)]) : ETyped[B] = expr.typ match {
    case TTuple(TType2(_, tB)) => ETyped(ESecond2(expr), tB)
  }

  def newAbs[A,B](varName: String, varType: Type[A], expr: ETyped[B]) : ETyped[A => B] =
    ETyped(EAbs(varName, varType, expr), TFun(varType, expr.typ))

  def tuple2[A,B](exprA: ETyped[A], exprB: ETyped[B]) : ETyped[(A,B)] = ???

  def newVar() = ???

  def add() = ???
}
