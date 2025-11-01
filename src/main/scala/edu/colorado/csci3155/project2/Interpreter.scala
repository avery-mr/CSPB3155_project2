package edu.colorado.csci3155.project2

object Interpreter {

    def binaryExprEval(expr: Expr, expr1: Expr, env: Environment)(fun: (Value, Value) => Value): Value = {
        val v1 = evalExpr(expr, env)
        val v2 = evalExpr(expr1, env)
        fun(v1, v2)
    }

    def evalExpr(e: Expr, env: Environment): Value = e match {
        case Const(d) => NumValue(d)
        case ConstBool(b) => BoolValue(b)
        case Ident(s) => env.lookup(s)

        // case Line(l) => {
        //     evalExpr(l, env) match ----???
        case Line(l) => {

            val v = evalExpr(l, env)
            v match {
                case NumValue(v) => 
                    val line_obj = Polygon(List((0.0, 0.0), (v, 0.0)))
                    FigValue(new MyCanvas(List(line_obj)))
                case _ => throw new IllegalArgumentException("Requires a numeric value")
            }
        }
        case EquiTriangle(sideLength) => {
            val v = evalExpr(sideLength, env)
            v match {
                case NumValue(v) =>
                    val equiTri_obj = Polygon(List((0.0, 0.0), (v, 0.0), (v/2, math.sqrt(3.0)*v / 2)))
                    FigValue(new MyCanvas(List(equiTri_obj)))
                case _ => throw new IllegalArgumentException("Requires a numeric value")
            }

            
        }
        case Rectangle(sideLength) => {
            val v = evalExpr(sideLength, env)
            v match {
                case NumValue(v) =>
                    val rect_obj = Polygon(List((0.0, 0.0), (0.0, v), (v, v), (v, 0.0)))
                    FigValue(new MyCanvas(List(rect_obj)))
                case _ => throw new IllegalArgumentException("Requires a numeric value")
            }
        }

        case Circle(rad) => {
            val v = evalExpr(rad, env)
            v match {
                case NumValue(v) =>
                    val circle_obj = MyCircle((v, v), v)
                    FigValue(new MyCanvas(List(circle_obj)))
                case _ => throw new IllegalArgumentException("Requires a numeric value")
            }    
        }

        case Plus (e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            (v1, v2) match {
                case (NumValue(v1), NumValue(v2)) => NumValue(v1 + v2)
                case (FigValue(c1), FigValue(c2)) => FigValue(c1.overlap(c2))
                case _ => throw new IllegalArgumentException("Addition requires two NumValues or two FigValues")
            }
        }
        case Minus (e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.minus)
        case Mult(e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            (v1, v2) match {
                case (NumValue(v1), NumValue(v2)) => NumValue(v1 * v2)
                case (FigValue(c1), FigValue(c2)) => FigValue(c1.placeRight(c2))
                case _ => throw new IllegalArgumentException("Multiplication requires either two NumValues or two FigValues")
            }
        }

        case Div(e1, e2) => {
            val v1 = evalExpr(e1, env)
            val v2 = evalExpr(e2, env)
            (v1, v2) match {
                case (NumValue(v1), NumValue(v2)) => 
                    if (v2 == 0.0) throw new IllegalArgumentException("Cannot divide by zero")
                    else NumValue(v1 / v2)
                case (FigValue(c), NumValue(v)) => FigValue(c.rotate(v))
                case (FigValue(c1), FigValue(c2)) => FigValue(c2.placeTop(c1))
                case _ => throw new IllegalArgumentException("Division requires either two NumValues or two FigValues")
            }    
        }
        case Geq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.geq)
        case Gt(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.gt)
        case Eq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.equal)
        case Neq(e1, e2) => binaryExprEval(e1, e2, env) (ValueOps.notEqual)
        case And(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("And applied to a non-Boolean value")
                    }
                }
                case BoolValue(false) => BoolValue(false)
                case _ => throw new IllegalArgumentException("And applied to a non-boolean value")
            }
        }

        case Or(e1, e2) => {
            val v1 = evalExpr(e1, env)
            v1 match {
                case BoolValue(true) => BoolValue(true)
                case BoolValue(false) => {
                    val v2 = evalExpr(e2, env)
                    v2 match {
                        case BoolValue(_) => v2
                        case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean value")
                    }
                }
                case _ => throw new IllegalArgumentException("Or Applied to a non-Boolean Value")
            }
        }

        case Not(e) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(b) => BoolValue(!b)
                case _ => throw new IllegalArgumentException("Not applied to a non-Boolean Value")
            }
        }

        case IfThenElse(e, e1, e2) => {
            val v = evalExpr(e, env)
            v match {
                case BoolValue(true) => evalExpr(e1, env)
                case BoolValue(false) => evalExpr(e2,env)
                case _ => throw new IllegalArgumentException("If then else condition is not a Boolean value")
            }
        }


        case Let(x, e1, e2) => {
            val v1 = evalExpr(e1, env)
            val env2 = Extend(x, v1, env)
            evalExpr(e2, env2)
        }

        case FunDef(x, e) => Closure(x, e, env)
        case LetRec(f, x, e1, e2) => {
            val env1 = ExtendREC(f, x, e1, env)
            evalExpr(e2, env1)
        }
        case FunCall(fCallExpr, arg) => {
            val f1 = evalExpr(fCallExpr, env)
            f1 match {
                case Closure(x, e, env1) =>
                    val argEval = evalExpr(arg, env)
                    evalExpr(e, Extend(x, argEval, env1))
            }   
        }
    }

    def evalProgram(p: Program): Value = p match {
        case TopLevel(e) => evalExpr(e, EmptyEnvironment)
    }

}
