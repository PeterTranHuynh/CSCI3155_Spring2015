object Lab2 extends jsy.util.JsyApplication {
  import jsy.lab2.Parser
  import jsy.lab2.ast._
  
  /*
   * CSCI 3155: Lab 2
   * Peter Huynh
   * 
   * Partner: NONE
   * Collaborators: NONE
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace 'YourIdentiKey' in the object name above with your IdentiKey.
   * 
   * Replace the 'throw new UnsupportedOperationException' expression with
   * your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * 'throws new UnsupportedOperationException' as needed to get something
   * that compiles without error.
   */
  
  /* We represent a variable environment is as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */
  
  type Env = Map[String, Expr]
  val emp: Env = Map()
  def get(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }
  
  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n.toDouble
      
      case Undefined => Double.NaN
      case null => 0.0
      case B(true) => 1.0
      case B(false) => 0.0
      case S(s) => try s.toDouble catch { case _: Throwable => Double.NaN }
    }
  }
  
  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      
      case Undefined => false
      case null => false
      case N(n) if ((n compare 0.0) == 0 || (n compare -0.0) == 0 || n.isNaN) => false
      case N(_) => true
      case S("") => false
      case S(_) => true
    }
  }
  
  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case Undefined => "undefined"
      
      case N(n) => if (n.isWhole) "%.0f" format n else n.toString
      case B(b) => b.toString
    }
  }
  
  /*
   * Helper function that implements the semantics of operators
   */
  def compare(bop: Bop, e1: Expr, e2: Expr): Boolean = {
    require(isValue(e1))
    require(isValue(e2))
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge || bop == Eq)
    (e1, e2) match
    {
      case (S(s1), S(s2)) =>
        (bop: @unchecked) match
        {
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
          case Eq => s1 == s2
          case Ne => s1 != s2
        }
      case _ =>
        val (n1, n2) = (toNumber(e1), toNumber(e2))
        (bop: @unchecked) match
        {
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2
          case Eq => n1 == n2
          case Ne => n1 != n2
        }
    }
  }
  
  def eval(env: Env, e: Expr): Expr = {
    /* Some helper functions for convenience. */
    def eToVal(e: Expr): Expr = eval(env, e)
    
    e match {
      /* Base Cases */
      case _ if isValue(e) => e
      case Var(x) => get(env, x)
      
      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined
      
      case Unary(Neg, e1) => N(- toNumber(e1))
      case Unary(Not, e1) => B(! toBoolean(e1))
      
      // I don't understand why this doesn't work, so I just stopped using the compare helper function for Eq and Ne
      // case Binary(bop @ (Eq | Ne), e1, e2) => B(compare(bop, eToVal(e1), eToVal(e2)))
      case Binary(bop @ (Eq | Ne), e1, e2) if (isValue(e1) && isValue(e2)) => bop match
      {
        case Eq => B(toNumber(e1) == toNumber(e2))
        case Ne => B(toNumber(e1) != toNumber(e2))
        case _ => throw new UnsupportedOperationException 
      }
      case Binary(bop @ (Lt | Le | Gt | Ge), e1, e2) => B(compare(bop, eToVal(e1), eToVal(e2)))
      
      case Binary(Plus, e1, e2) => (eToVal(e1), eToVal(e2)) match
      {
        case (S(s1), n2) => S(s1 + toStr(n2))
        case (n1, S(s2)) => S(toStr(n1) + s2)
        case (n1, n2) => N(toNumber(n1) + toNumber(n2))
      }      
      case Binary(Minus, e1, e2) => N(toNumber(e1) - toNumber(e2))
      case Binary(Times, e1, e2) => N(toNumber(e1) * toNumber(e2))
      case Binary(Div, e1, e2) => N(toNumber(e1) / toNumber(e2))
      
      case Binary(And, e1, e2) => 
      {
        val n1 = eToVal(e1)
        if (toBoolean(n1)) eToVal(e2)
        else n1
      }
      case Binary(Or, e1, e2) =>
      {
        val n1 = eToVal(e1)
        if (toBoolean(n1)) n1
        else eToVal(e2)
      }
      
      case Binary(Seq, e1, e2) => eToVal(e1); eToVal(e2)
      case If(e1, e2, e3) => 
        if (toBoolean(e1)) eToVal(e2)
        else eToVal(e3)
      case ConstDecl(x, e1, e2) => eval(extend(env, x, eToVal(e1)), e2)

      case _ => throw new UnsupportedOperationException
    }
  }
    
  // Interface to run your interpreter starting from an empty environment.
  def eval(e: Expr): Expr = eval(emp, e)

  // Interface to run your interpreter from a string.  This is convenient
  // for unit testing.
  def eval(s: String): Expr = eval(Parser.parse(s))

 /* Interface to run your interpreter from the command-line.  You can ignore what's below. */ 
 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }
    
    val expr = Parser.parseFile(file)
    
    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }
    
    if (debug) { println("Evaluating ...") }
    
    val v = eval(expr)
    
    println(pretty(v))
  }

}