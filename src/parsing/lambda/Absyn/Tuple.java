package parsing.lambda.Absyn; // Java Package generated by the BNF Converter.

public abstract class Tuple implements java.io.Serializable {
  public abstract <R,A> R accept(Tuple.Visitor<R,A> v, A arg);
  public interface Visitor <R,A> {
    public R visit(parsing.lambda.Absyn.Tuple2 p, A arg);
    public R visit(parsing.lambda.Absyn.Tuple3 p, A arg);

  }

}