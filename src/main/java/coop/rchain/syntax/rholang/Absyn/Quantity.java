package coop.rchain.syntax.rholang.Absyn; // Java Package generated by the BNF Converter.

public abstract class Quantity implements java.io.Serializable {
  public abstract <R,A> R accept(Quantity.Visitor<R,A> v, A arg);
  public interface Visitor <R,A> {
    public R visit(coop.rchain.syntax.rholang.Absyn.QBool p, A arg);
    public R visit(coop.rchain.syntax.rholang.Absyn.QInt p, A arg);
    public R visit(coop.rchain.syntax.rholang.Absyn.QDouble p, A arg);

  }

}
