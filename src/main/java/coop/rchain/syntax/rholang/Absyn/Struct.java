package coop.rchain.syntax.rholang.Absyn; // Java Package generated by the BNF Converter.

public abstract class Struct implements java.io.Serializable {
  public abstract <R,A> R accept(Struct.Visitor<R,A> v, A arg);
  public interface Visitor <R,A> {
    public R visit(coop.rchain.syntax.rholang.Absyn.StructConstr p, A arg);

  }

}
