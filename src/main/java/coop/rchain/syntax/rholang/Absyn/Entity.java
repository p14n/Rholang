package coop.rchain.syntax.rholang.Absyn; // Java Package generated by the BNF Converter.

public abstract class Entity implements java.io.Serializable {
  public abstract <R,A> R accept(Entity.Visitor<R,A> v, A arg);
  public interface Visitor <R,A> {
    public R visit(coop.rchain.syntax.rholang.Absyn.EChar p, A arg);
    public R visit(coop.rchain.syntax.rholang.Absyn.EStruct p, A arg);
    public R visit(coop.rchain.syntax.rholang.Absyn.ECollect p, A arg);
    public R visit(coop.rchain.syntax.rholang.Absyn.ETuple p, A arg);

  }

}