package openisetl.runtime._val;

public final class BaseValRef {
	
	public BaseValRef() {
	}
	
	public BaseValRef(GenericBaseVal val) {
		this.ref = val;
	}
	
	@Override
	public String toString() {
		return "Ref(" + ref + ")";
	}
	
	public GenericBaseVal ref;
	public static final UndefinedVal undefined = UndefinedVal$.MODULE$.undefined();
	public static final BooleanVal _true = BooleanVal$.MODULE$._true();
	public static final BooleanVal _false = BooleanVal$.MODULE$._false();
}
