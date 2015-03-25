package ist.meic.pa;


public class MethodPrint {
	
	private final Object[] arguments;
	private final String methodName;
	private final String className;
	
	public MethodPrint(String className, String methodName, Object... arguments) {
		super();
		this.methodName = methodName;
		this.className = className;
		this.arguments = arguments;
	}
	
	public Object[] getArguments() {
		return arguments;
	}
	
	public String getMethodName() {
		return methodName;
	}
	
	public String getClassName() {
		return className;
	}
}
