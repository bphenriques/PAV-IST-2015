package ist.meic.pa;

public class MethodPrint {
	
	private Object[] arguments;
	private String methodName;
	
	public MethodPrint(String methodName, Object... arguments) {
		super();
		this.methodName = methodName;
		this.arguments = arguments;
	}
	
	public Object[] getArguments() {
		return arguments;
	}
	
	public String getMethodName() {
		return methodName;
	}

}
