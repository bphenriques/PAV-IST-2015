package ist.meic.pa.debugger.stack;

public class StackElement {
	private final Object[] arguments;
	private final String methodName;
	private final Class<?> invokingClass;
	private Class<?> parameterTypes[] = null;
	private Class<?> returnType = null;

	public StackElement(Class<?> invokingClass, String methodName, Class<?> returnType, Object... arguments) {
		super();
		this.methodName = methodName;
		this.invokingClass = invokingClass;
		this.arguments = arguments;
		this.returnType = returnType;
	}
	
	public Class<?> getReturnType() {
		return returnType;
	}
	
	public void setParametersTypes(Class<?>...parameterTypes){
		this.parameterTypes = parameterTypes;
	}
	
	public Object[] getArguments() {
		return arguments;
	}
	
	public String getMethodName() {
		return methodName;
	}
	
	public Class<?> getIvokingClass() {
		return invokingClass;
	}
	
	public Class<?>[] getParameterTypes(){
		return parameterTypes;
	}
}
