package ist.meic.pa.debugger;

public class MethodPrint {
	private final Object[] arguments;
	private final String methodName;
	private final Class<?> invokingClass;
	private Class<?> parameterTypes[] = null;
	
	public MethodPrint(Class<?> invokingClass, String methodName, Object... arguments) {
		super();
		this.methodName = methodName;
		this.invokingClass = invokingClass;
		this.arguments = arguments;
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
