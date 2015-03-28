package ist.meic.pa.debugger;

import java.lang.reflect.Method;

public abstract class DInterface {

	protected abstract Object invokeMethodWithDebug(Class<?> targetClass, Object target, Method callingMethod, Object args[]) throws Throwable;
	
	public Object run(Class<?> targetClass, Object target,
			Class<?> returnType, String methodName, Class<?> parameterTypes[],
			Object args[]) throws Throwable {

		MethodPrint m = new MethodPrint(targetClass,  methodName, returnType, args);
		m.setParametersTypes(parameterTypes);
		DebuggerCLIStackManager.push(m);
		Method callingMethod = targetClass.getDeclaredMethod(
				methodName, parameterTypes);
		
		callingMethod.setAccessible(true);
		boolean previousAccessibility = callingMethod.isAccessible();
		
		try{
			Object returnObject = invokeMethodWithDebug(targetClass, target, callingMethod, args);
			return returnObject;
		}finally{
			DebuggerCLIStackManager.pop();
			callingMethod.setAccessible(previousAccessibility);
		}
	}
	
	
}