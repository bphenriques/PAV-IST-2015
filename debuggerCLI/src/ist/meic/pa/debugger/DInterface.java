package ist.meic.pa.debugger;

import java.lang.reflect.Method;

public abstract class DInterface {

	protected abstract Object invokeMethodWithDebug(Class<?> targetClass, Object target, Method callingMethod, Object args[]) throws Throwable;
	
	public Object run(Class<?> targetClass, Object target,
			Class<?> returnType, String methodName, Class<?> parameterTypes[],
			Object args[]) throws Throwable {


		DebuggerCLIStackManager.push(new MethodPrint(targetClass,  methodName, args));
		
		Method callingMethod = targetClass.getDeclaredMethod(
				methodName, parameterTypes);
		
		boolean previousAccessibility = callingMethod.isAccessible();
		callingMethod.setAccessible(true);
		
		Object returnObject = invokeMethodWithDebug(targetClass, target, callingMethod, args);
		
		callingMethod.setAccessible(previousAccessibility);
		
		DebuggerCLIStackManager.pop();
		
		return returnObject;
	}
	
	
}
