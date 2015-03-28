package ist.meic.pa.debugger;

import ist.meic.pa.debugger.stack.StackElement;
import ist.meic.pa.debugger.stack.StackManager;

import java.lang.reflect.Method;

public abstract class DInterface {

	protected abstract Object invokeMethodWithDebug(Class<?> targetClass, Object target, Method callingMethod, Object args[]) throws Throwable;
	
	public Object run(Class<?> targetClass, Object target,
			Class<?> returnType, String methodName, Class<?> parameterTypes[],
			Object args[]) throws Throwable {

		StackElement m = new StackElement(targetClass,  methodName, returnType, args);
		m.setParametersTypes(parameterTypes);
		StackManager.push(m);
		Method callingMethod = targetClass.getDeclaredMethod(
				methodName, parameterTypes);
		
		callingMethod.setAccessible(true);
		boolean previousAccessibility = callingMethod.isAccessible();
		
		try{
			Object returnObject = invokeMethodWithDebug(targetClass, target, callingMethod, args);
			return returnObject;
		}finally{
			StackManager.pop();
			callingMethod.setAccessible(previousAccessibility);
		}
	}
	
	
}