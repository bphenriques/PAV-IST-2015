package ist.meic.pa.debugger;

import ist.meic.pa.debugger.stack.StackElement;
import ist.meic.pa.debugger.stack.StackManager;

import java.lang.reflect.Method;

/**
 * The Class DInterface is the class used in the method calls instead of the
 * normal method, it is made for extension.
 */
public abstract class DInterface {

	/**
	 * Invoke method with debug.
	 *
	 * @param targetClass
	 *            the target class
	 * @param target
	 *            the target
	 * @param callingMethod
	 *            the calling method
	 * @param args
	 *            the args from the method call
	 * @return the return from the function
	 * @throws Throwable
	 *             the throwable can be anything the user code throws
	 */
	protected abstract Object invokeMethodWithDebug(Class<?> targetClass,
			Object target, Method callingMethod, Object args[])
			throws Throwable;

	/**
	 * Run.
	 *
	 * @param targetClass
	 *            the class from where the method was declared
	 * @param target
	 *            the class instance, null if static
	 * @param returnType
	 *            the method return type
	 * @param methodName
	 *            the method name
	 * @param parameterTypes
	 *            the parameter types
	 * @param args
	 *            the arguments provided
	 * @return the object is the return from the method call
	 * @throws Throwable
	 *             the throwable can be anything the user code throws
	 */
	public Object run(Class<?> targetClass, Object target, Class<?> returnType,
			String methodName, Class<?> parameterTypes[], Object args[])
			throws Throwable {

		StackElement m = new StackElement(targetClass, methodName, returnType,
				args);
		m.setParametersTypes(parameterTypes);
		StackManager.push(m);
		Method callingMethod = targetClass.getDeclaredMethod(methodName,
				parameterTypes);

		callingMethod.setAccessible(true);
		boolean previousAccessibility = callingMethod.isAccessible();

		try {
			Object returnObject = invokeMethodWithDebug(targetClass, target,
					callingMethod, args);
			return returnObject;
		} finally {
			StackManager.pop();
			callingMethod.setAccessible(previousAccessibility);
		}
	}

}