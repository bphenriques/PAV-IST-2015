package ist.meic.pa.debugger;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public abstract class DInterface {

	protected Class<?> targetClass;
	protected Object target;
	protected Class<?> returnType;
	protected String methodName;
	protected Class<?> parameterTypes[] = null;
	protected Object args[] = null;

	protected Object _returnResult = null;
	protected Method _callingMethod = null;
	
	
	public DInterface(Class<?> targetClass, Object target,
			Class<?> returnType, String methodName, Class<?> parameterTypes[],
			Object args[]){
		
		this.targetClass = targetClass;
		this.target = target;
		this.returnType = returnType;
		this.methodName = methodName;
		this.parameterTypes = parameterTypes;
		this.args = args;
	}
	
	public abstract void debugMethod(Throwable thrownException,
			Class<?> targetClass, Object target) throws Throwable;
	
	
	public Object run() throws Throwable {

		MethodPrint m = new MethodPrint(targetClass,  methodName, returnType, args);
		m.setParametersTypes(parameterTypes);
		DebuggerCLIStackManager.push(m);

		_callingMethod = targetClass.getDeclaredMethod(
				methodName, parameterTypes);
		
		
		while(_returnResult == null){
			boolean previousAccessibility = _callingMethod.isAccessible();
			try {
				_callingMethod.setAccessible(true);
				_returnResult = _callingMethod.invoke(target, args);
			} catch (InvocationTargetException e) {
				debugMethod(e.getTargetException(), targetClass, target);
			}finally{
				_callingMethod.setAccessible(previousAccessibility);
				DebuggerCLIStackManager.pop();
			}
		}
		
		return _returnResult;
	}
	
	
}
