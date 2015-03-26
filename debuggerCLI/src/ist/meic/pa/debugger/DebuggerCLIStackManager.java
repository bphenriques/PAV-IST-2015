package ist.meic.pa.debugger;

import java.util.Enumeration;
import java.util.Stack;

public final class DebuggerCLIStackManager {
	private final static Stack<MethodPrint> _stack = new Stack<MethodPrint>();
	
	public static final void pushToStack(String className, String methodName,
			Object[] args) {
		MethodPrint method = new MethodPrint(className, methodName, args);
		_stack.push(method);
	}

	public static final void popStack() {
		_stack.pop();
	}

	public static final MethodPrint getMostRecentMethodCall() {
		return _stack.get(_stack.size() - 1);
	}

	public static final Enumeration<MethodPrint> getStackEnumeration() {
		return _stack.elements();
	}
}
