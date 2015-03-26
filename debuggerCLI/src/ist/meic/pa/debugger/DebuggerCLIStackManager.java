package ist.meic.pa.debugger;

import java.util.Enumeration;
import java.util.Stack;

public final class DebuggerCLIStackManager {
	private final static Stack<MethodPrint> _stack = new Stack<MethodPrint>();
	
	public static final void push(MethodPrint mp){
		_stack.push(mp);
	}

	public static final void pop() {
		_stack.pop();
	}

	public static final MethodPrint getMostRecentMethodCall() {
		return _stack.get(_stack.size() - 1);
	}

	public static final Enumeration<MethodPrint> getStackEnumeration() {
		return _stack.elements();
	}
}
