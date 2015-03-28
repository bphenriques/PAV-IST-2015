package ist.meic.pa.debugger.stack;

import java.util.Enumeration;
import java.util.Stack;

public final class StackManager {
	private final static Stack<StackElement> _stack = new Stack<StackElement>();
	
	public static final void push(StackElement mp){
		_stack.add(0, mp);
	}

	public static final void pop() {
		_stack.remove(0);
	}

	public static final StackElement getMostRecentMethodCall() {
		return _stack.get(0);
	}

	public static final Enumeration<StackElement> getStackEnumeration() {
		return _stack.elements();
	}
}
