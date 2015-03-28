package ist.meic.pa.debugger.stack;

import java.util.Enumeration;
import java.util.Stack;

/**
 * The Class StackManager controls the stack, it stores and provides access to the previous method calls.
 */
public final class StackManager {
	
	/** The Constant _stack. */
	private final static Stack<StackElement> _stack = new Stack<StackElement>();
	
	/**
	 * Push.
	 *
	 * @param mp the mp
	 */
	public static final void push(StackElement mp){
		_stack.add(0, mp);
	}

	/**
	 * Pop.
	 */
	public static final void pop() {
		_stack.remove(0);
	}

	/**
	 * Gets the most recent method call.
	 *
	 * @return the most recent method call
	 */
	public static final StackElement getMostRecentMethodCall() {
		return _stack.get(0);
	}

	/**
	 * Gets the stack enumeration.
	 *
	 * @return the stack enumeration
	 */
	public static final Enumeration<StackElement> getStackEnumeration() {
		return _stack.elements();
	}
}
