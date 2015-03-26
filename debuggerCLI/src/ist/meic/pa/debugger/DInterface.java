package ist.meic.pa.debugger;

import ist.meic.pa.MethodPrint;
import ist.meic.pa.command.exception.CommandException;
import ist.meic.pa.debugger.command.Command;
import ist.meic.pa.debugger.command.CommandManager;

import java.lang.reflect.Method;
import java.util.Enumeration;
import java.util.Scanner;
import java.util.Stack;

public final class DInterface {

	private final static Stack<MethodPrint> _stack = new Stack<MethodPrint>();
	private final static Scanner sc = new Scanner(System.in);

	// + "_$ = ($r)" + PACKAGE_NAME +
	// ".DInterface.run($class, $0, $type, $proceed, $sig, $$);"

	public static Object run(String className, Object target,
			Class<?> returnType, String methodName, Class<?> parameterTypes[],
			Object args[]) throws Throwable {
		/*
		 * System.out.println("----------------------------------------");
		 * System.out.println("Class: " + className);
		 * System.out.println("target: " + target);
		 * System.out.println("returnType: " + returnType.getName());
		 * System.out.println("MethodName" + methodName);
		 * System.out.println("Parameter types " + parameterTypes);
		 * System.out.println("Args: " + args);
		 * System.out.println("----------------------------------------");
		 */

		DInterface.pushToStack(className, methodName, args);

		Method callingMethod = Class.forName(className).getDeclaredMethod(
				methodName, parameterTypes);
		boolean previousAccessibility = callingMethod.isAccessible();
		callingMethod.setAccessible(true);

		Object returnObject = null;
		
		boolean debug = true;
		while (debug) {
			try {
				returnObject = callingMethod.invoke(target, args);
				debug = false;
			} catch (Exception e) {
				Command command = debugMethod(e.getCause(), target);
				if (command.isReturnable()) {
					returnObject = command.getResult();
					debug = false;
				}else if (command.isRetriable()) {
					continue;
				}
				
				
			}
		}

		callingMethod.setAccessible(previousAccessibility);
		DInterface.popStack();
		
		return returnObject;
	}

	private static Command debugMethod(Throwable thrownException, Object target)
			throws Throwable {
		System.out.println(thrownException);
		while (true) {
			System.out.print("DebuggerCLI:> ");
			System.out.flush();

			String input = sc.nextLine();

			try {
				Command c = CommandManager.executeCommand(thrownException,
						input, target);
				
				if (c.isReturnable() || c.isRetriable()){
					return c;
				}
			} catch (CommandException e) {
				System.err.println("DEBUGGER ERROR : " + e);
			}
		}
	}

	public static void pushToStack(String className, String methodName,
			Object[] args) {
		MethodPrint method = new MethodPrint(className, methodName, args);
		_stack.push(method);
	}

	public static void popStack() {
		_stack.pop();
	}

	public static MethodPrint getMostRecentMethodCall() {
		return _stack.get(_stack.size() - 1);
	}

	public static Enumeration<MethodPrint> getStackEnumeration() {
		return _stack.elements();
	}

}
