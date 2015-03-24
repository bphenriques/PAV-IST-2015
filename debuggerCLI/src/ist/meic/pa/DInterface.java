package ist.meic.pa;

import ist.meic.pa.command.Command;
import ist.meic.pa.command.CommandManager;
import ist.meic.pa.command.exception.CommandException;

import java.util.Iterator;
import java.util.Scanner;
import java.util.Stack;

public final class DInterface {

	private final static Stack<MethodPrint> stack = new Stack<MethodPrint>();
	private final static Scanner sc = new Scanner(System.in);

	public static Command run(Exception thrownException) throws Exception {
		return run(thrownException, null);
	}

	public static Command run(Exception thrownException, Object target)
			throws Exception {

		String input;
		// printClassesInStack(thrownException.getStackTrace());
		System.out.println(thrownException);

		while (true) {
			printCommandPrompt();
			input = sc.nextLine();

			try {
				Command c = CommandManager.executeCommand(thrownException,
						input, target);
				if (c.isReturnable() || c.isRetriable()) {
					return c;
				}

			} catch (CommandException e) {
				System.err.println(e.toString());
			}
		}

	}


	public static void pushToStack(String methodName, Object[] args) {
		MethodPrint method = new MethodPrint(methodName, args);
		stack.push(method);
	}

	public static void popStack() {
		stack.pop();
	}

	public static Iterator<MethodPrint> getStackIterator() {
		return stack.iterator();
	}

	private static void printCommandPrompt() {
		System.out.print("DebuggerCLI:> ");
		System.out.flush();
	}

}
