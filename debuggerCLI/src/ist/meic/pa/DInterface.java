package ist.meic.pa;

import java.util.Scanner;

public class DInterface {

	private final static String STACK_TRACE_PRINT = "Inside %s.%s";

	private final static String ABORT_COMMAND = "Abort";
	private final static String INFO_COMMAND = "Info";
	private final static String THROW_COMMAND = "Throw";
	private final static String RETURN_COMMAND = "Return";
	private final static String GET_COMMAND = "Get";
	private final static String SET_COMMAND = "Set";
	private final static String RETRY_COMMAND = "Retry";

	public Exception thrownException;

	public DInterface(Exception thrownException) {
		super();
		this.thrownException = thrownException;
	}

	public void run() throws Exception {

		Scanner sc = new Scanner(System.in);
		try {
			String input;

			printClassesInStack(thrownException.getStackTrace());
			printCommandPrompt();

			while (true) {
				input = sc.next();

				switch (input) {
				case ABORT_COMMAND:
					System.exit(0);
					break;
				case INFO_COMMAND:
					// TODO
					break;
				case THROW_COMMAND:
					// TODO
					break;
				case RETURN_COMMAND:

					break;
				case GET_COMMAND:
					// TODO
					break;
				case SET_COMMAND:
					// TODO
					break;
				case RETRY_COMMAND:
					// TODO
					break;
				default:
					System.out.println("Unrecognized Command: " + input);
					break;
				}
			}
			
		} finally {
			sc.close();
		}

	}

	private void printCommandPrompt() {
		System.out.print("DebuggerCLI:> ");
		System.out.flush();
	}

	private void printClassesInStack(StackTraceElement[] stackTraceElements) {
		for (StackTraceElement ste : stackTraceElements) {
			String className = ste.getClassName();
			String methodName = ste.getMethodName();
			System.out.println(String.format(STACK_TRACE_PRINT, className,
					methodName));
		}

	}

}
