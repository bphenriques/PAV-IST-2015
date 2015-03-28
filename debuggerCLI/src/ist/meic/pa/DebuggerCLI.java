package ist.meic.pa;

import ist.meic.pa.debugger.DInterfaceSimple;
import javassist.ClassPool;
import javassist.Loader;
import javassist.Translator;


/**
 * The Class DebuggerCLI is responsible for starting
 * the program with the appropriate modifications
 * made in other classes
 */
public class DebuggerCLI {


	private static final String DEBUGGER_CLI = "DebuggerCLI";
	private static final String RUNNERCLASS = "DebugRunner";
	private static final String DEBUGGER_PACKAGE = "ist.meic.pa";

	/**
	 * The main method.
	 *
	 * @param args the arguments provided by the user
	 * @throws Throwable if the user chooses throws the user exception in the main
	 */
	public static void main(String[] args) throws Throwable {
		if (args.length < 1) {
			System.err.println("Usage: java " + DEBUGGER_PACKAGE + "." + DEBUGGER_CLI + " <Package>.<Class> <Args>");
			System.exit(1);
		} else {
			String className = args[0];
			Translator exceptionCatcherTrans = new ExceptionCatcherTranslator(DInterfaceSimple.class, Class.forName(className));
			Loader classLoader = new Loader();
			
			ClassPool pool = ClassPool.getDefault();
			classLoader.addTranslator(pool, exceptionCatcherTrans);
			
			String[] restArgs = new String[args.length - 1];
			System.arraycopy(args, 1, restArgs, 0, restArgs.length);

			classLoader.run(DEBUGGER_PACKAGE + "." + RUNNERCLASS, restArgs);
		}
	}


}
