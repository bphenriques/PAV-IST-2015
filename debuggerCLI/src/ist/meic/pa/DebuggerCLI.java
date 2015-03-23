package ist.meic.pa;

import javassist.ClassPool;
import javassist.Loader;
import javassist.Translator;

public class DebuggerCLI {

	
	public static void main(String[] args) throws Throwable {
		if (args.length < 1) {
			System.err
					.println("Usage: java ist.meic.pa.DebuggerCLI <Package>.<Class> <Args>");
			System.exit(1);
		} else {
			Translator exceptionCatcherTrans = new ExceptionCatcherTranslator();
			ClassPool pool = ClassPool.getDefault();
			String className = args[0];
			Loader classLoader = new Loader();
			classLoader.addTranslator(pool, exceptionCatcherTrans);
			String[] restArgs = new String[args.length - 1];
			System.arraycopy(args, 1, restArgs, 0, restArgs.length);
			classLoader.run(className, restArgs);

		}
	}


}
