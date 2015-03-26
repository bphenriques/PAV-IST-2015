package ist.meic.pa;

import ist.meic.pa.debugger.DInterfaceSimple;
import javassist.ClassPool;
import javassist.Loader;
import javassist.Translator;

public class DebuggerCLI {

	
	public static void main(String[] args) throws Throwable {
		if (args.length < 1) {
			System.err.println("Usage: java ist.meic.pa.DebuggerCLI <Package>.<Class> <Args>");
			System.exit(1);
		} else {
			String className = args[0];
			Translator exceptionCatcherTrans = new ExceptionCatcherTranslator(DInterfaceSimple.class, Class.forName(className));
			Loader classLoader = new Loader();
			
			ClassPool pool = ClassPool.getDefault();
			classLoader.addTranslator(pool, exceptionCatcherTrans);
			
			String[] restArgs = new String[args.length - 1];
			System.arraycopy(args, 1, restArgs, 0, restArgs.length);

			//classLoader.run(className, restArgs);
			classLoader.run("ist.meic.pa.DebugRunner", restArgs);
			
		}
	}


}
