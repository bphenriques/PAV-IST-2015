package ist.meic.pa;

import ist.meic.pa.debugger.DInterfaceSimple;
import javassist.ClassPool;
import javassist.Loader;
import javassist.Translator;

public class ExtendedDebuggerCLI {

	public static void main(String[] args) throws Throwable {
		if (args.length < 1) {
			System.err.println("Usage: java ist.meic.pa.DebuggerCLI <Package>.<Class> <Args>");
			System.exit(1);
		} else {
			
			
			//FIXME FIXME FIXME FIXME
			Translator exceptionCatcherTrans = new ExceptionCatcherTranslator( /* HERE ---> */ DInterfaceSimple.class);
			Loader classLoader = new Loader();
			
			ClassPool pool = ClassPool.getDefault();
			classLoader.addTranslator(pool, exceptionCatcherTrans);
			
			String[] restArgs = new String[args.length - 1];
			System.arraycopy(args, 1, restArgs, 0, restArgs.length);

			String className = args[0];
			classLoader.run(className, restArgs);

		}

	}

}
