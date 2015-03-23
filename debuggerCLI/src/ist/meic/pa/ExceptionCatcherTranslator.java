package ist.meic.pa;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
import javassist.NotFoundException;
import javassist.Translator;

public class ExceptionCatcherTranslator implements Translator {

	private static final String EXCEPTION_CATCHER_BODY = "{" + "try {"
			+ "DInterface debugInterface = new DInterface($e);"
			+ "Object resultValue = debugInterface.run();"
			+ "return ($r) resultValue;" + "} catch (Exception e) {"
			+ "throw e;" + "}" + "}";

	@Override
	public void onLoad(ClassPool pool, String className)
			throws NotFoundException, CannotCompileException {
		CtClass ctClass = pool.get(className);
		insertExceptionCatchers(ctClass);

	}

	@Override
	public void start(ClassPool arg0) throws NotFoundException,
			CannotCompileException {
		// Do nothing.
	}

	private void insertExceptionCatchers(CtClass ctClass) {
		for (CtMethod ctMethod : ctClass.getDeclaredMethods()) {
			insertExceptionCatcher(ctClass, ctMethod);
		}

	}

	private void insertExceptionCatcher(CtClass ctClass, CtMethod ctMethod) {
		try {
			CtClass etype = ClassPool.getDefault().get("java.lang.Exception");
			ctMethod.addCatch(EXCEPTION_CATCHER_BODY, etype);
		} catch (CannotCompileException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (NotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

}
