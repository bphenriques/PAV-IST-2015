package ist.meic.pa;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
import javassist.CtNewMethod;
import javassist.NotFoundException;
import javassist.Translator;

public class ExceptionCatcherTranslator implements Translator {
	
	private static final String EXCEPTION_CATCHER_NEW_BODY = "{" + "try {"
			+ "return ($r) %s$original($$);" + "} catch (Exception e) {"
			+ "Object resultValue = ist.meic.pa.DInterface.run(e);"
			+ "return ($r) resultValue;" + "}" + "}";

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
			String name = ctMethod.getName();

			ctMethod.setName(name + "$original");

			ctMethod = CtNewMethod.copy(ctMethod, name, ctClass, null);
			CtClass eType = ClassPool.getDefault().get("java.lang.Exception");

			CtClass[] eTypes = { eType };
			ctMethod.setExceptionTypes(eTypes); //To catch and throw checked exceptions
			ctMethod.setBody(String.format(EXCEPTION_CATCHER_NEW_BODY, name));
			ctClass.addMethod(ctMethod);
		} catch (NotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		} catch (CannotCompileException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}


}
