package ist.meic.pa;

import java.lang.reflect.Modifier;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
import javassist.CtNewMethod;
import javassist.NotFoundException;
import javassist.Translator;

public class ExceptionCatcherTranslator implements Translator {

	private static final String EXCEPTION_CATCHER_NEW_BODY = "{" 
			+ "ist.meic.pa.DInterface.pushToStack(\"%s\",$args);"
			+ "try{"
			+ "while(true){"
			+ "try {"
			+ "return ($r) %s$original($$);" + "} catch (Exception e) {"
			+ "ist.meic.pa.command.Command resultCommand = ist.meic.pa.DInterface.run(e, $0);"
			+ "if (resultCommand.isReturnable())" 
			+	"return ($r) resultCommand.getResult();"
			+ "}" 
			+ "}"
			+ "} finally {"
			+ "ist.meic.pa.DInterface.popStack();" 
			+ "}"
			+ "}";

	private static final String EXCEPTION_CATCHER_NEW_STATIC_BODY = "{"
			+ "ist.meic.pa.DInterface.pushToStack(\"%s\",$args);"
			+ "try {"
			+ "while(true){"
			+ "try {" 
			+ "return ($r) %s$original($$);" + "} catch (Exception e) {"
			+ "ist.meic.pa.command.Command resultCommand = ist.meic.pa.DInterface.run(e);"
			+ "if (resultCommand.isReturnable())" 
			+	"return ($r) resultCommand.getResult();"
			+ "}"
			+ "}"
			+ "} finally {"
			+ "ist.meic.pa.DInterface.popStack();"
			+ "}" 
			+ "}";

	@Override
	public void onLoad(ClassPool pool, String className)
			throws NotFoundException, CannotCompileException {
		// FIXME: it will ignore classes with package ist.meic.pa
		if (className.startsWith("ist.meic.pa"))
			return;

		CtClass ctClass = pool.get(className);
		for (CtMethod ctMethod : ctClass.getDeclaredMethods()) {
			insertExceptionCatcher(ctClass, ctMethod);
		}
	}

	@Override
	public void start(ClassPool arg0) throws NotFoundException,
			CannotCompileException {
		// Do nothing.
	}

	private void insertExceptionCatcher(CtClass ctClass, CtMethod ctMethod) {
		try {
			String methodName = ctMethod.getName();
			String completeMethodName = ctClass.getName() + "." +ctMethod.getName();

			ctMethod.setName(methodName + "$original");
			ctMethod = CtNewMethod.copy(ctMethod, methodName, ctClass, null);
			
			CtClass eType = ClassPool.getDefault().get("java.lang.Exception");

			CtClass[] eTypes = { eType };
			ctMethod.setExceptionTypes(eTypes); //To catch and throw checked exceptions
			String body;
			if (Modifier.isStatic(ctMethod.getModifiers())) {
				body = EXCEPTION_CATCHER_NEW_STATIC_BODY;
			} else {
				body = EXCEPTION_CATCHER_NEW_BODY;
			}
			ctMethod.setBody(String.format(body, completeMethodName, methodName));
			
			ctClass.addMethod(ctMethod);
		} catch (NotFoundException e1) {
			System.err.println("Error finding something: " + e1);
			System.exit(-1);
		} catch (CannotCompileException e) {
			System.err.println("Error compiling: " + e);
		}
	}
}
