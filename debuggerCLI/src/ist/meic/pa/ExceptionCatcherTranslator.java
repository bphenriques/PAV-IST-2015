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

	public String generate_catcher(boolean isStatic){
		
		String packageName = this.getClass().getPackage().getName();
		String runArguments = !isStatic ? "e, $0" : "e";
		
		return  "{" 
				+ packageName + ".DInterface.pushToStack(\"%s\",$args);"
				+ "try{"
				+ "		while(true){"
				+ "			try {"
				+ "				return ($r) %s$original($$);"
				+ 			"} catch (Exception e) {"
				+ 				packageName + ".command.Command resultCommand = " + packageName + ".DInterface.run(" + runArguments + ");"
				+ 				"if (resultCommand.isReturnable())" 
				+					"return ($r) resultCommand.getResult();"
				+ 			"}"
				+ 		"}"
				+ "} finally {"
				+ 	packageName + ".DInterface.popStack();" 
				+ "}"
			+ "}";
		
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
			String body = generate_catcher(Modifier.isStatic(ctMethod.getModifiers()));
			
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
