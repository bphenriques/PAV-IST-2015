package ist.meic.pa;

import java.lang.reflect.Modifier;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
import javassist.CtNewMethod;
import javassist.NotFoundException;
import javassist.Translator;
import javassist.expr.ExprEditor;
import javassist.expr.MethodCall;

public class ExceptionCatcherTranslator implements Translator {

	private final String PACKAGE_NAME = this.getClass().getPackage().getName() + ".debugger";
	
	
	@Override
	public void onLoad(ClassPool pool, String className)
			throws NotFoundException, CannotCompileException {
		// FIXME: it will ignore classes with package ist.meic.pa
		if (className.startsWith(PACKAGE_NAME))
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

	private final String generateCatcherBody(boolean isStatic){
		
		String runArguments = isStatic ? "e" : "e, $0";
		
		return  "{" 
				+ PACKAGE_NAME + ".DInterface.pushToStack(\"%s\", \"%s\" , $args);"
				+ "try{"
				+ "		while(true){"
				+ "			try {"
				+ "				return ($r) %s$original($$);"
				+ 			"} catch (Exception e) {"
				+ 				PACKAGE_NAME + ".command.Command resultCommand = " + PACKAGE_NAME + ".DInterface.run(" + runArguments + ");"
				+ 				"if (resultCommand.isReturnable())" 
				+					"return ($r) resultCommand.getResult();"
				+ 			"}"
				+ 		"}"
				+ "} finally {"
				+ 	PACKAGE_NAME + ".DInterface.popStack();" 
				+ "}"
			+ "}";
		
	}
	
	private final String generateMethodCallBody(boolean isStatic, String methodName){
		
		return "{"
				+ PACKAGE_NAME + ".DInterface.pushToStack(\"%s\", \"%s\" , $args);"
				+ PACKAGE_NAME + ".DInterface.run($class, \"%s\" , $args);"
				+"$_ = $proceed($$);"
				+ PACKAGE_NAME + ".DInterface.popStack();" 
				+"}";
		
	}
	
	private void insertExceptionCatcher(CtClass ctClass, CtMethod ctMethod) {
		try {
			
			//codemakeoverimminent
			ExprEditor editor = new ExprEditor(){
				public void edit(MethodCall methodCall) throws CannotCompileException{
					
					String className = methodCall.getClassName();
					String methodName = methodCall.getMethodName();
					
					String completeMethodName = className + "." + methodName;

					String methodCallBody = generateMethodCallBody(false, methodName);
					
					methodCall.replace(String.format(methodCallBody, className, completeMethodName, methodName));
				
				}
			
			};
			
			ctMethod.instrument(editor);
			
			///codemakeoveeeeer
			/*ctMethod.setName(methodName + "$original");
			ctMethod = CtNewMethod.copy(ctMethod, methodName, ctClass, null);
			
			CtClass eType = ClassPool.getDefault().get("java.lang.Exception");

			CtClass[] eTypes = { eType };
			ctMethod.setExceptionTypes(eTypes); //To catch and throw checked exceptions
			String body = generateCatcherBody(Modifier.isStatic(ctMethod.getModifiers()));
			
			ctMethod.setBody(String.format(body, ctClass.getName(), completeMethodName, methodName));
			
			ctClass.addMethod(ctMethod);*/
		}/* catch (NotFoundException e1) {
			System.err.println("Error finding something: " + e1);
			System.exit(-1);
		}*/ catch (CannotCompileException e) {
			System.err.println("Error compiling: " + e);
		}
	}
}
