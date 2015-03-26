package ist.meic.pa;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
import javassist.NotFoundException;
import javassist.Translator;
import javassist.expr.ExprEditor;
import javassist.expr.MethodCall;

public class ExceptionCatcherTranslator implements Translator {

	private final String PACKAGE_NAME = this.getClass().getPackage().getName() + ".debugger";
	private final String JAVA_ASSIST_PACKAGE = "javassist";
	
	@Override
	public void onLoad(ClassPool pool, String className)
			throws NotFoundException, CannotCompileException {
		
		if (className.startsWith(PACKAGE_NAME) || className.startsWith(JAVA_ASSIST_PACKAGE))
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
	
	private final String generateMethodCallBody(String className, String methodName){		
		
		return 
			"{"
				//+ "System.out.println(\"BURRAAAAA\");"
				//+ "System.out.println( \"Class: \"" + className + "\");"
				//+ "System.out.println(\"Class:\" " + className + "\"\n target: \" + $0 + \" \nreturnType: \" + $type + \"\n MethodName:\"" + methodName + "\");"
					+ "$_ = ($r)" + PACKAGE_NAME + ".DInterface.run(\"" + className + "\", $0, $type, \"" + methodName + "\", $sig, $args);"
				
			+"}";
		
	}
	
	private void insertExceptionCatcher(CtClass ctClass, CtMethod ctMethod) {
		try {
			
			ExprEditor editor = new ExprEditor(){
				public void edit(MethodCall methodCall) throws CannotCompileException{
					
					String className = methodCall.getClassName();
					String methodName = methodCall.getMethodName();
					
					String methodCallBody = generateMethodCallBody(className, methodName);
					
					String completeMethodName = className + "." + methodName;
					methodCall.replace(String.format(methodCallBody, className, completeMethodName, methodName));
				
				}
			};
			
			ctMethod.instrument(editor);
			
		} catch (CannotCompileException e) {
			System.err.println("Error compiling: " + e);
		}
	}
}
