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

	private final String PACKAGE_NAME = this.getClass().getPackage().getName();
	private final String JAVA_ASSIST_PACKAGE = "javassist";
	
	private final Class<?> desiredInterfaceClass;
	
	public ExceptionCatcherTranslator(Class<?> desiredInterfaceClass) {
		this.desiredInterfaceClass = desiredInterfaceClass;
	}
	
	@Override
	public void onLoad(ClassPool pool, String className)
			throws NotFoundException, CannotCompileException {
		
		if (className.startsWith(PACKAGE_NAME) || className.startsWith(JAVA_ASSIST_PACKAGE))
			return;

		CtClass ctClass = pool.get(className);
		for (CtMethod ctMethod : ctClass.getDeclaredMethods()) {
			//System.out.println("CHANGING: " + ctMethod.getName());
			insertExceptionCatcher(ctClass, ctMethod);
		}
	}

	@Override
	public void start(ClassPool arg0) throws NotFoundException,
			CannotCompileException {
		// Do nothing.
	}
	
	private final String generateMethodCallBody(String methodName){		
		
		String interfaceClassName = desiredInterfaceClass.getName();
		
		return 
			"{"
				+  interfaceClassName + " d = new " + interfaceClassName + "();"
				+ "$_ = ($r) d.run($class, $0, $type, \"" + methodName + "\", $sig, $args);"
				
			+"}";
		
	}
	
	private void insertExceptionCatcher(CtClass ctClass, CtMethod ctMethod) {
		try {
			
			ExprEditor editor = new ExprEditor(){
				public void edit(MethodCall methodCall) throws CannotCompileException{
					String methodName = methodCall.getMethodName();
					String methodCallBody = generateMethodCallBody(methodName);
					methodCall.replace(methodCallBody);
				}
			};
			
			ctMethod.instrument(editor);
			
		} catch (CannotCompileException e) {
			System.err.println("Error compiling: " + e);
		}
	}
}
