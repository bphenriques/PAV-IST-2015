package ist.meic.pa;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
import javassist.NotFoundException;
import javassist.Translator;
import javassist.expr.ExprEditor;
import javassist.expr.MethodCall;

// TODO: Auto-generated Javadoc
/**
 * The Class ExceptionCatcherTranslator
 * instruments the user code.
 */
public class ExceptionCatcherTranslator implements Translator {

	/** The package name. */
	private final String PACKAGE_NAME = this.getClass().getPackage().getName();
	
	/** The java assist package. */
	private final String JAVA_ASSIST_PACKAGE = "javassist";
	
	/** The desired interface class. */
	private final Class<?> desiredInterfaceClass;
	
	/** The user main class. */
	private final Class<?> desiredMainClass;
	
	/**
	 * Instantiates a new exception catcher translator.
	 *
	 * @param desiredInterfaceClass the desired interface class
	 * @param desiredMainClass the class to be ran as main
	 */
	public ExceptionCatcherTranslator(Class<?> desiredInterfaceClass, Class<?> desiredMainClass) {
		this.desiredInterfaceClass = desiredInterfaceClass;
		this.desiredMainClass = desiredMainClass;
	}
	
	/* (non-Javadoc)
	 * @see javassist.Translator#onLoad(javassist.ClassPool, java.lang.String)
	 */
	@Override
	public void onLoad(ClassPool pool, String className)
			throws NotFoundException, CannotCompileException {
		
		if(className.startsWith(PACKAGE_NAME + ".DebugRunner")){
			//This guarantees that their main method is ran with the debugger, since it's an ordinary methodCall
			CtClass mainClass = pool.get(className);
			
			CtMethod mainMethod = mainClass.getDeclaredMethod("main");
			
			String mainBody = 
			  "{"
			+	  desiredMainClass.getName() + ".main($1);" 
			+ "}";
			
			
			mainMethod.setBody(mainBody);
			insertExceptionCatcher(mainClass, mainMethod);
		}
		
		if (className.startsWith(PACKAGE_NAME) || className.startsWith(JAVA_ASSIST_PACKAGE))
			return;

		CtClass ctClass = pool.get(className);
		for (CtMethod ctMethod : ctClass.getDeclaredMethods()) {
			//System.out.println("CHANGING: " + ctMethod.getName());
			insertExceptionCatcher(ctClass, ctMethod);
		}
	}

	/* (non-Javadoc)
	 * @see javassist.Translator#start(javassist.ClassPool)
	 */
	@Override
	public void start(ClassPool arg0) throws NotFoundException,
			CannotCompileException {
		// Do nothing.
	}
	
	/**
	 * Generate method call body.
	 *
	 * @param methodName the method to be called in the methodcall
	 * @return the the method call's body to replace the default method call body
	 */
	private final String generateMethodCallBody(String methodName){		
		
		String interfaceClassName = desiredInterfaceClass.getName();
		
		return 
			"{"
				+  interfaceClassName + " d = new " + interfaceClassName + "();"
				+ "$_ = ($r) d.run($class, $0, $type, \"" + methodName + "\", $sig, $args);"
				
			+"}";
		
	}
	
	/**
	 * Instruments the user code inserting the debugger.
	 *
	 * @param ctClass the ct class
	 * @param ctMethod the method to be instrumented
	 */
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
