package ist.meic.pa;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Scanner;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
import javassist.CtNewMethod;
import javassist.NotFoundException;

public class DebuggerCLI {

	public final static String ABORT_COMMAND = "Abort";
	public final static String INFO_COMMAND = "Info";
	public final static String THROW_COMMAND = "Throw";
	public final static String RETURN_COMMAND = "Return";
	public final static String GET_COMMAND = "Get";
	public final static String SET_COMMAND = "Set";
	public final static String RETRY_COMMAND = "Retry";

	
	Throwable thrown;
	
	public DebuggerCLI(Throwable t) {
		thrown = t;
	}

	public static void main(String[] args) throws NotFoundException,
			CannotCompileException, NoSuchMethodException, SecurityException,
			IllegalAccessException, IllegalArgumentException,
			InvocationTargetException {
		if (args.length != 2) {
			System.err
					.println("Usage: java ist.meic.pa.DebuggerCLI <Package>.<Class> <Args>");
			System.exit(1);
		} else {
			ClassPool pool = ClassPool.getDefault();
			String className = args[0];
			CtClass ctClass = pool.get(args[0]);
			// INVOKE MAIN
			Class<?> rtClass = ctClass.toClass();
			Method main = rtClass.getMethod("main", args.getClass());

			String[] restArgs = new String[args.length - 2];
			System.arraycopy(args, 2, restArgs, 0, restArgs.length);

			
			try{
			
				main.invoke(null, new Object[] { restArgs });
			}catch(Throwable t){
				
				DebuggerCLI debugCLI = new DebuggerCLI(t);
				debugCLI.start();
				
			}
		}
	}

	private void start() {
			
			

		     Scanner sc = new Scanner(System.in);
		     String input;
		     
		     while(true){
		    	 input = sc.next();
		    	 
		    	 switch(input){
		    	 	case ABORT_COMMAND:
		    	 		//TODO
		    	 		break;
		    	 	case INFO_COMMAND:
		    	 		//TODO
		    	 		break;
		    	 	case THROW_COMMAND:
		    	 		//TODO
		    	 		break;
		    	 	case RETURN_COMMAND:
		    	 		//TODO
		    	 		break;
		    	 	case GET_COMMAND:
		    	 		//TODO
		    	 		break;
		    	 	case SET_COMMAND:
		    	 		//TODO
		    	 		break;
		    	 	case RETRY_COMMAND:
		    	 		//TODO
		    	 		break;
		    	 	default:
		    	 		System.out.println("Unrecognized Command: " + input);
		    	 		break;
		    	 }
		     }
		     
		     
		
	}

	private static void injectDebuggerCLI(CtClass ctClass, CtMethod ctMethod) throws CannotCompileException {
		/* CtField ctField = CtField.make(
				
				"static java.util.Hashtable cachedResults = "
						+ " new java.util.Hashtable();", ctClass); 
		ctClass.addField(ctField);*/
		String name = ctMethod.getName();
		ctMethod.setName(name + "$original");
		ctMethod = CtNewMethod.copy(ctMethod, name, ctClass, null);
		ctMethod.setBody("{" + " Object result = cachedResults.get($1);"
				+ " if (result == null) {" + " result = " + name
				+ "$original($$);" + " cachedResults.put($1, result);" + " }"
				+ " return ($r)result;" + "}");
		ctClass.addMethod(ctMethod);
		Exception exception;
		StackTraceElement st;


	}

	public static void parseDebug() {

	}

}
