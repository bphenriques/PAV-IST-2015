package ist.meic.pa.command.common;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

public final class FieldFinder {
	
	public static Field getDeclaredField(Class<?> type, String name) throws NoSuchFieldException, SecurityException{
		
		try {
			return type.getDeclaredField(name);
		} catch (NoSuchFieldException e) {
			if (type == Object.class) {
				throw new NoSuchFieldException(name);
			} else {
				return getDeclaredField(type.getSuperclass(), name);
			}
		}
	}
	
	public static List<Field> getDeclaredFields(Class<?> type){	
		List<Class<?>> listClasses = getSuperClasses(type);
		
		ArrayList<Field> result = new ArrayList<Field>();
		for(Class<?> cl : listClasses){
			for (Field f : cl.getDeclaredFields()){
				result.add(f);
			}
		}
		
		return result;
	}
	
	public static void setFieldObject(Object target, Field targetField, Object value) throws IllegalArgumentException, IllegalAccessException{
		boolean previousAccessiblValue = targetField.isAccessible();
		targetField.setAccessible(true);
		targetField.set(target, value);
		targetField.setAccessible(previousAccessiblValue);
	}

	private static List<Class<?>> getSuperClasses(Class<?> type) {
		List<Class<?> > listClasses = new ArrayList<Class<?> >();
		while(type != Object.class){
			listClasses.add(type);
			type = type.getSuperclass();
		}
		listClasses.add(Object.class);
		return listClasses;
	}
	
	public static Object getFieldObject(Object target, Field targetField) throws IllegalArgumentException, IllegalAccessException{
		boolean previousAccessiblValue = targetField.isAccessible();
		targetField.setAccessible(true);
		Object valueObject = targetField.get(target);
		targetField.setAccessible(previousAccessiblValue);
		
		return valueObject;
	}
	
	
}
