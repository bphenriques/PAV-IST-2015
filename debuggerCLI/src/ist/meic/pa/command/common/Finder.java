package ist.meic.pa.command.common;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;

public final class Finder {
	
	public static Field getDeclaredField(Class<?> type, String name) throws NoSuchFieldException{
		try {
			return type.getDeclaredField(name);
		} catch (NoSuchFieldException e) {
			if (type == Object.class) {
				throw new NoSuchFieldException(name);
			} else {
				return getDeclaredField(type.getSuperclass(), name);
			}
		} catch (SecurityException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	public static List<Field> getDeclaredFields(Class<?> type){
		
		List<Class<?> > listClasses = new ArrayList<Class<?> >();
		while(type != Object.class){
			listClasses.add(type);
			type = type.getSuperclass();
		}
		listClasses.add(Object.class);
		
		
		ArrayList<Field> result = new ArrayList<Field>();
		for(Class<?> cl : listClasses){
			for (Field f : cl.getDeclaredFields()){
				result.add(f);
			}
		}
		
		return result;
	
	}
}
