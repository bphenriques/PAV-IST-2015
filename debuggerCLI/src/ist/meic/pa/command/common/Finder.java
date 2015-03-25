package ist.meic.pa.command.common;

import java.lang.reflect.Field;

public final class Finder {
	
	public static Field getField(Class<?> type, String name) throws NoSuchFieldException{
		try {
			return type.getDeclaredField(name);
		} catch (NoSuchFieldException e) {
			if (type == Object.class) {
				throw new NoSuchFieldException(name);
			} else {
				return getField(type.getSuperclass(), name);
			}
		} catch (SecurityException e) {
			e.printStackTrace();
		}
		return null;
	}
}
