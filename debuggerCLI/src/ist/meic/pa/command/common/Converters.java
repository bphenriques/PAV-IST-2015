package ist.meic.pa.command.common;

import java.util.HashMap;
import java.util.Map;

public class Converters {

	private static final Map<Class<?>, Class<?>> WRAPPERS_TO_PRIMITIVES = new HashMap<Class<?>, Class<?>>();
	static{
		WRAPPERS_TO_PRIMITIVES.put(boolean.class, Boolean.class);
		WRAPPERS_TO_PRIMITIVES.put(byte.class, Byte.class);
		WRAPPERS_TO_PRIMITIVES.put(char.class, Character.class);
		WRAPPERS_TO_PRIMITIVES.put(double.class, Double.class);
		WRAPPERS_TO_PRIMITIVES.put(float.class, Float.class);
		WRAPPERS_TO_PRIMITIVES.put(int.class, Integer.class);
		WRAPPERS_TO_PRIMITIVES.put(long.class, Long.class);
		WRAPPERS_TO_PRIMITIVES.put(short.class, Short.class);
		WRAPPERS_TO_PRIMITIVES.put(void.class, Void.class);
	}

	private static final Map<Class<?>, Class<?>> PRIMITIVES_TO_WRAPPERS = new HashMap<Class<?>, Class<?>>();
	static{
		PRIMITIVES_TO_WRAPPERS.put(Boolean.class, boolean.class);
		PRIMITIVES_TO_WRAPPERS.put(Byte.class, byte.class);
		PRIMITIVES_TO_WRAPPERS.put(Character.class, char.class);
		PRIMITIVES_TO_WRAPPERS.put(Double.class, double.class);
		PRIMITIVES_TO_WRAPPERS.put(Float.class, float.class);
		PRIMITIVES_TO_WRAPPERS.put(Integer.class, int.class);
		PRIMITIVES_TO_WRAPPERS.put(Long.class, long.class);
		PRIMITIVES_TO_WRAPPERS.put(Short.class, short.class);
		PRIMITIVES_TO_WRAPPERS.put(Void.class, void.class);
	}
}
