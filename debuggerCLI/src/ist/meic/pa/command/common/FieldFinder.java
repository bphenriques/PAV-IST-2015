package ist.meic.pa.command.common;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;

/**
 * A common library for finding
 * accessible fields, methods
 * and other information in a class,
 * even if they're in their superclasses
 */
public final class FieldFinder {

	/**
	 * Gets the declared field.
	 *
	 * @param type the class to search in
	 * @param name the name of the field
	 * @return the declared field
	 * @throws NoSuchFieldException when the field isn't found
	 * @throws SecurityException when the field is unaccessible
	 */
	public static Field getDeclaredField(Class<?> type, String name)
			throws NoSuchFieldException, SecurityException {

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

	/**
	 * Gets the declared method.
	 *
	 * @param type the class to search in
	 * @param name the name of the method
	 * @param params the classes of the parameters, in order
	 * @return the declared method
	 * @throws SecurityException when the method is unaccessible
	 * @throws NoSuchMethodException when the method isn't found
	 */
	public static Method getDeclaredMethod(Class<?> type, String name,
			Class<?>[] params) throws SecurityException, NoSuchMethodException {

		try {
			return type.getDeclaredMethod(name, params);
		} catch (NoSuchMethodException e) {
			if (type == Object.class) {
				throw new NoSuchMethodException();
			} else {
				return getDeclaredMethod(type.getSuperclass(), name, params);
			}
		}
	}

	/**
	 * Gets the declared fields.
	 *
	 * @param type the class to search in
	 * @return the declared fields
	 */
	public static List<Field> getDeclaredFields(Class<?> type) {
		List<Class<?>> listClasses = getSuperClasses(type);

		ArrayList<Field> result = new ArrayList<Field>();
		for (Class<?> cl : listClasses) {
			for (Field f : cl.getDeclaredFields()) {
				result.add(f);
			}
		}

		return result;
	}

	/**
	 * Gets the declared methods.
	 *
	 * @param type the class to search in
	 * @return the declared methods
	 */
	public static List<Method> getDeclaredMethods(Class<?> type) {
		List<Class<?>> listClasses = getSuperClasses(type);
		ArrayList<Method> result = new ArrayList<Method>();

		for (Class<?> c : listClasses) {
			for (Method m : c.getDeclaredMethods()) {
				result.add(m);
			}
		}

		return result;
	}

	/**
	 * Sets the field object.
	 *
	 * @param target the instance to set the field in
	 * @param targetField the target field
	 * @param value the value to set in the field
	 * @throws IllegalArgumentException if the object provided as the value isn't assignable to the field requested
	 * @throws IllegalAccessException if the field in unaccessible
	 */
	public static void setFieldObject(Object target, Field targetField,
			Object value) throws IllegalArgumentException,
			IllegalAccessException {
		boolean previousAccessiblValue = targetField.isAccessible();
		targetField.setAccessible(true);
		targetField.set(target, value);
		targetField.setAccessible(previousAccessiblValue);
	}

	/**
	 * Gets the super classes.
	 *
	 * @param type the class to search in
	 * @return the super classes
	 */
	private static List<Class<?>> getSuperClasses(Class<?> type) {
		List<Class<?>> listClasses = new ArrayList<Class<?>>();
		while (type != Object.class) {
			listClasses.add(type);
			type = type.getSuperclass();
		}
		
		listClasses.add(Object.class);
		return listClasses;
	}

	/**
	 * Gets the current value of the field.
	 *
	 * @param target the class to search in
	 * @param targetField the target field
	 * @return the field object
	 * @throws IllegalArgumentException  if the specified object is not an instance of the class or interface declaring the underlying field
	 * @throws IllegalAccessException if the field in unaccessible
	 */
	public static Object getFieldObject(Object target, Field targetField)
			throws IllegalArgumentException, IllegalAccessException {
		boolean previousAccessiblValue = targetField.isAccessible();
		targetField.setAccessible(true);
		Object valueObject = targetField.get(target);
		targetField.setAccessible(previousAccessiblValue);

		return valueObject;
	}

}
