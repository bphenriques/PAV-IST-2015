package ist.meic.pa.command.common;

import java.beans.PropertyEditor;
import java.beans.PropertyEditorManager;

/**
 * The Class ObjectContructorFromString converts a string into the an instance
 * of the desired class.
 */
public final class ObjectContructorFromString {

	/**
	 * Convert.
	 *
	 * @param targetType
	 *            the class of the object result
	 * @param text
	 *            the value in string form
	 * @return the value in the appropriate type
	 */
	public Object convert(Class<?> targetType, String text) {
		PropertyEditor editor = PropertyEditorManager.findEditor(targetType);
		editor.setAsText(text);
		return editor.getValue();
	}
}
