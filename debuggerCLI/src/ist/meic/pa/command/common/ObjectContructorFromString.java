package ist.meic.pa.command.common;

import java.beans.PropertyEditor;
import java.beans.PropertyEditorManager;

public final class ObjectContructorFromString {

	public Object convert(Class<?> targetType, String text) {
	    PropertyEditor editor = PropertyEditorManager.findEditor(targetType);
	    editor.setAsText(text);
	    return editor.getValue();
	}
}
