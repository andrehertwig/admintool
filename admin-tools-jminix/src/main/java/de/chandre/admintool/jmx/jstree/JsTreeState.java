package de.chandre.admintool.jmx.jstree;

import java.io.Serializable;

public class JsTreeState  implements Serializable {
	private static final long serialVersionUID = 2211821322624575308L;
	
	// is the node open
	private boolean opened;
	// is the node disabled
	private boolean disabled;
	// is the node selected
	private boolean selected;
	
	public JsTreeState() {
	}
	
	public JsTreeState(boolean opened, boolean disabled, boolean selected) {
		super();
		this.opened = opened;
		this.disabled = disabled;
		this.selected = selected;
	}

	public boolean isOpened() {
		return opened;
	}
	public void setOpened(boolean opened) {
		this.opened = opened;
	}
	public boolean isDisabled() {
		return disabled;
	}
	public void setDisabled(boolean disabled) {
		this.disabled = disabled;
	}
	public boolean isSelected() {
		return selected;
	}
	public void setSelected(boolean selected) {
		this.selected = selected;
	}
}
