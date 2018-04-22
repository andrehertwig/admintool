package de.chandre.admintool.core.ui.select2;

import java.io.Serializable;

public class OptionTO implements Serializable {
	private static final long serialVersionUID = 6804008394662197234L;
	
	private String id;
	private String text;
	private boolean selected;
	private boolean disabled;
	
	public OptionTO() {
		super();
	}
	
	public OptionTO(String id, String text) {
		this(id, text, false, false);
	}
	public OptionTO(String id, String text, boolean selected, boolean disabled) {
		super();
		this.id = id;
		this.text = text;
		this.selected = selected;
		this.disabled = disabled;
	}

	public String getId() {
		return id;
	}
	public void setId(String id) {
		this.id = id;
	}
	public String getText() {
		return text;
	}
	public void setText(String text) {
		this.text = text;
	}
	public boolean isSelected() {
		return selected;
	}
	public void setSelected(boolean selected) {
		this.selected = selected;
	}
	public boolean isDisabled() {
		return disabled;
	}
	public void setDisabled(boolean disabled) {
		this.disabled = disabled;
	}
	
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("OptionTO [id=").append(id).append(", text=").append(text).append(", selected=").append(selected)
				.append(", disabled=").append(disabled).append("]");
		return builder.toString();
	}
}
