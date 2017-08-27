package de.chandre.admintool.jmx.jstree;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class JsTree implements Serializable {
	private static final long serialVersionUID = -3008103623566946897L;
	
	private String id;
	private String text;
	private String icon;
	private JsTreeState state;
	private List<JsTree> children = new ArrayList<>();
	private String type;
	
	public JsTree() {
	}
	public JsTree( String id, String text, String type) {
		this.id = id;
		this.text = text;
		this.type = type;
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

	public String getIcon() {
		return icon;
	}

	public void setIcon(String icon) {
		this.icon = icon;
	}

	public JsTreeState getState() {
		return state;
	}

	public void setState(JsTreeState state) {
		this.state = state;
	}

	public List<JsTree> getChildren() {
		return children;
	}

	public void setChildren(List<JsTree> children) {
		this.children = children;
	}
	
	public void addChildren(JsTree child) {
		this.children.add(child);
	}
	public String getType() {
		return type;
	}
	public void setType(String type) {
		this.type = type;
	}
	
}
