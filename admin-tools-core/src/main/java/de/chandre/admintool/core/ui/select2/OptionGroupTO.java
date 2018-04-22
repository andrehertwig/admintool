package de.chandre.admintool.core.ui.select2;

import java.util.ArrayList;
import java.util.List;

public class OptionGroupTO extends OptionTO {
	private static final long serialVersionUID = 2324648170741736932L;
	
	private String text;
	private List<OptionTO> children = new ArrayList<>();;
	
	public OptionGroupTO() {
		super();
	}
	public OptionGroupTO(String text) {
		super();
		this.text = text;
	}
	
	public String getText() {
		return text;
	}
	public void setText(String text) {
		this.text = text;
	}
	public List<OptionTO> getChildren() {
		return children;
	}
	public void setChildren(List<OptionTO> children) {
		this.children = children;
	}
	public void addChild(OptionTO child) {
		this.children.add(child);
	}
	
	public boolean hasChildren() {
		return this.children != null && !this.children.isEmpty();
	}
}
