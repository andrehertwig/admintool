package de.chandre.admintool.jmx.jstree;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class JmxResponseTO implements Serializable {
	private static final long serialVersionUID = -6747620476709046420L;
	
	private List<JmxMethodTO> methods = new ArrayList<>();
	private Boolean success;

	public List<JmxMethodTO> getMethods() {
		return methods;
	}

	public void setMethods(List<JmxMethodTO> methods) {
		this.methods = methods;
	}
	
	public void addMethod(JmxMethodTO method) {
		this.methods.add(method);
	}

	public Boolean getSuccess() {
		return success;
	}

	public void setSuccess(Boolean success) {
		this.success = success;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("JmxResponseTO [methods=").append(methods).append(", success=").append(success).append("]");
		return builder.toString();
	}
}
