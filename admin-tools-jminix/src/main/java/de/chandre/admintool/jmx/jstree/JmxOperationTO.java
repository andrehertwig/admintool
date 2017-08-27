package de.chandre.admintool.jmx.jstree;

import java.util.ArrayList;
import java.util.List;

/**
 * response object for jmx operation
 * @author André Hertwig
 * @since 1.1.6
 */
public class JmxOperationTO extends JmxMethodTO {
	private static final long serialVersionUID = 8721380149439116761L;
	
	private List<JmxMethodTO> parameters;
	
	public List<JmxMethodTO> getParameters() {
		return parameters;
	}

	public void setParameters(List<JmxMethodTO> parameters) {
		this.parameters = parameters;
	}
	
	public void addParameter(JmxMethodTO parameter) {
		if(null == this.parameters) {
			this.parameters = new ArrayList<>();
		}
		this.parameters.add(parameter);
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("JmxOperationTO [parameters=").append(parameters).append(", toString()=")
				.append(super.toString()).append("]");
		return builder.toString();
	}
}
