package de.chandre.admintool.jmx.jstree;

import java.util.List;

public class JmxExecuteTO extends JmxQueryTO {
	private static final long serialVersionUID = -8678306366043106315L;
	
	private List<JmxMethodTO> parameters;

	public List<JmxMethodTO> getParameters() {
		return parameters;
	}

	public void setParameters(List<JmxMethodTO> parameters) {
		this.parameters = parameters;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("JmxExecuteTO [parameters=").append(parameters).append(", toString()=").append(super.toString())
				.append("]");
		return builder.toString();
	}
}
