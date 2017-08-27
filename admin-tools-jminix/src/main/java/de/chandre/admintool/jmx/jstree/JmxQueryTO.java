package de.chandre.admintool.jmx.jstree;

import java.io.Serializable;

public class JmxQueryTO implements Serializable {
	private static final long serialVersionUID = -4564570953322072825L;
	
	private String server;
	private String domain;
	private String mbean;
	private String type;
	private String name;
	
	private String value;

	public String getServer() {
		return server;
	}

	public void setServer(String server) {
		this.server = server;
	}

	public String getDomain() {
		return domain;
	}

	public void setDomain(String domain) {
		this.domain = domain;
	}

	public String getMbean() {
		return mbean;
	}

	public void setMbean(String mbean) {
		this.mbean = mbean;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("JmxQueryTO [server=").append(server).append(", domain=").append(domain).append(", mbean=")
				.append(mbean).append(", type=").append(type).append(", name=").append(name).append(", value=")
				.append(value).append("]");
		return builder.toString();
	}
}
