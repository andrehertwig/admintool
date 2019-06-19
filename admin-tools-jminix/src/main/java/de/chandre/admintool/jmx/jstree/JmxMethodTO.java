package de.chandre.admintool.jmx.jstree;

import java.io.Serializable;

/**
 * response object for jmx operation
 * @author André Hertwig
 * @since 1.1.6
 */
public class JmxMethodTO implements Serializable {
	private static final long serialVersionUID = 1984699051628661797L;
	
	private String name;
	private String type;
	private Object value;
	private String newValue;
	private String description;
	
	private String generalType;
	
	private Boolean notPrimitive;
	private boolean setToEmpty;
	private String typeInstance;
	
	public String getName() {
		return name;
	}
	public void setName(String name) {
		this.name = name;
	}
	public String getType() {
		return type;
	}
	public void setType(String type) {
		this.type = type;
	}
	public Object getValue() {
		return value;
	}
	public void setValue(Object value) {
		this.value = value;
	}
	public String getNewValue() {
		return newValue;
	}
	public void setNewValue(String newValue) {
		this.newValue = newValue;
	}
	public String getDescription() {
		return description;
	}
	public void setDescription(String description) {
		this.description = description;
	}
	public String getGeneralType() {
		return generalType;
	}
	public void setGeneralType(String generalType) {
		this.generalType = generalType;
	}
	
	public boolean isSetToEmpty() {
		return setToEmpty;
	}
	public void setSetToEmpty(boolean setToEmpty) {
		this.setToEmpty = setToEmpty;
	}
	public String getTypeInstance() {
		return typeInstance;
	}
	public void setTypeInstance(String typeInstance) {
		this.typeInstance = typeInstance;
	}
	
	public Boolean isNotPrimitive() {
		return notPrimitive;
	}
	public void setNotPrimitive(Boolean primitive) {
		this.notPrimitive = primitive;
	}
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("JmxMethodTO [name=").append(name).append(", type=").append(type).append(", value=")
				.append(value).append(", newValue=").append(newValue).append(", description=").append(description)
				.append(", generalType=").append(generalType).append(", notPrimitive=").append(notPrimitive)
				.append(", setToEmpty=").append(setToEmpty).append(", typeInstance=").append(typeInstance).append("]");
		return builder.toString();
	}
}
