package de.chandre.admintool;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.jmx.export.annotation.ManagedAttribute;
import org.springframework.jmx.export.annotation.ManagedOperation;
import org.springframework.jmx.export.annotation.ManagedResource;
import org.springframework.stereotype.Component;

@Component
@ConfigurationProperties(prefix = "at.example", ignoreUnknownFields=true)
@ManagedResource(objectName = "de.chandre.admintool:type=Config,name=Example2Configuration", description = "Configuration Example for JMX Browser")
public class ExampleMXBean2 {
	
	private String simpleString;
	
	private Map<String, String> simpleMap = new HashMap<>();
	
	private List<String> simpleList = new ArrayList<>();

	@ManagedAttribute(description="Description for Simple String getter")
	public String getSimpleString() {
		return simpleString;
	}

	@ManagedAttribute(description="Description for Simple String getter")
	public void setSimpleString(String simpleString) {
		this.simpleString = simpleString;
	}

	@ManagedAttribute(description="Description for Simple Map getter")
	public Map<String, String> getSimpleMap() {
		return simpleMap;
	}

	@ManagedOperation(description="Description for Simple Map setter")
	public void setSimpleMap(Map<String, String> simpleMap) {
		this.simpleMap = simpleMap;
	}

	@ManagedAttribute(description="Description for Simple List getter")
	public List<String> getSimpleList() {
		return simpleList;
	}

	@ManagedOperation(description="Description for Simple List setter")
	public void setSimpleList(List<String> simpleList) {
		this.simpleList = simpleList;
	}
	
	
}
