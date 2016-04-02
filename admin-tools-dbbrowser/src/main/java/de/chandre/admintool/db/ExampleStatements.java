package de.chandre.admintool.db;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * 
 * @author Andre
 *
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.PROPERTY)
public class ExampleStatements implements Serializable
{
	private static final long serialVersionUID = 1L;
	
	private String vendor;
	private Map<String, List<ExampleStatement>> clusters;
	
	
	public String getVendor() {
		return vendor;
	}

	public void setVendor(String vendor) {
		this.vendor = vendor;
	}

	public Map<String, List<ExampleStatement>> getClusters() {
		return clusters;
	}

	public void setClusters(Map<String, List<ExampleStatement>> clusters) {
		this.clusters = clusters;
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("ExampleStatements [vendor=").append(vendor).append(", clusters=").append(clusters).append("]");
		return builder.toString();
	}
}
