package de.chandre.admintool.db;

import java.io.Serializable;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

/**
 * AdminTool dbbrowser executable statement transfer object
 * @author Andre
 *
 */
@XmlRootElement
@XmlAccessorType(XmlAccessType.PROPERTY)
public class StatementTO implements Serializable
{
	private static final long serialVersionUID = 1L;

	private int tab;
	private String datasourceName;
	
	private String clobEncoding;
	private boolean showClobs;
	private boolean showBlobs;
	private boolean baseEncoded;
	
	private int maxResults;
	
	private String statement;

	
	/**
	 * @return the tab
	 */
	public int getTab() {
		return tab;
	}

	/**
	 * @param tab the tab to set
	 */
	public void setTab(int tab) {
		this.tab = tab;
	}

	/**
	 * @return the datasourceName
	 */
	public String getDatasourceName() {
		return datasourceName;
	}

	/**
	 * @param datasourceName the datasourceName to set
	 */
	public void setDatasourceName(String datasourceName) {
		this.datasourceName = datasourceName;
	}

	/**
	 * @return the clobEncoding
	 */
	public String getClobEncoding() {
		return clobEncoding;
	}

	/**
	 * @param clobEncoding the clobEncoding to set
	 */
	public void setClobEncoding(String clobEncoding) {
		this.clobEncoding = clobEncoding;
	}

	/**
	 * @return the showClobs
	 */
	public boolean isShowClobs() {
		return showClobs;
	}

	/**
	 * @param showClobs the showClobs to set
	 */
	public void setShowClobs(boolean showClobs) {
		this.showClobs = showClobs;
	}

	/**
	 * @return the showBlobs
	 */
	public boolean isShowBlobs() {
		return showBlobs;
	}

	/**
	 * @param showBlobs the showBlobs to set
	 */
	public void setShowBlobs(boolean showBlobs) {
		this.showBlobs = showBlobs;
	}

	/**
	 * @return the baseEncoded
	 */
	public boolean isBaseEncoded() {
		return baseEncoded;
	}

	/**
	 * @param baseEncoded the baseEncoded to set
	 */
	public void setBaseEncoded(boolean baseEncoded) {
		this.baseEncoded = baseEncoded;
	}

	/**
	 * @return the maxResults
	 */
	public int getMaxResults() {
		return maxResults;
	}

	/**
	 * @param maxResults the maxResults to set
	 */
	public void setMaxResults(int maxResults) {
		this.maxResults = maxResults;
	}

	/**
	 * @return the statement
	 */
	public String getStatement() {
		return statement;
	}

	/**
	 * @param statement the statement to set
	 */
	public void setStatement(String statement) {
		this.statement = statement;
	}
	
	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("StatementTO [tab=").append(tab).append(", datasourceName=").append(datasourceName)
				.append(", clobEncoding=").append(clobEncoding).append(", showClobs=").append(showClobs)
				.append(", showBlobs=").append(showBlobs).append(", baseEncoded=").append(baseEncoded)
				.append(", maxResults=").append(maxResults).append(", statement=").append(statement).append("]");
		return builder.toString();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (baseEncoded ? 1231 : 1237);
		result = prime * result + ((clobEncoding == null) ? 0 : clobEncoding.hashCode());
		result = prime * result + ((datasourceName == null) ? 0 : datasourceName.hashCode());
		result = prime * result + maxResults;
		result = prime * result + (showBlobs ? 1231 : 1237);
		result = prime * result + (showClobs ? 1231 : 1237);
		result = prime * result + ((statement == null) ? 0 : statement.hashCode());
		result = prime * result + tab;
		return result;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		StatementTO other = (StatementTO) obj;
		if (baseEncoded != other.baseEncoded)
			return false;
		if (clobEncoding == null) {
			if (other.clobEncoding != null)
				return false;
		} else if (!clobEncoding.equals(other.clobEncoding))
			return false;
		if (datasourceName == null) {
			if (other.datasourceName != null)
				return false;
		} else if (!datasourceName.equals(other.datasourceName))
			return false;
		if (maxResults != other.maxResults)
			return false;
		if (showBlobs != other.showBlobs)
			return false;
		if (showClobs != other.showClobs)
			return false;
		if (statement == null) {
			if (other.statement != null)
				return false;
		} else if (!statement.equals(other.statement))
			return false;
		if (tab != other.tab)
			return false;
		return true;
	}
}
