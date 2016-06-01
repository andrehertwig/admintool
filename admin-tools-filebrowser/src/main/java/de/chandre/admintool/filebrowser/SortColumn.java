package de.chandre.admintool.filebrowser;

/**
 * 
 * @author Andre
 *
 */
public enum SortColumn {
	
	NAME(1),
	SIZE(2),
	TYPE(3),
	DATE(4);
	
	private int index;
	private SortColumn(int index) {
		this.index = index;
	}
	
	public static SortColumn fromIndex(String indexStr) {
		int index = -1;
		try {
			index = Integer.parseInt(indexStr);
		} catch (Exception ignore) {}
		for (SortColumn sortCol : values()) {
			if (sortCol.index == index) {
				return sortCol;
			}
		}
		return null;
	}
	
	public static SortColumn fromIndex(int index) {
		for (SortColumn sortCol : values()) {
			if (sortCol.index == index) {
				return sortCol;
			}
		}
		return null;
	}

	/**
	 * @return the index
	 */
	public int getIndex() {
		return index;
	}
	
}
