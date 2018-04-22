package de.chandre.admintool.core.ui.select2;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class Select2GroupedTO<E extends OptionTO> implements Serializable {
	private static final long serialVersionUID = 922341647233302483L;
	
	private List<E> result = new ArrayList<>();;

	public List<E> getResult() {
		return result;
	}

	public void setResult(List<E> result) {
		this.result = result;
	}
	
	public boolean addResult(E option) {
		if (option instanceof OptionGroupTO) {
			if (OptionGroupTO.class.cast(option).hasChildren()) {
				return this.result.add(option);
			}
			return false;
		}
		return this.result.add(option);
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("Select2TO [result=").append(result).append("]");
		return builder.toString();
	}
	
	
}
