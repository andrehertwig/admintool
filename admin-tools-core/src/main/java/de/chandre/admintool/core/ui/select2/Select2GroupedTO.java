package de.chandre.admintool.core.ui.select2;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import de.chandre.admintool.core.ui.ATError;

/**
 * View representation for select elements for select2 
 * @author Andre
 * @since 1.2.0
 *
 * @param <E> {@link OptionTO} or {@link OptionGroupTO}
 */
public class Select2GroupedTO<E extends OptionTO> implements Serializable {
	private static final long serialVersionUID = 922341647233302483L;
	
	private List<E> result = new ArrayList<>();
	
	private Set<ATError> errors;

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
	
	public Set<ATError> getErrors() {
		return errors;
	}

	public void setErrors(Set<ATError> errors) {
		this.errors = errors;
	}
	
	public void addError(ATError error) {
		if (null == errors) {
			this.errors = new HashSet<>();
		}
		this.errors.add(error);
	}

	@Override
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("Select2GroupedTO [result=").append(result).append(", errors=").append(errors).append("]");
		return builder.toString();
	}
	
}
