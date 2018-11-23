package de.chandre.admintool.security.dbuser.contoller;

import java.util.Collections;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import de.chandre.admintool.core.ui.select2.OptionGroupTO;
import de.chandre.admintool.core.ui.select2.OptionTO;
import de.chandre.admintool.core.ui.select2.Select2GroupedTO;
import de.chandre.admintool.security.dbuser.domain.AccessRelation;

/**
 * 
 * @author André
 * @since 1.2.0
 *
 */
@Service
public class AdminToolSecDBTransformUtil {
	
	
	public Select2GroupedTO<OptionTO> transformAccessRelationToSelect2(List<? extends AccessRelation> accessRelation) {
		Select2GroupedTO<OptionTO> response = new Select2GroupedTO<>();
		OptionGroupTO optionActive = new OptionGroupTO("Active Groups");
		OptionGroupTO optionInactive = new OptionGroupTO("Inactive Groups");
		accessRelation.stream().forEach(accessRel -> {
			OptionTO option = new OptionTO(accessRel.getName(), accessRel.getDisplayName());
			if(accessRel.isActive()) {
				optionActive.addChild(option);
			} else {
				optionInactive.addChild(option);
			}
		});
		
		if (!CollectionUtils.isEmpty(optionActive.getChildren())) {
			Collections.sort(optionActive.getChildren());
		}
		if (!CollectionUtils.isEmpty(optionInactive.getChildren())) {
			Collections.sort(optionInactive.getChildren());
		}
		
		if (optionActive.hasChildren() && optionInactive.hasChildren()) {
			response.addResult(optionActive);
			response.addResult(optionInactive);
		} else {
			response.setResult(optionActive.getChildren());
			response.getResult().addAll(optionInactive.getChildren());
		}
		return response;
	}
}
