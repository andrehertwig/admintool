package de.chandre.admintool;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.ModelMap;
import org.springframework.web.bind.annotation.RequestMapping;

import de.chandre.admintool.core.AdminTool;

/**
 * Demo controller
 * @author Andre
 *
 */
@Controller
public class DemoController {
	
	@Autowired
	private AdminTool adminTool;
	
	@RequestMapping({"","/"})
	public String hello() {
		return "index";
	}
	
	@RequestMapping({"/info"})
	public String admintoolInfo(ModelMap model) {
		model.put("adminToolInfo", adminTool.toString());
		return "info";
	}
	
}
