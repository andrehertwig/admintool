package de.chandre.admintool.core;

import javax.annotation.PostConstruct;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service("adminToolInitializer")
public class AdminToolInitializer extends AbstractAdminToolLoader
{
	@Autowired
	private AdminTool adminTool;
	
	@PostConstruct
	public void init() {
		
		boolean relative = !shouldCDNsUsed();
		String adminLtePrefix = getAdminLTEPrefixUri();
		String commonPrefix = getWebjarsPrefixUri();

		adminTool.addGlobalStyleSheet(adminLtePrefix + "bootstrap/css/bootstrap.min.css", relative);
		adminTool.addGlobalStyleSheet(commonPrefix + "font-awesome/" + 
				coreConfig.getFontAwsomeCdnVersion() + "/css/font-awesome.min.css", relative);
		adminTool.addGlobalStyleSheet(commonPrefix + "ionicons/2.0.1/css/ionicons.min.css", relative);	
		
		adminTool.addGlobalJavaScript(adminLtePrefix + "plugins/jQuery/jQuery-2.2.0.min.js", relative);
		adminTool.addGlobalJavaScript(adminLtePrefix + "bootstrap/js/bootstrap.min.js", relative);
		adminTool.addGlobalJavaScript(adminLtePrefix + "dist/js/app.min.js", relative);
		adminTool.addGlobalStyleSheet(adminLtePrefix + "dist/css/AdminLTE.min.css", relative);
		adminTool.addGlobalStyleSheet(adminLtePrefix + "dist/css/skins/skin-blue.min.css", relative);
		
		adminTool.addGlobalJavaScript("/static/admintool/js/adminTool.js", true);
		adminTool.addGlobalStyleSheet("/static/admintool/css/adminTool.css", false);
		
		adminTool.addGlobalJavaScript(adminLtePrefix + "plugins/datepicker/bootstrap-datepicker.js", relative);
		adminTool.addGlobalStyleSheet(adminLtePrefix + "plugins/datepicker/datepicker3.css", relative);
		adminTool.addGlobalJavaScript(adminLtePrefix + "plugins/timepicker/bootstrap-timepicker.min.js", relative);
		adminTool.addGlobalStyleSheet(adminLtePrefix + "plugins/timepicker/bootstrap-timepicker.min.css", relative);
		
		adminTool.addGlobalJavaScript(adminLtePrefix + "plugins/iCheck/icheck.min.js", relative);
		adminTool.addGlobalStyleSheet(adminLtePrefix + "plugins/iCheck/minimal/minimal.css", relative);
		
		adminTool.addGlobalJavaScript(adminLtePrefix + "plugins/datatables/jquery.dataTables.min.js", relative);
		adminTool.addGlobalStyleSheet(adminLtePrefix + "plugins/datatables/jquery.dataTables.min.css", relative);
		
	}
}
