package de.chandre.admintool.security.commons.auth.thymeleaf;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;

import org.springframework.security.core.Authentication;
import org.thymeleaf.Arguments;
import org.thymeleaf.Configuration;
import org.thymeleaf.context.IContext;
import org.thymeleaf.context.IWebContext;
import org.thymeleaf.dom.Element;
import org.thymeleaf.exceptions.ConfigurationException;
import org.thymeleaf.extras.springsecurity4.auth.AuthUtils;
import org.thymeleaf.processor.attr.AbstractConditionalVisibilityAttrProcessor;
import org.thymeleaf.standard.expression.IStandardExpression;
import org.thymeleaf.standard.expression.IStandardExpressionParser;
import org.thymeleaf.standard.expression.StandardExpressions;

public class ATAuthUrlAttrProcessor extends AbstractConditionalVisibilityAttrProcessor {
	
	public static final String ATTR_NAME = "authorize-url";
    
	@Override
	public int getPrecedence() {
		return 300;
	}
    
    public ATAuthUrlAttrProcessor() {
        super(ATTR_NAME);
    }

	@Override
	protected boolean isVisible(Arguments arguments, Element element, String attributeName) {

		String attributeValue = element.getAttributeValue(attributeName);

		final Configuration configuration = arguments.getConfiguration();
		final IStandardExpressionParser expressionParser = StandardExpressions.getExpressionParser(configuration);

		final IStandardExpression expression = expressionParser.parseExpression(configuration, arguments,
				attributeValue);
		final Object value = expression.execute(configuration, arguments);
		
		attributeValue = value.toString();

		if (attributeValue == null || attributeValue.trim().equals("")) {
            return false;
        }
        attributeValue = attributeValue.trim();
        
        final int spaceIndex = attributeValue.indexOf(' ');
        final String url = 
                (spaceIndex < 0? attributeValue : attributeValue.substring(spaceIndex + 1)).trim();
        final String method =
                (spaceIndex < 0? "GET" : attributeValue.substring(0, spaceIndex)).trim();

        final IContext context = arguments.getContext();
        if (!(context instanceof IWebContext)) {
            throw new ConfigurationException(
                    "Thymeleaf execution context is not a web context (implementation of " +
                    IWebContext.class.getName() + ". Spring Security integration can only be used in " +
                    "web environements.");
        }
        final IWebContext webContext = (IWebContext) context;
        
        final HttpServletRequest request = webContext.getHttpServletRequest();
        final ServletContext servletContext = webContext.getServletContext();
        
        final Authentication authentication = AuthUtils.getAuthenticationObject();

        if (authentication == null) {
            return false;
        }
        
        return AuthUtils.authorizeUsingUrlCheck(
                url, method, authentication, request, servletContext);
	}

}
