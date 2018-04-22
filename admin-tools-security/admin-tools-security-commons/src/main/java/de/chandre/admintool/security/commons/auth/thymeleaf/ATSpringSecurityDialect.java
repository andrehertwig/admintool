/*
 * =============================================================================
 * 
 *   Copyright (c) 2011-2014, The THYMELEAF team (http://www.thymeleaf.org)
 * 
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 * 
 * =============================================================================
 */
package de.chandre.admintool.security.commons.auth.thymeleaf;

import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.security.core.Authentication;
import org.thymeleaf.context.IContext;
import org.thymeleaf.context.IProcessingContext;
import org.thymeleaf.context.IWebContext;
import org.thymeleaf.dialect.AbstractDialect;
import org.thymeleaf.dialect.IExpressionEnhancingDialect;
import org.thymeleaf.extras.springsecurity4.auth.AuthUtils;
import org.thymeleaf.extras.springsecurity4.auth.Authorization;
import org.thymeleaf.extras.springsecurity4.dialect.processor.AuthenticationAttrProcessor;
import org.thymeleaf.extras.springsecurity4.dialect.processor.AuthorizeAclAttrProcessor;
import org.thymeleaf.extras.springsecurity4.dialect.processor.AuthorizeAttrProcessor;
import org.thymeleaf.extras.springsecurity4.dialect.processor.AuthorizeUrlAttrProcessor;
import org.thymeleaf.processor.IProcessor;



/**
 * copied from Thymeleaf secuity to add own ATAuthUrlAttrProcessor
 * 
 * @author Daniel Fern&aacute;ndez
 *
 */
public class ATSpringSecurityDialect 
        extends AbstractDialect 
        implements IExpressionEnhancingDialect {

    public static final String DEFAULT_PREFIX = "atsec";
    
    public static final String AUTHENTICATION_EXPRESSION_OBJECT_NAME = "authentication";
    public static final String AUTHORIZATION_EXPRESSION_OBJECT_NAME = "authorization";
    
    
    public ATSpringSecurityDialect() {
        super();
    }
    
    public String getPrefix() {
        return DEFAULT_PREFIX;
    }

    
    public boolean isLenient() {
        return false;
    }
    
    @Override
    public Set<IProcessor> getProcessors() {
        final Set<IProcessor> processors = new LinkedHashSet<IProcessor>();
        processors.add(new ATAuthUrlAttrProcessor());
        return processors;
    }
    
    public Map<String, Object> getAdditionalExpressionObjects(
            final IProcessingContext processingContext) {
        
        final IContext context = processingContext.getContext();
        final IWebContext webContext =
                (context instanceof IWebContext? (IWebContext)context : null);
        
        final Map<String,Object> objects = new HashMap<String, Object>(3, 1.0f);
        
        /*
         * Create the #authentication and #authorization expression objects
         */
        if (webContext != null) {
            
            final HttpServletRequest request = webContext.getHttpServletRequest();
            final HttpServletResponse response = webContext.getHttpServletResponse();
            final ServletContext servletContext = webContext.getServletContext();
            
            if (request != null && response != null && servletContext != null) {
                
                final Authentication authentication = AuthUtils.getAuthenticationObject();
                final Authorization authorization = 
                        new Authorization(processingContext, authentication, request, response, servletContext); 
                        
                objects.put(AUTHENTICATION_EXPRESSION_OBJECT_NAME, authentication);
                objects.put(AUTHORIZATION_EXPRESSION_OBJECT_NAME, authorization);
                
            }
            
        }
       
        return objects;
        
    }
    
}
