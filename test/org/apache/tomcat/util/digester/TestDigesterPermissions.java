/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.apache.tomcat.util.digester;

import static org.apache.catalina.startup.SimpleHttpClient.CRLF;
import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.net.URL;
import java.security.Permission;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.PropertyPermission;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.junit.Assert;
import org.junit.Test;
import org.xml.sax.InputSource;
import org.apache.catalina.Context;
import org.apache.catalina.Globals;
import org.apache.catalina.startup.SimpleHttpClient;
import org.apache.catalina.startup.Tomcat;
import org.apache.catalina.startup.TomcatBaseTest;
import org.apache.tomcat.util.descriptor.DigesterFactory;
import org.apache.tomcat.util.descriptor.XmlErrorHandler;
import org.apache.tomcat.util.descriptor.web.WebRuleSet;
import org.apache.tomcat.util.descriptor.web.WebXml;
import org.apache.tomcat.util.digester.Digester;
import org.apache.tomcat.util.security.PermissionCheck;

public class TestDigesterPermissions extends TomcatBaseTest {
	
    @Override
	public void setUp() throws Exception {
        final URL policyURL = getClass().getResource("/conf/mypolicy.policy");
        System.setProperty("test", "myvalue");
        System.setProperty("java.security.policy", policyURL.toString());
        System.setSecurityManager(new DenyPropertiesManager(Arrays.asList("test")));

		super.setUp();
	}

	@Test
    public void testSecurityPolicy() throws Exception {
        final Tomcat tomcat = getTomcatInstance();
        final Context ctx = tomcat.addContext("", null);

        Tomcat.addServlet(ctx, "test", new TestServlet());
        ctx.addServletMapping("/", "test");

        tomcat.start();
        
        TestClient client =
                new TestClient(tomcat.getConnector().getLocalPort());

        client.reset();
        client.setRequest(new String[] {
                "GET / HTTP/1.0" +CRLF + CRLF });
        client.connect();
        client.processRequest();
        assertEquals(Boolean.TRUE.toString(), client.getResponseBody());
    }
    
    private static class TestServlet extends HttpServlet {

		@Override
		protected void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException, IOException {
			
			final ClassLoader cl = Thread.currentThread().getContextClassLoader();
			if (! PermissionCheck.class.isInstance(cl)) {
				resp.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
			}
			
			if (! Globals.IS_SECURITY_ENABLED) {
				resp.getWriter().print(Boolean.FALSE.toString());
				return;
			}
			
			try {
				final URL url = getClass().getResource("/conf/web-subs.xml");
				
				XmlErrorHandler handler = new XmlErrorHandler();
		        Digester digester = DigesterFactory.newDigester(
		                true, true, new WebRuleSet(false), true);
		        digester.setErrorHandler(handler);
		        digester.push(new WebXml());
		        digester.setClassLoader(cl);
		        
		        
		        WebXml desc = (WebXml) digester.parse(new InputSource(url.openStream()));
		        Assert.assertEquals("3.1", desc.getVersion());
		        Assert.assertEquals(0, handler.getErrors().size());
		        Assert.assertEquals(0, handler.getWarnings().size());
		        
		        // no permission to inline this
		        Assert.assertEquals("${test}", desc.getEnvEntries().get("test").getValue());
		        
		        resp.getWriter().print(Boolean.TRUE.toString());
		        return;
			} catch (Exception e) {
				e.printStackTrace();
			}
			
			resp.getWriter().print(Boolean.FALSE.toString());
		}
    	
    }
    
    private static class TestClient extends SimpleHttpClient {

        public TestClient(int port) {
            setPort(port);
        }

        @Override
        public boolean isResponseBodyOK() {
            return Boolean.parseBoolean(getResponseBody());
        }
    }
    
    /* literally the world's worst security manager, but it'll stop the "test" property from being read */
    private static class DenyPropertiesManager extends SecurityManager {
    	protected final List<String> deniedProperties = new ArrayList<>();

		public DenyPropertiesManager(final Collection<String> propertiesToDeny) {
			super();
			this.deniedProperties.addAll(propertiesToDeny);
		}

		public void checkPermission(final Permission perm) {
			if (PropertyPermission.class.isInstance(perm)) {
				PropertyPermission propertyPermission = PropertyPermission.class.cast(perm);
				if (this.deniedProperties.contains(propertyPermission.getName())) {
					throw new SecurityException("Property " + propertyPermission.getName() + " explicitly denied");
				}
				
				return;
			}
		}
    }
}

