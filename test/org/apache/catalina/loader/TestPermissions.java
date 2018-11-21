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
package org.apache.catalina.loader;

import static org.apache.catalina.startup.SimpleHttpClient.CRLF;
import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.net.URL;
import java.security.Permission;
import java.util.PropertyPermission;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.catalina.Context;
import org.apache.catalina.Globals;
import org.apache.catalina.startup.SimpleHttpClient;
import org.apache.catalina.startup.Tomcat;
import org.apache.catalina.startup.TomcatBaseTest;
import org.apache.tomcat.util.security.PermissionCheck;
import org.junit.Test;

public class TestPermissions extends TomcatBaseTest {
	
    @Override
	public void setUp() throws Exception {
        final URL policyURL = getClass().getResource("/conf/mypolicy.policy");
        System.setProperty("java.security.policy", policyURL.toString());
        System.setSecurityManager(new SecurityManager());

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
			
			final PermissionCheck permissionCheck = PermissionCheck.class.cast(cl);
			final Permission p = new PropertyPermission("java.home", "read");
			
			boolean permitted = permissionCheck.check(p);
			
			resp.getWriter().print(Boolean.toString(permitted));
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
}
