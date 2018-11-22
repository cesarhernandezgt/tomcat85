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
package org.apache.catalina.startup;

import java.io.File;

import org.junit.Assert;
import org.junit.Test;
import org.apache.catalina.Context;
import org.apache.tomcat.util.buf.ByteChunk;

public class TestSecurityAnnotations extends TomcatBaseTest {

	@Test
    public void test() throws Exception{

        Tomcat tomcat = getTomcatInstance();
        File appDir = new File("test/webapp-3.1-plain");
        Context context =
                tomcat.addWebapp(null, "/test", appDir.getAbsolutePath());
        
        Tomcat.addServlet(context, "test1", "org.apache.catalina.startup.DenyServlet");
        Tomcat.addServlet(context, "test2", "org.apache.catalina.startup.TesterPlainServlet");
        
        context.addServletMapping("/test/*", "test1");
        context.addServletMapping("/test/a", "test2");

        tomcat.start();

        ByteChunk res = new ByteChunk();
        int sc = getUrl("http://localhost:" + getPort() + "/test/test/a", res, null);

        Assert.assertEquals(403, sc);
    }

}
