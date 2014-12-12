/*
 * Copyright 2012-2014 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.intellij.erlang.completion;

import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.testFramework.LightProjectDescriptor;
import com.intellij.testFramework.fixtures.DefaultLightProjectDescriptor;
import org.intellij.erlang.sdk.ErlangSdkRelease;
import org.intellij.erlang.sdk.ErlangSdkType;

public class ErlangCompletionWithSdkTest extends ErlangCompletionTestBase {
  @Override
  protected LightProjectDescriptor getProjectDescriptor() {
    return new DefaultLightProjectDescriptor() {
      @Override
      public Sdk getSdk() {
        return ErlangSdkType.createMockSdk("testData/mockSdk-R16B/", ErlangSdkRelease.V_R16B);
      }
    };
  }  

  public void testNoBifDuplicates1() throws Throwable {
    doCheckResult("bar() ->crc<caret>", "bar() ->erlang:crc32(<caret>)");
  }

  public void testNoBifDuplicates2() throws Throwable {
    doCheckResult("bar() ->eras<caret>", "bar() ->erase(<caret>)");
  }

  public void testCompleteFromSdk() throws Throwable {
    doCheckResult("bar() ->abs<caret>", "bar() ->abs(<caret>)");
  }

  public void test353() throws Throwable {
    doTestInclude("-record(aaa, {}). -record(bbb, {}). foo() -> is_record(1, <caret>)", "aaa", "bbb");
  }

  public void testSkdFunWithModule() throws Throwable {
    doTestEquals("abs(D) -> D. foo() -> ab<caret>", "abs", "erlang:abs", "atom_to_binary");
  }

  public void testAutoImportCompletion1() throws Throwable {
    doCheckResult("bar() ->erlabs<caret>", "bar() ->abs(<caret>)");
  }

  public void testAutoImportCompletion2() throws Throwable {
    doCheckResult("bar() ->e:abs<caret>", "bar() ->erlang:abs(<caret>)");
  }


  public void testNoAutoImportCompletion() throws Throwable {
    doCheckResult("bar() ->dt_get_<caret>", "bar() ->erlang:dt_get_tag()<caret>");
  }

  public void test353_2() throws Throwable {
    doTestInclude("-record(aaa, {}). -record(bbb, {}). foo() -> is_record(1,<caret>", "aaa", "bbb");
  }
}