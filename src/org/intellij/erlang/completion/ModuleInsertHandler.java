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

import com.intellij.codeInsight.AutoPopupController;
import com.intellij.codeInsight.completion.InsertionContext;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import org.intellij.erlang.psi.impl.ErlangElementFactory;

public class ModuleInsertHandler extends SingleCharInsertHandler {
  public static final String QUOTA = "'";
  private final Project myProject;
  private final String myModuleName;
  private final boolean myWithColon;

  public ModuleInsertHandler(Project project, String moduleName, boolean withColon) {
    super(':');
    myProject = project;
    myModuleName = moduleName;
    myWithColon = withColon;
  }

  @Override
  public void handleInsert(InsertionContext context, LookupElement item) {
    if (needQuotation()) {
      Editor editor = context.getEditor();
      Document document = editor.getDocument();
      context.commitDocument();
      int tailOffset = context.getTailOffset();
      int startOffset = context.getStartOffset();

      document.insertString(startOffset, QUOTA);
      document.insertString(tailOffset + 1, QUOTA);
      editor.getCaretModel().moveToOffset(tailOffset + 2);
      context.setTailOffset(tailOffset + 2);
    }
    if (myWithColon) {
      AutoPopupController.getInstance(context.getProject()).autoPopupMemberLookup(context.getEditor(), null);
      super.handleInsert(context, item);
    }
  }

  private boolean needQuotation() {
    try {
      ErlangElementFactory.createQAtomFromText(myProject, myModuleName);
      return false;
    } catch (Exception ignored) {
    }
    return true;
  }
}
