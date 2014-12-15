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

package org.intellij.erlang.quickfixes;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.util.PsiTreeUtil;
import org.intellij.erlang.psi.ErlangAttribute;
import org.intellij.erlang.psi.ErlangFile;
import org.intellij.erlang.psi.ErlangImportFunction;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

public class ErlangRemoveImportFunctionFromImportFix extends ErlangRemoveFunctionFromImportFix {
  final boolean myOnlyCurrentImport;

  public ErlangRemoveImportFunctionFromImportFix(boolean onlyCurrentImport) {
    myOnlyCurrentImport = onlyCurrentImport;
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    ErlangImportFunction function = PsiTreeUtil.getParentOfType(descriptor.getPsiElement(), ErlangImportFunction.class, true);
    if (function == null) return;
    String fullName = ErlangPsiImplUtil.createFunctionPresentation(function);
    if (fullName == null) return;
    if (myOnlyCurrentImport) {
      ErlangAttribute currentImportAttribute = PsiTreeUtil.getParentOfType(function, ErlangAttribute.class);
      removeFunctionFromImport(currentImportAttribute, fullName);
      return;
    }
    for (ErlangImportFunction importFunction : ((ErlangFile)function.getContainingFile()).getImportedFunctions()) {
      removeFunctionFromImport(PsiTreeUtil.getParentOfType(importFunction, ErlangAttribute.class), fullName);
    }
  }
}
