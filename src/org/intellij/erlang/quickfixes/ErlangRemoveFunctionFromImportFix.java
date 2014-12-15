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
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.intellij.erlang.ErlangTypes;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

public class ErlangRemoveFunctionFromImportFix  extends ErlangQuickFixBase {
  @NotNull
  @Override
  public String getFamilyName() {
    return "Remove from import";
  }

  @Override
  public void applyFix(@NotNull Project project, @NotNull ProblemDescriptor descriptor) {
    ErlangFunction function = PsiTreeUtil.getParentOfType(descriptor.getPsiElement(), ErlangFunction.class);
    if (function == null) return;
    String fullName = ErlangPsiImplUtil.createFunctionPresentation(function);
    for (ErlangImportFunction importFunction : ((ErlangFile)function.getContainingFile()).getImportedFunctions()) {
      removeFunctionFromImport(PsiTreeUtil.getParentOfType(importFunction, ErlangAttribute.class), fullName);
    }
  }

  public static void removeFunctionFromImport(@Nullable ErlangAttribute importAttribute, @Nullable String name) {
    ErlangImportDirective importDirective = importAttribute != null ? importAttribute.getImportDirective() : null;
    ErlangImportFunctions fns = importDirective != null && importDirective.getModuleRef() != null ? importDirective.getImportFunctions() : null;
    List<ErlangImportFunction> functions = fns == null ? ContainerUtil.<ErlangImportFunction>emptyList() : fns.getImportFunctionList();
    if (name == null || functions.isEmpty()) return;

    for (int i = 0; i < functions.size(); ++i) {
      String presentation = ErlangPsiImplUtil.createFunctionPresentation(functions.get(i));
      if (presentation != null && presentation.equals(name)) {
        cutFunction(functions.get(i), i == functions.size() - 1);
      }
    }
    //noinspection unchecked
    if (PsiTreeUtil.getChildOfAnyType(fns, ErlangImportFunction.class, PsiComment.class) == null) {
      //noinspection ConstantConditions
      if (importAttribute.getNextSibling() instanceof PsiWhiteSpace) {
        importAttribute.getNextSibling().delete();
      }
      importAttribute.delete();
    }
  }

  private static void cutFunction(@NotNull ErlangImportFunction function, boolean isLast) {
    PsiElement sibling = function.getNextSibling();
    while (sibling != null) {
      if (sibling.getNode().getElementType() == ErlangTypes.ERL_COMMA) {
        sibling.delete();
        break;
      }
      if (!(sibling instanceof PsiComment || sibling instanceof PsiWhiteSpace)) break;
      sibling = sibling.getNextSibling();
    }
    if (isLast) {
      sibling = function.getPrevSibling();
      while (sibling != null) {
        if (sibling.getNode().getElementType() == ErlangTypes.ERL_COMMA) {
          sibling.delete();
          break;
        }
        if (!(sibling instanceof PsiComment || sibling instanceof PsiWhiteSpace)) break;
        sibling = sibling.getPrevSibling();
      }
    }
    function.delete();
  }
}
