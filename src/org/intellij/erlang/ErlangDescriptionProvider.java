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

package org.intellij.erlang;

import com.intellij.codeInsight.highlighting.HighlightUsagesDescriptionLocation;
import com.intellij.psi.ElementDescriptionLocation;
import com.intellij.psi.ElementDescriptionProvider;
import com.intellij.psi.PsiElement;
import com.intellij.usageView.UsageViewLongNameLocation;
import com.intellij.usageView.UsageViewNodeTextLocation;
import com.intellij.usageView.UsageViewShortNameLocation;
import com.intellij.usageView.UsageViewTypeLocation;
import org.intellij.erlang.psi.*;
import org.intellij.erlang.psi.impl.ErlangPsiImplUtil;
import org.jetbrains.annotations.NotNull;

public class ErlangDescriptionProvider implements ElementDescriptionProvider {
  @Override
  public String getElementDescription(@NotNull PsiElement o, @NotNull ElementDescriptionLocation location) {
    if (location == UsageViewNodeTextLocation.INSTANCE && (o instanceof ErlangNamedElement || o instanceof ErlangQAtom)) {
      return getElementDescription(o, UsageViewTypeLocation.INSTANCE) + " " +
        "'" + getElementDescription(o, UsageViewShortNameLocation.INSTANCE) + "'";
    }
    if (location == UsageViewShortNameLocation.INSTANCE || location == UsageViewLongNameLocation.INSTANCE) {
      if (o instanceof ErlangNamedElement) return ((ErlangNamedElement) o).getName();
      if (o instanceof ErlangQAtom) return ErlangPsiImplUtil.getName((ErlangQAtom)o);
    }
    if (location == HighlightUsagesDescriptionLocation.INSTANCE) {
      String d = getElementDescription(o, UsageViewTypeLocation.INSTANCE);
      return d != null ? d.toLowerCase() : null;
    }
    if (location == UsageViewTypeLocation.INSTANCE) {
      if (o instanceof ErlangModule) return "Module";
      if (o instanceof ErlangFunction) return "Function";
      if (o instanceof ErlangRecordDefinition) return "Record";
      if (o instanceof ErlangQVar) return "Variable";
      if (o instanceof ErlangMacrosDefinition) return "Macros";
      if (o instanceof ErlangTypedExpr) return "Record field";
      if (o instanceof ErlangTypeDefinition) return "Type";
      if (o instanceof ErlangAttribute) return "Attribute";
      if (o instanceof ErlangQAtom) return "Atom";
    }
    return null;
  }
}
