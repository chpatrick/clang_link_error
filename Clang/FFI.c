
#include <stdlib.h>

#include "clang-c/Index.h"

#include "../cbits/util.h"

CXIndex inline_c_0_0743333ac0e2089afd635e6749e4d3ce11c9abae() {
return ( clang_createIndex(0, 1) );
}


CXTranslationUnit inline_c_1_8c5ad0c9c306ba27a3732907e151708d3bb115d2(CXIndex cidx_inline_c_0, char * cPath_inline_c_1, const char *const* cArgs_inline_c_2, long cArgs_inline_c_3) {
return (
        clang_parseTranslationUnit(
          cidx_inline_c_0,
          cPath_inline_c_1,
          cArgs_inline_c_2, cArgs_inline_c_3,
          NULL, 0,
          0)
        );
}


void inline_c_2_a2e45b1281c118d5d21c9c978fd99d4ab0bec709(CXCursor * cp_inline_c_0, CXTranslationUnit ctu_inline_c_1) {
 *cp_inline_c_0 = clang_getTranslationUnitCursor(ctu_inline_c_1); 
}


void inline_c_3_7b7fdbfad4a7e4caea937b89b1104e2f75f0425a(CXCursor * cp_inline_c_0, void (* visitChild_inline_c_1)(CXCursor *)) {

      clang_visitChildren(
        *cp_inline_c_0,
        visit_haskell,
        visitChild_inline_c_1)
    ;
}


const char * inline_c_4_c470689c35203c5a147da5ef95822986fccca25d(CXString * sp_inline_c_0) {
return ( clang_getCString(*sp_inline_c_0) );
}


void inline_c_5_41cf7dc6a82375063871f5de5d8b13a7085d4893(CXString * sp_inline_c_0) {
 clang_disposeString(*sp_inline_c_0) ;
}


void inline_c_6_0b4bcc6baa536a7828b15ba220027d404440ee37(CXString * sp_inline_c_0, CXCursor * cp_inline_c_1) {

    *sp_inline_c_0 = clang_getCursorSpelling(*cp_inline_c_1);
    
}

