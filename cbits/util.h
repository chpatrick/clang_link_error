typedef void (*haskell_visitor)(CXCursor*);

// Traverse children using a haskell_visitor passed in as client_data.
static enum CXChildVisitResult visit_haskell(CXCursor cursor, CXCursor parent, CXClientData client_data) {
  ((haskell_visitor) client_data)(&cursor);
  return CXChildVisit_Continue;
};
