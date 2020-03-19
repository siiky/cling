(module
  cling
  (*program-name* *usage* arg cling help process-arguments usage)
  (import (only chicken.base include))
  (include "cling-impl.scm"))
