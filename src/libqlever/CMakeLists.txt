add_library(qlever
  Qlever.cpp
  IdTableValueExtraction.cpp
  QueryTemplates.cpp
  InterfaceFilling.cpp
  IncrementalQueryExecutor.cpp
  IndexManager.cpp
)
qlever_target_link_libraries(qlever parser engine util index absl::strings)
