// Copyright 2025 The QLever Authors, in particular:
//
// 2025 Johannes Kalmbach <kalmbach@cs.uni-freiburg.de>, UFR
//
// UFR = University of Freiburg, Chair of Algorithms and Data Structures

#include "libqlever/IndexManager.h"

#include <iostream>
#include <memory>

#include "libqlever/Qlever.h"
#include "util/Log.h"
#include "util/NullStream.h"

namespace qlever {

std::unique_ptr<Qlever> buildAndRunQleverIndex(
    const std::string& indexBasename, const IndexBuilderConfig& config) {
  // Build index for the given input file and write index files to disk.
  std::cout << "\x1b[1mBuilding index"
            << " with basename \"" << indexBasename << "\"\x1b[0m" << std::endl;

  setRuntimeParameter<&RuntimeParameters::stripColumns_>(true);
  try {
    qlever::Qlever::buildIndex(config);
  } catch (const std::exception& e) {
    std::cerr << "Building the index failed: " << e.what() << std::endl;
    throw;
  }

  // Load index.
  std::cout << "\x1b[1mLoading index with basename \"" << indexBasename
            << "\"\x1b[0m" << std::endl;
  qlever::EngineConfig engineConfig{config};
  auto qlever = std::make_unique<Qlever>(engineConfig);
  std::cout << std::endl;

  // Suppress QLever internal logging
  static ad_utility::NullStream nullStream;
  ad_utility::setGlobalLoggingStream(&nullStream);

  return qlever;
}

}  // namespace qlever
