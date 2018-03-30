#ifndef _SERVER_UTILS_H
#define _SERVER_UTILS_H

#include <vector>

#include "WikidataEntity.h"

class ServerUtils {
 public:
  static std::string entitiesToJson(const std::vector<WikidataEntityShort>& entities, size_t num);
  static std::string escapeJson(const std::string& wordNarrow);
};

#endif  // _SERVER_UTILS_H
