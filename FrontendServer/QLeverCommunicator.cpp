#include "QLeverCommunicator.h"

#include <sstream>

#include "../picojson/picojson.h"

// __________________________________________________________________________________
QLeverCommunicator::QLeverCommunicator(const std::string& serverAddress, unsigned int port)
    : _port(port), _serverAddress(serverAddress), _client([](const std::string& s) {std::cout << s << std::endl;}) {
    _client.InitSession();
}

// _________________________________________________________________________
QLeverCommunicator::~QLeverCommunicator() {
  _client.CleanupSession();
}

// _________________________________________________________________
std::string QLeverCommunicator::getRawQLeverResponse(const std::string& query) {
  long statusCode;
  std::string res;
  std::string request ="http://" + _serverAddress + ":" + std::to_string(_port);  
  request += "/?query=" + query;
  _client.GetText(request, res, statusCode);
  return statusCode == 200 ? res : std::string("");
}

// __________________________________________________________________
QLeverResult QLeverCommunicator::parseJSON(const std::string& json) {
  QLeverResult res;
  res.status = "Invalid Json returned from QLever";
  picojson::value v;
  std::stringstream stream(json);
  stream >> v;
  if (! v.is<picojson::object>()) {
    return res;
  }
  auto m = v.get<picojson::object>();
  const auto& it = m.find("res");
  if (it == m.end()) return res;

  if (! it->second.is<picojson::array>()) return res;
  const auto& r = it->second.get<picojson::array>();

  // r is now the vector which hold one result vector each
  for (const auto& el : r) {
    if (! el.is<picojson::array>()) return res;
    auto single = el.get<picojson::array>();
    res.res.emplace_back();
    for (const auto& wdNameV : single) {
      if (!wdNameV.is<std::string>()) return res;
      const auto& wdName = wdNameV.get<std::string>();
      res.res.back().push_back(wdName);
    }
  }
  return res;
}


// ____________________________________________________________________
QLeverResult QLeverCommunicator::GetQueryResult(const std::string& query) {
  return parseJSON(getRawQLeverResponse(query));
}
