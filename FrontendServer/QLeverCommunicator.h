#ifndef _QLEVER_COMMUNICATOR_H
#define _QLEVER_COMMUNICATOR_H
// Class which handles the communication from this Frontend Server to the QLever
// Backend.
//
#include "HTTPClient.h"
#include "QLeverResult.h"
class QLeverCommunicator {
  std::string _serverAddress;
  unsigned int _port;
  CHTTPClient _client;

  std::string getRawQLeverResponse(const std::string& query);
  QLeverResult parseJSON(const std::string& json);

  public:
    QLeverCommunicator(const std::string& serverAddress, unsigned int port);
    ~QLeverCommunicator();
    QLeverResult GetQueryResult(const std::string& query);
};

#endif  // _QLEVER_COMMUNICATOR_H
