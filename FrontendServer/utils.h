// Copyright 2016 Johannes Kalmbach
// Authors: <johannes.kalmbach@gmail.com>
//
// Simple helper files for a http server

#ifndef JK544_SSERVER_UTILS_H
#define JK544_SSERVER_UTILS_H

#include <string>
#include <fstream>
#include <iostream>

// Decode URLS from URL-Encoding to valid strings
std::string decodeURL(std::string encoded) {
  std::string output = "";
  auto curPos = encoded.find("%");
  output.append(encoded.substr(0, curPos));
  while (curPos != encoded.npos) {
    // we have found another "%", decode
    int nextByte = 0;
    // two chars after "%" are hex string of byte
    std::string nextByteStr = encoded.substr(curPos + 1, 2);
    // we have to go through an int, or the results will be wrong
    std::istringstream(nextByteStr) >> std::hex >> nextByte;
    output.push_back(static_cast<char>(nextByte));
    auto posStart = curPos + 3;
    curPos = encoded.find("%", posStart);
    output.append(encoded.substr(posStart, curPos - posStart));
  }

  // decode "+" to " "
  curPos = 0;
  while (true) {
    curPos = output.find("+", curPos);
    if (curPos == output.npos) break;
    output = output.replace(curPos, 1, " ");
  }
  return output;
}


// read the file specified by filename arg and return a
// pair<bool, string> with
// <successful?, contentsOfFile>
std::pair<bool, std::string> readFile(std::string filename) {
  std::ifstream infile(filename.c_str());
  if (!infile.is_open()) {
    return std::make_pair(false, std::string(
        "The requested file does not exist"));
  } else {
    std::stringstream lineStream;
    lineStream << infile.rdbuf();
    return std::make_pair(true, lineStream.str());
  }
}


// detect the file ending of the given file name and return the appropriate
// MIME type. only html, css and js are correct.
// Returns: pair<bool, string> with <successful?, MIME-type>
std::pair<bool, std::string> detectContentType(std::string filename) {
  // for raghu, search for the end to also parse
  // a.b.c.d.e.f.g.txt
  auto posDot = filename.find_last_of(".");
  bool reqValid = true;
  std::string contentType = "text/html";
  if (posDot == filename.npos) {
    // invalid file without extension
    reqValid = false;
  } else {
    auto suffix = filename.substr(posDot, filename.length() - posDot);
    std::cout << "suffix is " << suffix << std::endl;
    if (suffix == std::string(".txt")) {
      contentType = "text/plain";
    } else if (suffix == std::string(".html")
        || suffix == std::string(".htm")) {
      contentType = "text/html";
    }  else if (suffix == std::string(".css")) {
      contentType = "text/css";
    }  else if (suffix == std::string(".js")) {
      contentType = "application/javascript";
    } else {
      std::cout << "no valid extension" << std::endl;
      reqValid = false;
    }
  }
  return std::make_pair(reqValid, contentType);
}

#endif  // JK544_SSERVER_UTILS_H
