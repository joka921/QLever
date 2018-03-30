#include <string>
#include <iostream>
#include <locale>
#include <codecvt>

std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>> converter;
std::string narrow = converter.to_bytes(wide_utf16_source_string);
std::wstring wide = converter.from_bytes(narrow_utf8_source_string);

int main(void) {
  std::string s = "äóß";
  std::wstring ws("äóß");

  std::cout << s <<" "<< s.length() << std::endl;
  std::cout << ws.c_str() <<" "<< ws.length() << std::endl;
}
 

