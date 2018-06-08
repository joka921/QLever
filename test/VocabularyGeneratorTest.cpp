#include <gtest/gtest.h>
#include <string>
#include <fstream>
#include <boost/filesystem.hpp>
#include <boost/iostreams/device/mapped_file.hpp>

#include "../src/index/VocabularyGenerator.h"
#include "../src/global/Constants.h"

// Test fixture that sets up the binary files vor partial vocabulary and
// everything else connected with vocabulary merging.
class MergeVocabularyTest : public ::testing::Test {
protected:

  // path of the 2 partial Vocabularies that are used by mergeVocabulary
  boost::filesystem::path _path0;
  boost::filesystem::path _path1;
  // path of the 2 partial vocabularies that are the expected output of
  // mergeVocabulary
  boost::filesystem::path _pathExp0;
  boost::filesystem::path _pathExp1;
  // the base directory for our test
  boost::filesystem::path _basePath;
  // path to expected vocabulary text file
  boost::filesystem::path _pathVocabExp;
  // path to expected external vocabulary text file
  boost::filesystem::path _pathExternalVocabExp;

  // Constructor. TODO: Better write Setup method because of complex logic which
  // may throw?
  MergeVocabularyTest() {
    // name of random subdirectory
    _basePath = boost::filesystem::unique_path("qleverVocTest%%%%_%%%%_%%%%_%%%%");
    // those names are required by mergeVocabulary
    _path0 = boost::filesystem::path(PARTIAL_VOCAB_FILE_NAME + std::to_string(0));
    _path1 =  boost::filesystem::path(PARTIAL_VOCAB_FILE_NAME + std::to_string(1));
    // those names can be random
    _pathExp0 = boost::filesystem::path(".partialVocabExp0");
    _pathExp1  = boost::filesystem::path(".partialVocabExp1");

    // create random subdirectory in /tmp
    auto tempPath = boost::filesystem::temp_directory_path();
    _basePath = tempPath / _basePath;
    boost::filesystem::create_directory(_basePath);

    // make paths abolute under created tmp directory
    _path0 = _basePath / _path0;
    _path1 = _basePath / _path1;
    _pathExp0 = _basePath / _pathExp0;
    _pathExp1 = _basePath / _pathExp1;
    _pathVocabExp  = _basePath / boost::filesystem::path(".vocabExp");
    _pathExternalVocabExp = _basePath / boost::filesystem::path("externalTextFileExp");


    // these will be the contents of partial vocabularies, second element of
    // pair is the correct Id which is expected from mergeVocabulary
    std::vector<std::pair<std::string, size_t>> words1{{"ape", 0}, {"gorilla", 2}, {"monkey", 3}, {std::string{EXTERNALIZED_LITERALS_PREFIX} + "bla", 5}};
    std::vector<std::pair<std::string, size_t>> words2{{"bear", 1}, {"monkey", 3}, {"zebra", 4}};

    // write expected vocabulary files
    std::ofstream expVoc(_pathVocabExp.string());
    std::ofstream expExtVoc(_pathExternalVocabExp.string());
    expVoc << "ape\nbear\ngorilla\nmonkey\nzebra\n";
    expExtVoc << EXTERNALIZED_LITERALS_PREFIX << "bla\n";
   

    // open files for partial Vocabularies
    auto mode = std::ios_base::out | std::ios_base::binary;
    std::ofstream partial0(_path0.string(), mode);
    std::ofstream partial1(_path1.string(), mode);
    std::ofstream partialExp0(_pathExp0.string(), mode);
    std::ofstream partialExp1(_pathExp1.string(), mode);

    if (! partial0.is_open()) std::cerr << "could not open temp file at" << _path0 << '\n';
    if (! partial1.is_open()) std::cerr << "could not open temp file at" << _path1 << '\n';
    if (! partialExp0.is_open()) std::cerr << "could not open temp file at" << _pathExp0 << '\n';
    if (! partialExp1.is_open()) std::cerr << "could not open temp file at" << _pathExp1 << '\n';


    // write first partial vocabulary
    for (const auto& w : words1) {
      std::string word;
      size_t id;
      size_t zeros = 0;
      std::tie(word, id) = w;
      uint32_t len = word.size();
      // write 4 Bytes of string length
      partial0.write((char*) &len, sizeof(uint32_t));
      partialExp0.write((char*) &len, sizeof(uint32_t));

      // write the word
      partial0.write(word.c_str(), len);
      partialExp0.write(word.c_str(), len);

      // zero for the file on which mergeVocabulary will work (that's how
      // they are created in index.cpp
      partial0.write((char*) &zeros, sizeof(size_t));
      // valid Id for the expected partial vocab
      partialExp0.write((char*) &id, sizeof(size_t));
    }

    // write second partialVocabulary
    for (const auto& w : words2) {
      std::string word;
      size_t id;
      size_t zeros = 0;
      std::tie(word, id) = w;
      uint32_t len = word.size();
      partial1.write((char*) &len, sizeof(uint32_t));
      partialExp1.write((char*) &len, sizeof(uint32_t));

      partial1.write(word.c_str(), len);
      partialExp1.write(word.c_str(), len);

      partial1.write((char*) &zeros, sizeof(size_t));
      partialExp1.write((char*) &id, sizeof(size_t));
    }
  }

  // __________________________________________________________________
  ~MergeVocabularyTest() {
    // TODO: shall we delete the tmp files? doing so is cleaner, but makes it
    // harder to debug test failures
  }

  // returns true if and only if the files with names n1 and n2 exist, can be
  // opened for reading and are bytewise equal.
  bool areBinaryFilesEqual(const std::string& n1, const std::string& n2) {
    namespace io = boost::iostreams;
    io::mapped_file_source f1(n1);
    io::mapped_file_source f2(n2);

    return (f1.is_open() && f2.is_open() && f1.size() == f2.size() && std::equal(f1.data(), f1.data() + f1.size(), f2.data()));
  }

};

// Test for merge Vocabulary
TEST_F(MergeVocabularyTest, bla) {

  // mergeVocabulary only gets name of directory and number of files.
  mergeVocabulary(_basePath.string() + '/', 2);
  // Assert that partial vocabularies have the expected ids
  ASSERT_TRUE(areBinaryFilesEqual(_path0.string(), _pathExp0.string()));
  ASSERT_TRUE(areBinaryFilesEqual(_path1.string(), _pathExp1.string()));
  // check that (external) vocabulary has the right form.
  ASSERT_TRUE(areBinaryFilesEqual(_pathVocabExp.string(), _basePath.string() + "/.vocabulary"));
  ASSERT_TRUE(areBinaryFilesEqual(_pathExternalVocabExp.string(), _basePath.string() + "/.externalTextFile"));
}
