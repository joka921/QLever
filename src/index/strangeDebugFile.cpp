#include <array>
#include "../parser/NTriplesParser.h"
#include "Index.h"
using std::array;
using std::string;
void passNTriplesFileIntoIdVector(const string& ntFile, 
                                         bool onDiskLiterals, size_t linesPerPartial) {
  array<string, 3> spo;
  NTriplesParser p(ntFile);
  google::sparse_hash_map<string, Id> vocabMap = vocabMapFromPartialIndexedFile("/nfs/raid5/kalmbacj/testidx/" + "partialVocabularywithIdx0") ;
  size_t i = 0;
  size_t numFiles = 0;
  // write using vector_bufwriter
  ExtVec::bufwriter_type writer(data);
  while (p.getLine(spo)) {
    if (ad_utility::isXsdValue(spo[2])) {
      spo[2] = ad_utility::convertValueLiteralToIndexWord(spo[2]);
    }
    if (onDiskLiterals && isLiteral(spo[2]) && shouldBeExternalized(spo[2])) {
      spo[2] = string({EXTERNALIZED_LITERALS_PREFIX}) + spo[2];
    }

    // Duplicated in pass for Id Vector, externalize to function
    bool broken = false;
    for (size_t k = 0; k < 3; ++k) {
      auto pref = _vocabWithPrefixes.removeAndGetPrefix(spo[k]);
      if (pref != "") {
	spo[k] = pref + ":" + spo[k];
	//LOG(INFO) << spo[k] << "\n";
	if (vocabMap.find(spo[k]) == vocabMap.end()) {
	  LOG(INFO) << "not found in partial Vocab: " << spo[k] << '\n';
	  broken = true;
	}

      }
    }
    if (broken) continue;
    writer << array<Id, 3>{{
                               vocabMap.find(spo[0])->second,
                               vocabMap.find(spo[1])->second,
                               vocabMap.find(spo[2])->second
                           }};
    ++i;
    if (i % 10000000 == 0) {
      LOG(INFO) << "Lines processed: " << i << '\n';
    }

    if (i % linesPerPartial == 0) {
      numFiles++;
      LOG(INFO) << "Lines processed: " << i << '\n';
      std::cout << "reading partial vocab no. " << numFiles << std::endl;
      vocabMap = vocabMapFromPartialIndexedFile("/nfs/raid5/kalmbacj/testidx/" + "partialVocabularywithIdx" + std::to_string(numFiles));
    }
  }
  writer.finish();
  LOG(INFO) << "Pass done.\n";
}
