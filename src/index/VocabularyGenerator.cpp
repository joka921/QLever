#include "./VocabularyGenerator.h"
#include <unordered_set>
#include <vector>
#include <queue>
#include <string>
#include <utility>
#include <fstream>
#include <iostream>

class PairCompare {
 public:
  bool operator()(const std::pair<std::string, size_t>& p1, const std::pair<std::string, size_t>& p2) {
    return p1.first > p2.first;}
};
// ___________________________________________________________________
void mergeVocabulary(const std::string& basename, size_t numFiles) {
  size_t bufferSize = 200000;
  std::vector<std::ifstream> infiles;
  std::vector<std::ofstream> outfilesSplit;
  std::vector<std::vector<std::string>> vecs(numFiles);
  std::ofstream outfile(basename + "finalVocab");
  std::vector<bool> endOfFile(numFiles, false);
  std::vector<std::vector<std::string>::iterator> iterators;
  for (size_t i = 0; i < numFiles; i++) {
    infiles.emplace_back(basename + std::to_string(i));
    outfilesSplit.emplace_back(basename + "withIdx" +  std::to_string(i));
    endOfFile[i] = true;
    size_t numRead = 0;
    vecs[i].reserve(bufferSize);
    std::string word;
    while (std::getline(infiles[i], word)) {
      vecs[i].push_back(word);
      numRead += 1;
      if (numRead == bufferSize) {
	endOfFile[i] = false;
	break;
      }
    }
  }

 
  using pair_T = std::pair<std::string, size_t>;
  std::priority_queue<pair_T, std::vector<pair_T>, PairCompare> queue;
  for (size_t i = 0; i < numFiles; i++) {
    if (!vecs[i].empty()) {
      queue.push(std::make_pair(vecs[i].front(), i));
      //std::cout << vecs[i].front() << i << std::endl;;
    }
    iterators.push_back(vecs[i].begin());
  }

  std::string lastWritten = ""; 
  size_t totalWritten = 0;

  while (! queue.empty()) {
    auto top = queue.top();
    if (top.first.substr(0, 3) == "owl") {
      std::cout << "breakpoint";
    }
    queue.pop();


    // avoid duplicates
    if (top.first != lastWritten) {
      lastWritten = top.first;
      outfile << top.first << std::endl;
      outfilesSplit[top.second] << top.first << "\t" << totalWritten << '\n';
      totalWritten++;
      //std::cout << "outputting " << top.first << " from " << top.second << std::endl;
    } else {
      // always write Index (also in case of duplicates)
      // we already have increased total written, so for the duplicate
      // we have to subtract one again
      outfilesSplit[top.second] << top.first << "\t" << totalWritten - 1 << '\n';

    }

    // refill with top element from current vector
    iterators[top.second]++;
    if (iterators[top.second] == vecs[top.second].end()) {
      if (!endOfFile[top.second]) {
	// if above condition is false, we have no more words for file i, just
	// skip
	
	// refill vector from file
	vecs[top.second].clear();
	auto i = top.second;
	size_t numRead = 0;
	std::string word;
	endOfFile[top.second] = true;
	std::cout << "reloading from partial file no. " << top.second << std::endl;
	while (std::getline(infiles[top.second], word)) {
	  vecs[i].push_back(word);
	  numRead += 1;
	  if (numRead == bufferSize) {
	    endOfFile[i] = false;
	    break;
	  }
	}
	iterators[i] = vecs[i].begin();
	if (vecs[i].begin() != vecs[i].end()) {
	  queue.push(std::make_pair(vecs[i].front(), i));
	}
      }
    } else {
      queue.push(std::make_pair(*(iterators[top.second]), top.second));
    }
    /*
    if (totalWritten % 100 == 0) {
      std::cout << totalWritten << std::endl;
      std::terminate();
    }
    */
  }
}
/*
// Alternative and Terminating implementation
size_t Index::passNTriplesFileForVocabulary(const string& ntFile,
                                            bool onDiskLiterals) {
  LOG(INFO) << "Making pass over NTriples " << ntFile << " for vocabulary."
            << std::endl;
  array<string, 3> spo;
  NTriplesParser p(ntFile);
  ad_utility::HashSet<string> items;
  size_t i = 0;
  size_t numFiles = 0;
  while (p.getLine(spo)) {
    if (ad_utility::isXsdValue(spo[2])) {
      spo[2] = ad_utility::convertValueLiteralToIndexWord(spo[2]);
    }
    if (onDiskLiterals && isLiteral(spo[2]) && shouldBeExternalized(spo[2])) {
      spo[2] = string({EXTERNALIZED_LITERALS_PREFIX}) + spo[2];
    }
    items.insert(spo[0]);
    items.insert(spo[1]);
    items.insert(spo[2]);
    ++i;
    if (i % 10000000 == 0) {
      std::cout << "Lines processed: " << i << '\n';
    }

    if (i % 100000000 == 0) {
      LOG(INFO) << "Lines processed: " << i << '\n';
      std::cout << "writing partial vocab no. " << numFiles << std::endl;
      Vocabulary vocab;
      _vocab.createFromSet(items);
      _vocab.writeToFile(onDiskBase + "partialVocabulary" + std::to_string(numFiles);
      items.clear();
      numFiles++;
    }
  }
  std::terminate()
  LOG(INFO) << "Pass done.\n";
  return i;

}

 */ 

/*
int main(void) {
  std::vector<SparqlPrefix> prefixes;
  prefixes.emplace_back("rdf"s, "http://www.w3.org/1999/02/22-rdf-syntax-ns#"s);
  prefixes.emplace_back("xsd"s, "http://www.w3.org/2001/XMLSchema#"s);
  prefixes.emplace_back("rdfs"s, "http://www.w3.org/2000/01/rdf-schema#"s);
  prefixes.emplace_back("owl"s, "http://www.w3.org/2002/07/owl#"s);
  prefixes.emplace_back("wikibase"s, "http://wikiba.se/ontology-beta#"s);
  prefixes.emplace_back("wds"s, "http://www.wikidata.org/entity/statement/"s);
  prefixes.emplace_back("wdata"s, "https://www.wikidata.org/wiki/Special:EntityData/"s);
  prefixes.emplace_back("skos"s, "http://www.w3.org/2004/02/skos/core#"s);
  mergeVocabulary("/nfs/raid5/kalmbacj/hugePartialVocab/partialVocabulary", 41);
}

*/


