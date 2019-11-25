// Copyright 2018, University of Freiburg,
// Chair of Algorithms and Data Structures.
// Author: Florian Kramer (florian.kramer@mail.uni-freiburg.de)

#include "GroupBy.h"

#include "../index/Index.h"
#include "../util/Conversions.h"
#include "../util/HashSet.h"
#include "CallFixedSize.h"
#include "../util/HighOrderFunctions.h"

using highOrderFunctions::ADD;
using highOrderFunctions::CONST;
using highOrderFunctions::ID;

GroupBy::GroupBy(QueryExecutionContext* qec,
                 const vector<string>& groupByVariables,
                 const std::vector<ParsedQuery::Alias>& aliases)
    : Operation(qec), _subtree(nullptr), _groupByVariables(groupByVariables) {
  _aliases.reserve(aliases.size());
  for (const ParsedQuery::Alias& a : aliases) {
    // Only aggregate aliases need to be processed by GruopBy, other aliases
    // will be processed seperately.
    if (a._isAggregate) {
      _aliases.push_back(a);
    }
  }
  std::sort(_aliases.begin(), _aliases.end(),
            [](const ParsedQuery::Alias& a1, const ParsedQuery::Alias& a2) {
              return a1._outVarName < a2._outVarName;
            });

  // sort the groupByVariables to ensure the cache key is order invariant
  std::sort(_groupByVariables.begin(), _groupByVariables.end());

  // The returned columns are all groupByVariables followed by aggregrates
  size_t colIndex = 0;
  for (std::string var : _groupByVariables) {
    _varColMap[var] = colIndex;
    colIndex++;
  }
  for (const ParsedQuery::Alias& a : _aliases) {
    _varColMap[a._outVarName] = colIndex;
    colIndex++;
  }
}

void GroupBy::setSubtree(std::shared_ptr<QueryExecutionTree> subtree) {
  _subtree = subtree;
}

string GroupBy::asString(size_t indent) const {
  std::ostringstream os;
  for (size_t i = 0; i < indent; ++i) {
    os << " ";
  }
  os << "GROUP_BY ";
  for (const std::string var : _groupByVariables) {
    os << var << ", ";
  }
  for (auto p : _aliases) {
    os << p._function << ", ";
  }
  os << std::endl;
  os << _subtree->asString(indent);
  return os.str();
}

string GroupBy::getDescriptor() const {
  return "GroupBy on " + ad_utility::join(_groupByVariables, ' ');
}

size_t GroupBy::getResultWidth() const { return _varColMap.size(); }

vector<size_t> GroupBy::resultSortedOn() const {
  auto varCols = getVariableColumns();
  vector<size_t> sortedOn;
  sortedOn.reserve(_groupByVariables.size());
  for (std::string var : _groupByVariables) {
    sortedOn.push_back(varCols[var]);
  }
  return sortedOn;
}

vector<pair<size_t, bool>> GroupBy::computeSortColumns(
    std::shared_ptr<QueryExecutionTree> inputTree) {
  vector<pair<size_t, bool>> cols;
  if (_groupByVariables.empty()) {
    // the entire input is a single group, no sorting needs to be done
    return cols;
  }

  ad_utility::HashMap<string, size_t> inVarColMap =
      inputTree->getVariableColumns();

  std::unordered_set<size_t> sortColSet;

  for (std::string var : _groupByVariables) {
    size_t col = inVarColMap[var];
    // avoid sorting by a column twice
    if (sortColSet.find(col) == sortColSet.end()) {
      sortColSet.insert(col);
      cols.push_back({col, false});
    }
  }

  for (const ParsedQuery::Alias& a : _aliases) {
    size_t col = inVarColMap[a._inVarName];
    if (sortColSet.find(col) == sortColSet.end()) {
      sortColSet.insert(col);
      cols.push_back({inVarColMap[a._inVarName], false});
    }
  }
  return cols;
}

ad_utility::HashMap<string, size_t> GroupBy::getVariableColumns() const {
  return _varColMap;
}

float GroupBy::getMultiplicity(size_t col) {
  // Group by should currently not be used in the optimizer, unless
  // it is part of a subquery. In that case multiplicities may only be
  // taken from the actual result
  (void)col;
  return 1;
}

size_t GroupBy::getSizeEstimate() {
  // TODO: stub implementation of getSizeEstimate()
  return _subtree->getSizeEstimate();
}

size_t GroupBy::getCostEstimate() {
  // TODO: add the cost of the actual group by operation to the cost.
  // Currently group by is only added to the optimizer as a terminal operation
  // and its cost should not affect the optimizers results.
  return _subtree->getCostEstimate();
}

template <typename T, typename C>
struct resizeIfVec {
  static void resize(T& t, int size) {
    (void)t;
    (void)size;
  }
};

template <typename C>
struct resizeIfVec<vector<C>, C> {
  static void resize(vector<C>& t, int size) { t.resize(size); }
};

/**
 * @brief This method takes a single group and computes the output for the
 *        given aggregate.
 * @param a The aggregate which should be used to create the output
 * @param blockStart Where the group start.
 * @param blockEnd Where the group ends.
 * @param input The input Table.
 * @param inputTypes The types of the input tables columns.
 * @param result
 * @param inTable The input ResultTable, which is required for its local
 *                vocabulary
 * @param outTable The output ResultTable, the vocabulary of which needs to be
 *                 expanded for GROUP_CONCAT aggregates
 * @param index The kb index. Required for vocabulary lookups.
 * @param distinctHashSet An empty hash set. This is only passed in as an
 *                        argument to allow for efficient reusage of its
 *                        its already allocated storage.
 */

template <int IN_WIDTH, int OUT_WIDTH>
void processGroup(const GroupBy::Aggregate& a, size_t blockStart,
                  size_t blockEnd, const IdTableStatic<IN_WIDTH>& input,
                  const vector<ResultTable::ResultType>& inputTypes,
                  IdTableStatic<OUT_WIDTH>* result, size_t resultRow,
                  [[maybe_unused]] const ResultTable* inTable, ResultTable* outTable,
                  const Index& index,
                  ad_utility::HashSet<size_t>& distinctHashSet) {

  auto numericalAggregateLoop = [blockStart, blockEnd, inCol = a._inCol, &input](auto combineFun, auto getFun) {
    auto res = getFun(input(blockStart, inCol));
    for (size_t i = blockStart + 1; i <= blockEnd; i++) {
      res = combineFun(res, getFun(input(i, inCol)));
    }
    return res;
  };

  auto numericalAggregate = [&inputTypes, &a, &numericalAggregateLoop](auto aggregateFun) {
    if (inputTypes[a._inCol] == ResultTable::ResultType::VERBATIM) {
      return numericalAggregateLoop(aggregateFun, ID);
    } else if (inputTypes[a._inCol] == ResultTable::ResultType::FLOAT) {
      return ResultTable::idFromFloat(numericalAggregateLoop(aggregateFun, ResultTable::floatFromVerbatim));
    } else if (inputTypes[a._inCol] == ResultTable::ResultType::TEXT ||
               inputTypes[a._inCol] == ResultTable::ResultType::LOCAL_VOCAB) {
      return ID_NO_VALUE;
    } else {
      return numericalAggregateLoop(aggregateFun, ID);
    }
  };

  // this lambda performs the actual aggregation logic
  // args: initialResult: the initial aggregate value (typically 0 or an default-constructed ostream). Type of
  //                      initial result determinex the return type, so make sure to differ float(0), size_t(0)...
  //       getAndCombineValue: must takes a pointer to the aggregate type and an Id. Might return void or bool
  //                           returning false means: aggregation can be shortcutted (breaks from the loop)

  auto processLambda = [&a, blockStart, blockEnd, &distinctHashSet, &input](auto getAndCombineValue, auto&& initialResult) {
    auto res{std::forward<decltype(initialResult)>(initialResult)};

    // handle a single aggregation step for index i
    const auto combine = [&res, &getAndCombineValue, &input, &a] (size_t i) {
      // the combine aggregate must be callable from a pointer to the resulttype and an Id, and must return void or bool
      using CombineResult = std::decay_t<std::invoke_result_t<decltype(getAndCombineValue), decltype(&res), decltype(input(0, 0))>>;
      constexpr bool combineIsVoid = std::is_same_v<void, CombineResult>;
      static_assert(combineIsVoid || std::is_same_v<bool, CombineResult>);

      if constexpr (combineIsVoid) {
        getAndCombineValue(&res, input(i, a._inCol));
        return true;  // make the void case always return true, makes life easier and will get optimized out.
      } else {
        return getAndCombineValue(&res, input(i, a._inCol));
      }
    };

    if (a._distinct) {
      distinctHashSet.clear();
      for (size_t i = blockStart; i <= blockEnd; i++) {
        if (!distinctHashSet.count(input(i, a._inCol))) {
          distinctHashSet.insert(input(i, a._inCol));
          if (!combine(i)) break;
        }
      }
    } else {
      for (size_t i = blockStart; i <= blockEnd; i++) {
        if (!combine(i)) break;
      }
    }
    return res;
  };


  switch (a._type) {
    case ParsedQuery::AggregateType::AVG: [[fallthrough]];
    case ParsedQuery::AggregateType::SUM: {
      float res = 0;
      if (inputTypes[a._inCol] == ResultTable::ResultType::VERBATIM) {
        const auto combine = [](float* res, const auto& x) {*res += x;};
        res = processLambda(combine, res);
      } else if (inputTypes[a._inCol] == ResultTable::ResultType::FLOAT) {
        constexpr auto combineValue = [=] (auto res, const auto& x) {*res += ResultTable::floatFromVerbatim(x); };
        res = processLambda(combineValue, res);
      } else if (inputTypes[a._inCol] == ResultTable::ResultType::TEXT ||
                 inputTypes[a._inCol] == ResultTable::ResultType::LOCAL_VOCAB) {
        res = std::numeric_limits<float>::quiet_NaN();
      } else {
        const auto combineValue = [&index](auto res, const auto& x) {
          auto val = index.idToOptionalString(x).value_or("");
          if (!ad_utility::startsWith(val, VALUE_FLOAT_PREFIX)) {
            *res = std::numeric_limits<float>::quiet_NaN();
            return false;  // allows for early breaking;
          }
          *res += ad_utility::convertIndexWordToFloat(val);
          return true;
        };

        res = processLambda(combineValue, res);
      }
      if (a._type == ParsedQuery::AggregateType::AVG) {
        // this is where sum and AVG actually differ
        res /= static_cast<float>(blockEnd - blockStart + 1);
      }
      (*result)(resultRow, a._outCol) = ResultTable::idFromFloat(res);
      break;
    }

    case ParsedQuery::AggregateType::COUNT: {
      constexpr auto combineLambda = [](auto resPtr, [[maybe_unused]] const auto &value) { ++(*resPtr); };
      (*result)(resultRow, a._outCol) = processLambda(combineLambda, size_t(0));
      break;
    }

    case ParsedQuery::AggregateType::GROUP_CONCAT: {
      std::ostringstream out;
      auto delim = reinterpret_cast<string*>(a._userData);
      auto & concatRes = (*(outTable->_concatResults))[a._outCol];
      concatRes._delim = *delim; // this is done for every group, but unecessary, TODO move
      concatRes._resultType = inputTypes[a._inCol]; // same here
      auto entrySizeBefore = concatRes._entries.size();
      const auto combineLambda = [] (auto it, const auto& val) {*it = val;};
      processLambda(combineLambda, std::back_inserter(concatRes._entries));
      (*result)(resultRow, a._outCol) = concatRes._offsets.size();
      concatRes._offsets.emplace_back(entrySizeBefore, concatRes._entries.size());
      break;
    }
    case ParsedQuery::AggregateType::MAX: {
      auto maxLambda = [](const auto& a, const auto& b) {return std::max(a, b);};
      (*result)(resultRow, a._outCol) = numericalAggregate(maxLambda);
      break;
    }
    case ParsedQuery::AggregateType::MIN: {
      auto minLambda = [](const auto& a, const auto& b) {return std::min(a, b);};
      (*result)(resultRow, a._outCol) = numericalAggregate(minLambda);
      break;
    }
    case ParsedQuery::AggregateType::SAMPLE: [[fallthrough]];
    case ParsedQuery::AggregateType::FIRST: [[fallthrough]];
    case ParsedQuery::AggregateType::LAST:
      // SAMPLE, FIRST and LAST share the same behavior, as the non grouping rows have no
      // inherent order.
      (*result)(resultRow, a._outCol) = input(blockEnd, a._inCol);
      break;
  }
}

template <int IN_WIDTH, int OUT_WIDTH>
void doGroupBy(const IdTable& dynInput,
               const vector<ResultTable::ResultType>& inputTypes,
               const vector<size_t>& groupByCols,
               const vector<GroupBy::Aggregate>& aggregates, IdTable* dynResult,
               const ResultTable* inTable, ResultTable* outTable,
               const Index& index) {
  LOG(DEBUG) << "Group by input size " << dynInput.size() << std::endl;
  if (dynInput.size() == 0) {
    return;
  }
  const IdTableStatic<IN_WIDTH> input = dynInput.asStaticView<IN_WIDTH>();
  IdTableStatic<OUT_WIDTH> result = dynResult->moveToStatic<OUT_WIDTH>();
  ad_utility::HashSet<size_t> distinctHashSet;


  for (const auto& a : aggregates) {
    if (a._type == ParsedQuery::AggregateType::GROUP_CONCAT) {
      outTable->_concatResults = std::make_shared<std::vector<ResultTable::GroupConcatResults>>(result.cols());
      break;
    }
  }

  if (groupByCols.empty()) {
    // The entire input is a single group
    size_t blockStart = 0;
    size_t blockEnd = input.size();

    result.emplace_back();
    size_t resIdx = result.size() - 1;
    for (const GroupBy::Aggregate& a : aggregates) {
      processGroup(a, blockStart, blockEnd, input, inputTypes, &result, resIdx,
                   inTable, outTable, index, distinctHashSet);
    }
    *dynResult = result.moveToDynamic();
    return;
  }

  // This stores the values of the group by cols for the current block. A block
  // ends when one of these values changes.
  std::vector<std::pair<size_t, Id>> currentGroupBlock;
  for (size_t col : groupByCols) {
    currentGroupBlock.push_back(std::pair<size_t, Id>(col, input(0, col)));
  }
  size_t blockStart = 0;
  size_t blockEnd = 0;
  for (size_t pos = 1; pos < input.size(); pos++) {
    bool rowMatchesCurrentBlock = true;
    for (size_t i = 0; i < currentGroupBlock.size(); i++) {
      if (input(pos, currentGroupBlock[i].first) !=
          currentGroupBlock[i].second) {
        rowMatchesCurrentBlock = false;
        break;
      }
    }
    if (!rowMatchesCurrentBlock) {
      blockEnd = pos - 1;

      result.emplace_back();
      size_t resIdx = result.size() - 1;
      for (const GroupBy::Aggregate& a : aggregates) {
        processGroup(a, blockStart, blockEnd, input, inputTypes, &result,
                     resIdx, inTable, outTable, index, distinctHashSet);
      }
      // setup for processing the next block
      blockStart = pos;
      for (size_t i = 0; i < currentGroupBlock.size(); i++) {
        currentGroupBlock[i].second = input(pos, currentGroupBlock[i].first);
      }
    }
  }
  blockEnd = input.size() - 1;
  {
    result.emplace_back();
    size_t resIdx = result.size() - 1;
    for (const GroupBy::Aggregate& a : aggregates) {
      processGroup(a, blockStart, blockEnd, input, inputTypes, &result, resIdx,
                   inTable, outTable, index, distinctHashSet);
    }
  }
  *dynResult = result.moveToDynamic();
}

void GroupBy::computeResult(ResultTable* result) {
  LOG(DEBUG) << "GroupBy result computation..." << std::endl;
  std::vector<size_t> groupByColumns;

  result->_sortedBy = resultSortedOn();
  result->_data.setCols(getResultWidth());

  std::vector<Aggregate> aggregates;
  aggregates.reserve(_aliases.size() + _groupByVariables.size());

  // parse the group by columns
  ad_utility::HashMap<string, size_t> subtreeVarCols =
      _subtree->getVariableColumns();
  for (const string& var : _groupByVariables) {
    auto it = subtreeVarCols.find(var);
    if (it == subtreeVarCols.end()) {
      LOG(WARN) << "Group by variable " << var << " is not groupable."
                << std::endl;
      AD_THROW(ad_semsearch::Exception::BAD_QUERY,
               "Groupby variable " + var + " is not groupable");
    }
    groupByColumns.push_back(it->second);
    // Add an "identity" aggregate in the form of a sample aggregate to
    // facilitate the passthrough of the groupBy columns into the result
    aggregates.emplace_back();
    aggregates.back()._type = ParsedQuery::AggregateType::SAMPLE;
    aggregates.back()._inCol = it->second;
    aggregates.back()._outCol = _varColMap.find(var)->second;
    aggregates.back()._userData = nullptr;
    aggregates.back()._distinct = false;
  }

  // parse the aggregate aliases
  for (const ParsedQuery::Alias& alias : _aliases) {
    if (alias._isAggregate) {
      aggregates.emplace_back();
      aggregates.back()._type = alias._type;
      aggregates.back()._distinct = alias._isDistinct;
      if (alias._type == ParsedQuery::AggregateType::GROUP_CONCAT) {
        aggregates.back()._userData = new std::string(alias._delimiter);
      } else {
        aggregates.back()._userData = nullptr;
      }
      auto inIt = subtreeVarCols.find(alias._inVarName);
      if (inIt == subtreeVarCols.end()) {
        LOG(WARN) << "The aggregate alias " << alias._function << " refers to "
                  << "a column not present in the query." << std::endl;
        return;
      }
      aggregates.back()._inCol = inIt->second;
      aggregates.back()._outCol = _varColMap.find(alias._outVarName)->second;
    }
  }

  std::shared_ptr<const ResultTable> subresult = _subtree->getResult();
  LOG(DEBUG) << "GroupBy subresult computation done" << std::endl;

  RuntimeInformation& runtimeInfo = getRuntimeInfo();
  runtimeInfo.addChild(_subtree->getRootOperation()->getRuntimeInfo());

  // populate the result type vector
  result->_resultTypes.resize(result->_data.cols());
  for (size_t i = 0; i < result->_data.cols(); i++) {
    switch (aggregates[i]._type) {
      case ParsedQuery::AggregateType::AVG:
        result->_resultTypes[i] = ResultTable::ResultType::FLOAT;
        break;
      case ParsedQuery::AggregateType::COUNT:
        result->_resultTypes[i] = ResultTable::ResultType::VERBATIM;
        break;
      case ParsedQuery::AggregateType::GROUP_CONCAT:
        result->_resultTypes[i] = ResultTable::ResultType::CONCATENATION;
        break;
      case ParsedQuery::AggregateType::MAX:
        result->_resultTypes[i] =
            subresult->getResultType(aggregates[i]._inCol);
        break;
      case ParsedQuery::AggregateType::MIN:
        result->_resultTypes[i] =
            subresult->getResultType(aggregates[i]._inCol);
        break;
      case ParsedQuery::AggregateType::SAMPLE:
        result->_resultTypes[i] =
            subresult->getResultType(aggregates[i]._inCol);
        break;
      case ParsedQuery::AggregateType::SUM:
        result->_resultTypes[i] = ResultTable::ResultType::FLOAT;
        break;
      default:
        result->_resultTypes[i] = ResultTable::ResultType::KB;
    }
  }

  std::vector<size_t> groupByCols;
  groupByCols.reserve(_groupByVariables.size());
  for (const string& var : _groupByVariables) {
    groupByCols.push_back(subtreeVarCols[var]);
  }

  std::vector<ResultTable::ResultType> inputResultTypes;
  inputResultTypes.reserve(subresult->_data.cols());
  for (size_t i = 0; i < subresult->_data.cols(); i++) {
    inputResultTypes.push_back(subresult->getResultType(i));
  }

  int inWidth = subresult->_data.cols();
  int outWidth = result->_data.cols();
  CALL_FIXED_SIZE_2(inWidth, outWidth, doGroupBy, subresult->_data,
                    inputResultTypes, groupByCols, aggregates, &result->_data,
                    subresult.get(), result, getIndex());

  // Free the user data used by GROUP_CONCAT aggregates.
  for (Aggregate& a : aggregates) {
    if (a._type == ParsedQuery::AggregateType::GROUP_CONCAT) {
      delete static_cast<std::string*>(a._userData);
    }
  }

  LOG(DEBUG) << "GroupBy result computation done." << std::endl;
}
