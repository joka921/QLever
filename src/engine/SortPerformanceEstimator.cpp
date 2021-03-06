//
// Created by johannes on 21.04.21.
//

#include "SortPerformanceEstimator.h"

#include <cstdlib>

#include "../util/Log.h"
#include "../util/Random.h"
#include "../util/Timer.h"
#include "CallFixedSize.h"
#include "Engine.h"
#include "IdTable.h"

// ___________________________________________________________________
IdTable createRandomIdTable(
    size_t numRows, size_t numColumns,
    const ad_utility::AllocatorWithLimit<Id>& allocator) {
  IdTable result{allocator};
  result.setCols(numColumns);
  result.reserve(numRows);

  FastRandomIntGenerator<Id> generator;

  for (size_t i = 0; i < numRows; ++i) {
    result.push_back();
    for (size_t j = 0; j < numColumns; ++j) {
      result(i, j) = generator();
    }
  }
  return result;
}

// TODO<C++20>: use std::is_sorted which then becomes constexpr.
template <size_t I>
constexpr bool isSorted(const std::array<size_t, I>& array) {
  bool isSorted = true;
  for (size_t i = 1; i < I; ++i) {
    isSorted = isSorted && array[i] >= array[i - 1];
  }
  return isSorted;
}

// ____________________________________________________________________
double SortPerformanceEstimator::measureSortingTimeInSeconds(
    size_t numRows, size_t numColumns,
    const ad_utility::AllocatorWithLimit<Id>& allocator) {
  auto randomTable = createRandomIdTable(numRows, numColumns, allocator);
  ad_utility::Timer timer;
  timer.start();
  // Always sort on the first column for simplicity;
  CALL_FIXED_SIZE_1(numColumns, Engine::sort, &randomTable, 0ull);
  timer.stop();
  return timer.secs();
}

SortPerformanceEstimator::SortPerformanceEstimator(
    const ad_utility::AllocatorWithLimit<Id>& allocator)
    : _samples{} {
  static_assert(isSorted(sampleValuesCols));
  static_assert(isSorted(sampleValuesRows));

  LOG(INFO) << "Sorting some random result tables to estimate the sorting "
               "performance of this machine. This might take several minutes"
            << std::endl;

  for (size_t i = 0; i < NUM_SAMPLES_ROWS; ++i) {
    for (size_t j = 0; j < NUM_SAMPLES_COLS; ++j) {
      auto rows = sampleValuesRows[i];
      auto cols = sampleValuesCols[j];
      try {
        _samples[i][j] = measureSortingTimeInSeconds(rows, cols, allocator);
      } catch (const ad_utility::detail::AllocationExceedsLimitException& e) {
        // These estimates are not too important, since results of this size
        // cannot be sorted anyway because of the memory limit.
        LOG(TRACE) << "Creating the table failed because of a lack of memory"
                   << std::endl;
        LOG(TRACE) << "Creating an estimate from a smaller result" << std::endl;
        if (i > 0) {
          // Assume that sorting time grows linearly in the number of rows
          float ratio = static_cast<float>(sampleValuesRows[i]) /
                        static_cast<float>(sampleValuesRows[i - 1]);
          _samples[i][j] = _samples[i - 1][j] * ratio;
        } else if (j > 0) {
          // Assume that sorting time grows with the square root in the number
          // of columns. The square root is just a heuristic: a simple function
          // between linear and constant.
          float ratio = static_cast<float>(sampleValuesCols[j]) /
                        static_cast<float>(sampleValuesCols[j - 1]);
          _samples[i][j] = _samples[i][j - 1] * std::sqrt(ratio);
        } else {
          // not even the smallest IdTable could be created, this should never
          // happen.
          AD_CHECK(false);
        }
        LOG(TRACE) << "Estimated the sort time to be " << std::fixed
                   << std::setprecision(3) << _samples[i][j] << " seconds."
                   << std::endl;
      }
    }
  }
  LOG(INFO) << "Done creating sort estimates" << std::endl;
}

double SortPerformanceEstimator::estimatedSortTimeInSeconds(
    size_t numRows, size_t numCols) const noexcept {
  // Return the index of the element in the !sorted! `sampleVector`, which is
  // closest to 'value'
  auto getClosestIndex = [](const auto& sampleVector, size_t value) -> size_t {
    // Return the absolute distance to `value`
    auto dist = [value](const size_t& x) {
      return std::abs(static_cast<long long>(value) -
                      static_cast<long long>(x));
    };

    // Returns true iff `a` is closer to `value` than `b`
    auto cmp = [&dist](const auto& a, const auto& b) {
      return dist(a) < dist(b);
    };

    auto closestIterator =
        std::min_element(sampleVector.begin(), sampleVector.end(), cmp);
    return closestIterator - sampleVector.begin();
  };

  // Get index of the closest samples wrt. numRows and numCols
  auto rowIndex = getClosestIndex(sampleValuesRows, numRows);
  auto columnIndex = getClosestIndex(sampleValuesCols, numCols);

  // start with the closest sample
  double result = _samples[rowIndex][columnIndex];

  LOG(TRACE) << "Closest sample result was " << sampleValuesRows[rowIndex]
             << " rows with " << sampleValuesCols[columnIndex]
             << " columns and an estimate of " << result << " seconds."
             << std::endl;

  auto numRowsInSample = static_cast<double>(sampleValuesRows[rowIndex]);
  double rowRatio = static_cast<double>(numRows) / numRowsInSample;

  auto numColumnsInSample = static_cast<double>(sampleValuesCols[columnIndex]);
  double columnRatio = static_cast<double>(numCols) / numColumnsInSample;
  // Scale linearly with the number of rows. Scale the number of columns with
  // the square root
  result = result * rowRatio * std::sqrt(columnRatio);

  return result;
}
