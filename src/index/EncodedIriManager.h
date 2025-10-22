// Copyright 2025, University of Freiburg
// Chair of Algorithms and Data Structures
// Authors: Johannes Kalmbach <kalmbacj@cs.uni-freiburg.de>

#ifndef QLEVER_SRC_INDEX_ENCODEDVALUES_H
#define QLEVER_SRC_INDEX_ENCODEDVALUES_H

#include <limits>

#include "backports/StartsWithAndEndsWith.h"
#include "backports/algorithm.h"
#include "backports/three_way_comparison.h"
#include "global/Id.h"
#include "util/BitUtils.h"
#include "util/CtreHelpers.h"
#include "util/Log.h"
#include "util/json.h"

namespace detail {
// CTRE named capture group identifiers for C++17 compatibility
constexpr ctll::fixed_string digitsCaptureGroup = "digits";

// Enum to identify special hard-coded encoding schemes
enum class SpecialEncodingType {
  None,          // Not a special encoding
  RangePattern,  // BMW range_ pattern: range_<special-32-bit>_<8-bit>_<8-bit>P
  ValRangePattern,  // BMW valRange_ pattern:
                    // valRange_<special-32-bit>_<11-bit>_<11-bit>M
  LaneRef,          // BMW laneRef pattern: laneRef_<special-64-bit>_<4-bit>
  RoadRef,          // BMW roadRef pattern: roadRef_<special-64-bit>_<4-bit>
  SpeedProfile,     // BMW speedprofile pattern:
                    // speedprofile_<special-64-bit>_<4-bit>
  StopLoc,          // BMW stopLoc pattern: stopLoc_<special-64-bit>_<4-bit>
  StopLoc32         // BMW stopLoc pattern (32-bit variant):
                    // stopLoc_<32-bit>_<18-bit>
};

// Configuration for a prefix encoding. Either plain (just a prefix and digits)
// or with bit pattern constraints (prefix + numeric value where certain bits
// must be zero), or with special hard-coded encoding schemes.
struct PrefixConfig {
  // The IRI prefix (with leading angle bracket, e.g., "<http://example.org/")
  std::string prefix;

  // For bit pattern mode: the range of bits that must be zero in the numeric
  // value [bitStart, bitEnd) (half-open interval, bitEnd is exclusive).
  // If not set, plain digit encoding is used.
  std::optional<std::pair<size_t, size_t>> zeroBitRange;

  // Special encoding type for hard-coded patterns
  SpecialEncodingType specialEncoding = SpecialEncodingType::None;

  // Default constructor for plain prefix mode
  explicit PrefixConfig(std::string p) : prefix(std::move(p)) {}

  // Constructor for bit pattern mode. The bit range is [bitStart, bitEnd)
  // where bitEnd is exclusive.
  PrefixConfig(std::string p, size_t bitStart, size_t bitEnd)
      : prefix(std::move(p)), zeroBitRange(std::make_pair(bitStart, bitEnd)) {
    AD_CONTRACT_CHECK(bitStart < bitEnd);
    AD_CONTRACT_CHECK(bitEnd <= 64);
  }

  // Constructor for special hard-coded encodings
  PrefixConfig(std::string p, SpecialEncodingType type)
      : prefix(std::move(p)), specialEncoding(type) {}

  // Check if this is a bit pattern encoding
  bool isBitPatternMode() const { return zeroBitRange.has_value(); }

  // Check if this is a special hard-coded encoding
  bool isSpecialEncoding() const {
    return specialEncoding != SpecialEncodingType::None;
  }

  // Get the bit range (throws if not in bit pattern mode)
  std::pair<size_t, size_t> getBitRange() const {
    AD_CONTRACT_CHECK(isBitPatternMode());
    return zeroBitRange.value();
  }

  // Equality for testing
  bool operator==(const PrefixConfig& other) const {
    return prefix == other.prefix && zeroBitRange == other.zeroBitRange &&
           specialEncoding == other.specialEncoding;
  }
};
}  // namespace detail

// This class allows the encoding of IRIs that start with a fixed prefix
// followed by a sequence of decimal digits directly into an `Id`. For
// example, <http://example.org/12345> with digit sequence `12345` and
// prefix `http://example.org/`.
//
// Two encoding modes are supported:
//
// 1. PLAIN DIGIT MODE (original behavior):
// The digits are encoded in a non-standard way which ensures the order of
// encoded values corresponds to the lexical order of the original IRIs.
// Each decimal digit is encoded as a 4-bit nibble, where digit `i` is
// encoded as `i+1` and converted to a hexadecimal number. The nibbles are
// stored left-aligned (not right-aligned) and filled on the right with zeroes.
//
// 2. BIT PATTERN MODE (new):
// For IRIs with numeric values where certain bits are always zero, this mode
// compresses the value by removing those zero bits. The prefix configuration
// specifies a bit range [bitStart, bitEnd) (half-open interval) that must be
// all zeros. The value is then stored with those bits removed, allowing more
// efficient encoding.
//
// An `Id` has 64 bits, of which the `NumBitsTotal` rightmost bits are
// used for the encoding. The `64 - NumBitsTotal` leftmost bits are ignored when
// decoding and can be used for other purposes. The next `NumBitsTags` bits
// encode the IRI prefix; that is, at most `2 ** NumBitsTags` different prefixes
// can be used. The remaining `NumBitsTotal - NumBitsTags` bits are used to
// encode the digits that follow the prefix.
//
// For example, here are a few example encodings, with `NumBitsTotal = 40` and
// `NumBitsTags = 8`. The prefix is `http://example.org/` and encoded in 8
// bits as `ff`. Note that the IRIs on the left are in lexical order, and so are
// the encodings on the right.
//
// <http://example.org/1>    ->  00 00 00 ff 20 00 00 00
// <http://example.org/10>   ->  00 00 00 ff 21 00 00 00
// <http://example.org/100>  ->  00 00 00 ff 21 10 00 00
// <http://example.org/2>    ->  00 00 00 ff 30 00 00 00
// <http://example.org/20>   ->  00 00 00 ff 31 00 00 00
//
// NOTE: Only IRIs that fulfill these constraints can be encoded. For example,
// if 4 times the number of digits is larger than `NumBitsTotal - NumBitsTags`,
// the IRI will not be encoded (but stored as a regular IRI). See the bottom of
// the file for the default values of `NumBitsTotal` and `NumBitsTags`.
//
template <size_t NumBitsTotal, size_t NumBitsTags>
class EncodedIriManagerImpl {
 public:
  static constexpr size_t NumBitsEncoding = NumBitsTotal - NumBitsTags;

  // We use 4-bit nibbles per digit in the encoding.
  static constexpr size_t NibbleSize = 4;
  static constexpr size_t NumDigits = NumBitsEncoding / NibbleSize;
  static_assert(NumBitsEncoding % NibbleSize == 0);

  static_assert(NumBitsTotal <= 64);
  static_assert(NumBitsTags <= 64);
  static_assert(NumDigits > 0);

  // The prefixes of the IRIs that will be encoded.
  std::vector<detail::PrefixConfig> prefixes_;

  // By default, `prefixes_` contains only the hard-coded BMW patterns.
  EncodedIriManagerImpl() { initializeHardCodedPrefixes(); }

  // Construct from the list of prefixes. The prefixes have to be specified
  // without any brackes, so e.g. "http://example.org/" if IRIs of the form
  // `<http://example.org/1234>` should be encoded.
  explicit EncodedIriManagerImpl(
      std::vector<std::string> prefixesWithoutAngleBrackets) {
    // Reserve space for hard-coded prefixes (6 special patterns)
    static constexpr size_t numHardCodedPrefixes = 6;
    static constexpr auto maxNumPrefixes = 1ULL << NumBitsTags;
    static constexpr auto maxNumConfigurablePrefixes =
        maxNumPrefixes - numHardCodedPrefixes;

    if (prefixesWithoutAngleBrackets.empty()) {
      // Initialize only hard-coded prefixes
      initializeHardCodedPrefixes();
      return;
    }
    // Sort the prefixes lexicographically to make the ordering deterministic
    // (provided that the prefixes do not end with digits).
    ql::ranges::sort(prefixesWithoutAngleBrackets);

    // Remove duplicates.
    //
    // NOTE: `ql::ranges::unique` does not work because of a discrepancy in the
    // return types between `std::ranges` and `range-v3`.
    prefixesWithoutAngleBrackets.erase(
        ::ranges::unique(prefixesWithoutAngleBrackets),
        prefixesWithoutAngleBrackets.end());

    if (prefixesWithoutAngleBrackets.size() > maxNumConfigurablePrefixes) {
      throw std::runtime_error(absl::StrCat(
          "Number of prefixes specified with `--encode-as-id` is ",
          prefixesWithoutAngleBrackets.size(), ", which is too many; ",
          "the maximum is ", maxNumConfigurablePrefixes, " (reduced from ",
          maxNumPrefixes, " to reserve space for hard-coded BMW patterns)"));
    }

    // TODO<C++23> use `std::views::adjacent`.
    for (size_t i = 0; i < prefixesWithoutAngleBrackets.size() - 1; ++i) {
      const auto& a = prefixesWithoutAngleBrackets.at(i);
      const auto& b = prefixesWithoutAngleBrackets.at(i + 1);
      if (ql::starts_with(b, a)) {
        throw std::runtime_error(absl::StrCat(
            "None of the prefixes specified with `--encode-as-id` "
            "may be a prefix of another; here is a violating pair: \"",
            a, "\" and \"", b, "\"."));
      }
    }
    prefixes_.reserve(prefixesWithoutAngleBrackets.size() +
                      numHardCodedPrefixes);
    for (const auto& prefix : prefixesWithoutAngleBrackets) {
      if (ql::starts_with(prefix, '<')) {
        throw std::runtime_error(absl::StrCat(
            "The prefixes specified with `--encode-as-id` must not "
            "be enclosed in angle brackets; here is a violating prefix: \"",
            prefix, "\""));
      }
      prefixes_.emplace_back(absl::StrCat("<", prefix));
    }

    // Add hard-coded prefixes at the end (using the highest prefix IDs)
    initializeHardCodedPrefixes();
  }

  // Try to encode the given string as an `Id`. If the encoding fails, return
  // `std::nullopt`. This happens in one of the following cases:
  //
  // 1. The string is not an `<iriref-in-angle-brackets>`
  // 2. The string does not start with any of the `prefixes_`
  // 3. After the matching prefix, there are characters other than `[0-9]`
  // 4. There are more digits than fit into `NumBitsEncoding` (4 bits / digit)
  std::optional<Id> encode(std::string_view repr) const {
    // Find the matching prefix.
    auto it = ql::ranges::find_if(prefixes_,
                                  [&repr](const detail::PrefixConfig& cfg) {
                                    return ql::starts_with(repr, cfg.prefix);
                                  });
    if (it == prefixes_.end()) {
      return std::nullopt;
    }

    // Get the index of the used prefix.
    auto prefixIndex = static_cast<size_t>(it - prefixes_.begin());

    // Handle special hard-coded encodings
    if (it->isSpecialEncoding()) {
      repr.remove_prefix(it->prefix.size());
      return encodeSpecialPattern(repr, prefixIndex, it->specialEncoding);
    }

    // Check that after the prefix, the string contains only digits and the
    // trailing '>'.
    repr.remove_prefix(it->prefix.size());
    static constexpr auto regex = ctll::fixed_string{"(?<digits>[0-9]+)>"};
    auto match = ctre::match<regex>(repr);
    if (!match) {
      return std::nullopt;
    }

    // Extract the substring with the digits.
    const auto& numString =
        match.template get<detail::digitsCaptureGroup>().to_view();

    // Handle bit pattern mode
    if (it->isBitPatternMode()) {
      return encodeBitPattern(numString, prefixIndex, it->getBitRange());
    }

    // Handle plain digit mode
    if (numString.size() > NumDigits) {
      return std::nullopt;
    }
    return Id::makeFromEncodedVal(encodeDecimalToNBit(numString) |
                                  (prefixIndex << NumBitsEncoding));
  }

  // Convert an `Id` that was encoded using this encoder back to a string.
  // Throw an exception if the `Id` has a datatype different from `EncodedVal`.
  std::string toString(Id id) const {
    AD_CORRECTNESS_CHECK(id.getDatatype() == Datatype::EncodedVal);
    // Get only the rightmost bits that represent the digits.
    static constexpr auto mask =
        ad_utility::bitMaskForLowerBits(NumBitsEncoding);
    auto digitEncoding = id.getEncodedVal() & mask;
    // Get the index of the prefix.
    auto prefixIdx = id.getEncodedVal() >> NumBitsEncoding;
    std::string result;
    const auto& prefixConfig = prefixes_.at(prefixIdx);
    result.reserve(prefixConfig.prefix.size() + NumDigits + 1);
    result = prefixConfig.prefix;

    // Handle special hard-coded encodings
    if (prefixConfig.isSpecialEncoding()) {
      decodeSpecialPattern(result, digitEncoding, prefixConfig.specialEncoding);
      result.push_back('>');
      return result;
    }

    // Handle bit pattern mode
    if (prefixConfig.isBitPatternMode()) {
      decodeBitPattern(result, digitEncoding, prefixConfig.getBitRange());
    } else {
      // Handle plain digit mode
      decodeDecimalFrom64Bit(result, digitEncoding);
    }
    result.push_back('>');
    return result;
  }

  // Conversion to and from JSON.
  static constexpr const char* jsonKey_ =
      "prefixes-with-leading-angle-brackets";
  static constexpr const char* jsonKeyExtended_ = "prefix-configs";

  friend void to_json(nlohmann::json& j,
                      const EncodedIriManagerImpl& encodedIriManager) {
    // Use new format that supports plain, bit pattern, and special modes
    nlohmann::json configs = nlohmann::json::array();
    for (const auto& cfg : encodedIriManager.prefixes_) {
      nlohmann::json item;
      item["prefix"] = cfg.prefix;
      if (cfg.isBitPatternMode()) {
        auto [bitStart, bitEnd] = cfg.getBitRange();
        item["zeroBitStart"] = bitStart;
        item["zeroBitEnd"] = bitEnd;
      } else if (cfg.isSpecialEncoding()) {
        item["specialEncoding"] = static_cast<int>(cfg.specialEncoding);
      }
      configs.push_back(item);
    }
    j[jsonKeyExtended_] = configs;
  }

  friend void from_json(const nlohmann::json& j,
                        EncodedIriManagerImpl& encodedIriManager) {
    encodedIriManager.prefixes_.clear();

    // Try new format first
    if (j.contains(jsonKeyExtended_)) {
      const auto& configs = j[jsonKeyExtended_];
      for (const auto& item : configs) {
        std::string prefix = static_cast<std::string>(item["prefix"]);
        if (item.contains("zeroBitStart") && item.contains("zeroBitEnd")) {
          size_t bitStart = static_cast<size_t>(item["zeroBitStart"]);
          size_t bitEnd = static_cast<size_t>(item["zeroBitEnd"]);
          encodedIriManager.prefixes_.emplace_back(std::move(prefix), bitStart,
                                                   bitEnd);
        } else if (item.contains("specialEncoding")) {
          int encodingType = static_cast<int>(item["specialEncoding"]);
          encodedIriManager.prefixes_.emplace_back(
              std::move(prefix),
              static_cast<detail::SpecialEncodingType>(encodingType));
        } else {
          encodedIriManager.prefixes_.emplace_back(std::move(prefix));
        }
      }
    } else if (j.contains(jsonKey_)) {
      // Fall back to old format for backward compatibility
      std::vector<std::string> oldPrefixes =
          static_cast<std::vector<std::string>>(j[jsonKey_]);
      for (auto& prefix : oldPrefixes) {
        encodedIriManager.prefixes_.emplace_back(std::move(prefix));
      }
    }
  }

  // Hash support for use in `TestIndexConfig`.
  template <typename H>
  friend H AbslHashValue(H h, const EncodedIriManagerImpl& manager) {
    return H::combine(std::move(h), manager.prefixes_);
  }

  // Equality operator for use in `TestIndexConfig`.
  QL_DEFINE_DEFAULTED_EQUALITY_OPERATOR_LOCAL(EncodedIriManagerImpl, prefixes_)

 private:
  // Encode a number with bit pattern constraints. The numString must parse to
  // a valid uint64_t, and the bits in the range [bitStart, bitEnd) must all be
  // zero. Returns nullopt if constraints are not met or if the number is too
  // large to fit after removing the zero bits.
  std::optional<Id> encodeBitPattern(std::string_view numString,
                                     size_t prefixIndex,
                                     std::pair<size_t, size_t> bitRange) const {
    auto [bitStart, bitEnd] = bitRange;

    // Parse the numeric string to uint64_t
    uint64_t value = 0;
    for (char c : numString) {
      // Check for overflow
      if (value > (std::numeric_limits<uint64_t>::max() - (c - '0')) / 10) {
        return std::nullopt;
      }
      value = value * 10 + (c - '0');
    }

    // Check that all bits in the range [bitStart, bitEnd) are zero
    for (size_t bit = bitStart; bit < bitEnd; ++bit) {
      if ((value >> bit) & 1) {
        return std::nullopt;
      }
    }

    // Remove the zero bits from the value
    uint64_t lowerBits = value & ad_utility::bitMaskForLowerBits(bitStart);
    uint64_t upperBits = value >> bitEnd;
    uint64_t compressedValue = (upperBits << bitStart) | lowerBits;

    // Check if the compressed value fits in the available encoding bits
    // We need to store the compressed value, so it must fit in NumBitsEncoding
    if (compressedValue >= (1ULL << NumBitsEncoding)) {
      return std::nullopt;
    }

    return Id::makeFromEncodedVal(compressedValue |
                                  (prefixIndex << NumBitsEncoding));
  }

  // Decode a bit pattern encoded value back to the original uint64_t and
  // append it to the result string. The bit range [bitStart, bitEnd) specifies
  // which bits were removed during encoding.
  static void decodeBitPattern(std::string& result, uint64_t encoded,
                               std::pair<size_t, size_t> bitRange) {
    auto [bitStart, bitEnd] = bitRange;

    // Reconstruct the original value by reinserting the zero bits
    uint64_t lowerBits = encoded & ad_utility::bitMaskForLowerBits(bitStart);
    uint64_t upperBits = encoded >> bitStart;
    uint64_t originalValue = (upperBits << bitEnd) | lowerBits;

    // Convert to decimal string
    result.append(std::to_string(originalValue));
  }

  // Encode the `numberStr` (which may only consist of digits) into a 64-bit
  // number.
  static constexpr uint64_t encodeDecimalToNBit(std::string_view numberStr) {
    auto len = numberStr.size();
    AD_CORRECTNESS_CHECK(len <= NumDigits);

    uint64_t result = 0;

    // Compute the starting shift (for the first digit).
    uint64_t shift = NumBitsEncoding - NibbleSize;

    for (const char digitChar : numberStr) {
      // Deliberately encode [0, ..., 9] as [1, ..., A], so that the padding
      // nibble `0`is smaller than any valid digit encoding.
      uint8_t digit = (digitChar - '0') + 1;
      result |= static_cast<uint64_t>(digit) << shift;
      shift -= NibbleSize;
    }
    return result;
  }

  // The inverse of `encodeDecimalToNBit`. The result is appended to the
  // `result` string.
  static void decodeDecimalFrom64Bit(std::string& result, uint64_t encoded) {
    size_t shift = NumBitsEncoding - NibbleSize;
    auto numTrailingZeros = std::countr_zero(encoded);
    size_t numTrailingZeroNibbles = numTrailingZeros / NibbleSize;
    size_t len = NumDigits - numTrailingZeroNibbles;
    for (size_t i = 0; i < len; ++i) {
      result.push_back(static_cast<char>(((encoded >> shift) & 0xF) + '0' - 1));
      shift -= NibbleSize;
    }
  }

  // Initialize the hard-coded BMW-specific encoding patterns
  void initializeHardCodedPrefixes() {
    // Range pattern: range_<special-32-bit>_<8-bit>_<8-bit>P
    // First number has upper 3 bits as "001"
    prefixes_.emplace_back(
        "<http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/"
        "behaviorMap#range_",
        detail::SpecialEncodingType::RangePattern);

    // ValRange pattern: valRange_<special-32-bit>_<11-bit>_<11-bit>M
    // First number has upper 3 bits as "001"
    prefixes_.emplace_back(
        "<http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/"
        "behaviorMap#valRange_",
        detail::SpecialEncodingType::ValRangePattern);

    // LaneRef pattern: laneRef_<special-64-bit>_<4-bit>
    prefixes_.emplace_back(
        "<http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/"
        "behaviorMap#laneRef_",
        detail::SpecialEncodingType::LaneRef);

    // RoadRef pattern: roadRef_<special-64-bit>_<4-bit>
    prefixes_.emplace_back(
        "<http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/"
        "behaviorMap#roadRef_",
        detail::SpecialEncodingType::RoadRef);

    // SpeedProfile pattern: speedprofile_<special-64-bit>_<4-bit>
    prefixes_.emplace_back(
        "<http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/"
        "behaviorMap#speedprofile_",
        detail::SpecialEncodingType::SpeedProfile);

    // StopLoc pattern (32-bit variant): stopLoc_<32-bit>_<18-bit>
    prefixes_.emplace_back(
        "<http://www.bmw-carit.de/Foresight/Map/Ontologies/Low/"
        "behaviorMap#stopLoc_",
        detail::SpecialEncodingType::StopLoc32);
  }

  // Parse a decimal number string to uint64_t. Returns nullopt on overflow.
  static std::optional<uint64_t> parseDecimal(std::string_view numString) {
    uint64_t value = 0;
    for (char c : numString) {
      // Check for overflow
      if (value > (std::numeric_limits<uint64_t>::max() - (c - '0')) / 10) {
        return std::nullopt;
      }
      value = value * 10 + (c - '0');
    }
    return value;
  }

  // Encode special BMW patterns
  std::optional<Id> encodeSpecialPattern(
      std::string_view suffix, size_t prefixIndex,
      detail::SpecialEncodingType encodingType) const {
    switch (encodingType) {
      case detail::SpecialEncodingType::RangePattern:
        return encodeRangePattern(suffix, prefixIndex);
      case detail::SpecialEncodingType::ValRangePattern:
        return encodeValRangePattern(suffix, prefixIndex);
      case detail::SpecialEncodingType::LaneRef:
      case detail::SpecialEncodingType::RoadRef:
      case detail::SpecialEncodingType::SpeedProfile:
        return encodeRefPattern(suffix, prefixIndex);
      case detail::SpecialEncodingType::StopLoc:
      case detail::SpecialEncodingType::StopLoc32:
        return encodeStopLocPattern(suffix, prefixIndex);
      default:
        return std::nullopt;
    }
  }

  // Decode special BMW patterns
  static void decodeSpecialPattern(std::string& result, uint64_t encoded,
                                   detail::SpecialEncodingType encodingType) {
    switch (encodingType) {
      case detail::SpecialEncodingType::RangePattern:
        decodeRangePattern(result, encoded);
        break;
      case detail::SpecialEncodingType::ValRangePattern:
        decodeValRangePattern(result, encoded);
        break;
      case detail::SpecialEncodingType::LaneRef:
      case detail::SpecialEncodingType::RoadRef:
      case detail::SpecialEncodingType::SpeedProfile:
        decodeRefPattern(result, encoded);
        break;
      case detail::SpecialEncodingType::StopLoc:
      case detail::SpecialEncodingType::StopLoc32:
        decodeStopLocPattern(result, encoded);
        break;
      default:
        break;
    }
  }

  // Encode range_ pattern: range_<special-32-bit>_<8-bit>_<8-bit>P>
  // First number has upper 3 bits as "001" (bit 31=0, bit 30=0, bit 29=1)
  // After removing bit 29, we get 29 bits + 8 + 8 = 45 bits, but we store as
  // 29 + 10 + 11 = 50 bits to align with encoding
  std::optional<Id> encodeRangePattern(std::string_view suffix,
                                       size_t prefixIndex) const {
    // Pattern: <num1>_<num2>_<num3>P>
    static constexpr auto regex =
        ctll::fixed_string{"([0-9]+)_([0-9]+)_([0-9]+)P>"};
    auto match = ctre::match<regex>(suffix);
    if (!match) {
      return std::nullopt;
    }

    auto num1Str = match.get<1>().to_view();
    auto num2Str = match.get<2>().to_view();
    auto num3Str = match.get<3>().to_view();

    auto num1 = parseDecimal(num1Str);
    auto num2 = parseDecimal(num2Str);
    auto num3 = parseDecimal(num3Str);

    if (!num1 || !num2 || !num3) {
      return std::nullopt;
    }

    // Check constraints on num1:
    // - Must fit in 32 bits
    if (*num1 >= (1ULL << 32)) {
      return std::nullopt;
    }
    // - Bits 31 and 30 must be 0 (upper 3 bits are "001")
    if ((*num1 >> 30) != 0) {
      return std::nullopt;
    }
    // - Bit 29 must be 1
    if (((*num1 >> 29) & 1) != 1) {
      return std::nullopt;
    }

    // Check constraints: num2 and num3 fit in 8 bits
    if (*num2 >= (1ULL << 8) || *num3 >= (1ULL << 8)) {
      return std::nullopt;
    }

    // Remove bit 29 from num1 (we know it's 1)
    // Extract lower 29 bits
    uint32_t num1Compressed = *num1 & ad_utility::bitMaskForLowerBits(29);

    // Pack into 50 bits: [num1Compressed:29][num2:10][num3:11]
    // We extend num2 and num3 to 10 and 11 bits respectively for alignment
    uint64_t encoded = (static_cast<uint64_t>(num1Compressed) << 21) |
                       (static_cast<uint64_t>(*num2) << 11) | *num3;

    // Check that encoded value fits in NumBitsEncoding
    if (encoded >= (1ULL << NumBitsEncoding)) {
      return std::nullopt;
    }

    return Id::makeFromEncodedVal(encoded | (prefixIndex << NumBitsEncoding));
  }

  // Decode range_ pattern
  static void decodeRangePattern(std::string& result, uint64_t encoded) {
    // Extract num3 (lowest 11 bits)
    uint16_t num3 = encoded & ad_utility::bitMaskForLowerBits(11);
    // Extract num2 (next 10 bits)
    uint16_t num2 = (encoded >> 11) & ad_utility::bitMaskForLowerBits(10);
    // Extract num1Compressed (next 29 bits)
    uint32_t num1Compressed =
        (encoded >> 21) & ad_utility::bitMaskForLowerBits(29);

    // Reconstruct num1 by setting bit 29
    uint32_t num1 = (1U << 29) | num1Compressed;

    result.append(std::to_string(num1));
    result.push_back('_');
    result.append(std::to_string(num2));
    result.push_back('_');
    result.append(std::to_string(num3));
    result.push_back('P');
  }

  // Encode valRange_ pattern: valRange_<special-32-bit>_<11-bit>_<11-bit>M>
  // First number has upper 3 bits as "001" (bit 31=0, bit 30=0, bit 29=1)
  // After removing bit 29, we get 29 bits + 11 + 11 = 51 bits
  std::optional<Id> encodeValRangePattern(std::string_view suffix,
                                          size_t prefixIndex) const {
    // Pattern: <num1>_<num2>_<num3>M>
    static constexpr auto regex =
        ctll::fixed_string{"([0-9]+)_([0-9]+)_([0-9]+)M>"};
    auto match = ctre::match<regex>(suffix);
    if (!match) {
      return std::nullopt;
    }

    auto num1Str = match.get<1>().to_view();
    auto num2Str = match.get<2>().to_view();
    auto num3Str = match.get<3>().to_view();

    auto num1 = parseDecimal(num1Str);
    auto num2 = parseDecimal(num2Str);
    auto num3 = parseDecimal(num3Str);

    if (!num1 || !num2 || !num3) {
      return std::nullopt;
    }

    // Check constraints on num1:
    // - Must fit in 32 bits
    if (*num1 >= (1ULL << 32)) {
      return std::nullopt;
    }
    // - Bits 31 and 30 must be 0 (upper 3 bits are "001")
    if ((*num1 >> 30) != 0) {
      return std::nullopt;
    }
    // - Bit 29 must be 1
    if (((*num1 >> 29) & 1) != 1) {
      return std::nullopt;
    }

    // Check constraints: num2 and num3 fit in 11 bits
    if (*num2 >= (1ULL << 11) || *num3 >= (1ULL << 11)) {
      return std::nullopt;
    }

    // Remove bit 29 from num1 (we know it's 1)
    // Extract lower 29 bits
    uint32_t num1Compressed = *num1 & ad_utility::bitMaskForLowerBits(29);

    // Pack into 51 bits: [num1Compressed:29][num2:11][num3:11]
    uint64_t encoded = (static_cast<uint64_t>(num1Compressed) << 22) |
                       (static_cast<uint64_t>(*num2) << 11) | *num3;

    // Check that encoded value fits in NumBitsEncoding
    if (encoded >= (1ULL << NumBitsEncoding)) {
      return std::nullopt;
    }

    return Id::makeFromEncodedVal(encoded | (prefixIndex << NumBitsEncoding));
  }

  // Decode valRange_ pattern
  static void decodeValRangePattern(std::string& result, uint64_t encoded) {
    // Extract num3 (lowest 11 bits)
    uint16_t num3 = encoded & ad_utility::bitMaskForLowerBits(11);
    // Extract num2 (next 11 bits)
    uint16_t num2 = (encoded >> 11) & ad_utility::bitMaskForLowerBits(11);
    // Extract num1Compressed (next 29 bits)
    uint32_t num1Compressed =
        (encoded >> 22) & ad_utility::bitMaskForLowerBits(29);

    // Reconstruct num1 by setting bit 29
    uint32_t num1 = (1U << 29) | num1Compressed;

    result.append(std::to_string(num1));
    result.push_back('_');
    result.append(std::to_string(num2));
    result.push_back('_');
    result.append(std::to_string(num3));
    result.push_back('M');
  }

  // Encode stopLoc pattern - handles both 64-bit and 32-bit variants
  // Variant 1 (64-bit): stopLoc_<special-64-bit>_<4-bit>
  //   - First number: highest 3 bits "001", bits [17,32) all zero
  //   - Second number: < 16
  //   - Encoded with flag bit 51 = 0
  // Variant 2 (32-bit): stopLoc_<32-bit>_<18-bit>
  //   - First number: any 32-bit value
  //   - Second number: < 2^18
  //   - Encoded with flag bit 51 = 1
  std::optional<Id> encodeStopLocPattern(std::string_view suffix,
                                         size_t prefixIndex) const {
    // Pattern: <num1>_<num2>
    static constexpr auto regex = ctll::fixed_string{"([0-9]+)_([0-9]+)>"};
    auto match = ctre::match<regex>(suffix);
    if (!match) {
      return std::nullopt;
    }

    auto num1Str = match.get<1>().to_view();
    auto num2Str = match.get<2>().to_view();

    auto num1 = parseDecimal(num1Str);
    auto num2 = parseDecimal(num2Str);

    if (!num1 || !num2) {
      return std::nullopt;
    }

    // Try 64-bit variant first (more restrictive)
    if (*num2 < 16) {
      // Check 64-bit constraints
      if ((*num1 >> 62) == 0 && ((*num1 >> 61) & 1) == 1) {
        // Check bits [17, 32) are all zero
        bool bitsZero = true;
        for (size_t bit = 17; bit < 32; ++bit) {
          if ((*num1 >> bit) & 1) {
            bitsZero = false;
            break;
          }
        }

        if (bitsZero) {
          // Encode as 64-bit variant
          // Extract bits [0, 17) and [32, 61)
          uint64_t lowerBits = *num1 & ad_utility::bitMaskForLowerBits(17);
          uint64_t middleBits =
              (*num1 >> 32) & ad_utility::bitMaskForLowerBits(29);
          // Pack: [0:flag][middleBits:29][lowerBits:17][num2:4] = 51 bits
          uint64_t encoded = (middleBits << 21) | (lowerBits << 4) | *num2;

          if (encoded < (1ULL << NumBitsEncoding)) {
            return Id::makeFromEncodedVal(encoded |
                                          (prefixIndex << NumBitsEncoding));
          }
        }
      }
    }

    // Try 32-bit variant
    if (*num1 < (1ULL << 32) && *num2 < (1ULL << 18)) {
      // Pack: [1:flag][num1:32][num2:18] = 51 bits
      uint64_t encoded = (1ULL << 50) | (*num1 << 18) | *num2;

      if (encoded < (1ULL << NumBitsEncoding)) {
        return Id::makeFromEncodedVal(encoded |
                                      (prefixIndex << NumBitsEncoding));
      }
    }

    return std::nullopt;
  }

  // Decode stopLoc pattern - handles both variants
  static void decodeStopLocPattern(std::string& result, uint64_t encoded) {
    // Check flag bit (bit 50)
    bool is32BitVariant = (encoded >> 50) & 1;

    if (is32BitVariant) {
      // 32-bit variant: [1:flag][num1:32][num2:18]
      uint32_t num2 = encoded & ad_utility::bitMaskForLowerBits(18);
      uint32_t num1 = (encoded >> 18) & ad_utility::bitMaskForLowerBits(32);

      result.append(std::to_string(num1));
      result.push_back('_');
      result.append(std::to_string(num2));
    } else {
      // 64-bit variant: [0:flag][middleBits:29][lowerBits:17][num2:4]
      uint8_t num2 = encoded & 0xF;
      uint64_t lowerBits = (encoded >> 4) & ad_utility::bitMaskForLowerBits(17);
      uint64_t middleBits =
          (encoded >> 21) & ad_utility::bitMaskForLowerBits(29);

      // Reconstruct num1: [0][0][1][middleBits:29][zeros:15][lowerBits:17]
      uint64_t num1 = (1ULL << 61) | (middleBits << 32) | lowerBits;

      result.append(std::to_string(num1));
      result.push_back('_');
      result.append(std::to_string(num2));
    }
  }

  // Encode laneRef/roadRef/speedprofile pattern:
  // <type>_<special-64-bit>_<4-bit>
  // The 64-bit number must have:
  // - Highest 3 bits are "001" (so bit 63=0, bit 62=0, bit 61=1)
  // - Bits [17, 32) are all 0
  // - The second number (4-bit) is < 16
  //
  // After removing the constraints:
  // - Remove bit 61 (we know it's 1)
  // - Remove bits [17, 32) (15 bits, we know they're 0)
  // Total removed: 16 bits
  // Remaining: 64 - 16 = 48 bits from first number + 4 bits from second = 52
  // bits
  std::optional<Id> encodeRefPattern(std::string_view suffix,
                                     size_t prefixIndex) const {
    // Pattern: <num1>_<num2>
    static constexpr auto regex = ctll::fixed_string{"([0-9]+)_([0-9]+)>"};
    auto match = ctre::match<regex>(suffix);
    if (!match) {
      return std::nullopt;
    }

    auto num1Str = match.get<1>().to_view();
    auto num2Str = match.get<2>().to_view();

    auto num1 = parseDecimal(num1Str);
    auto num2 = parseDecimal(num2Str);

    if (!num1 || !num2) {
      return std::nullopt;
    }

    // Check that num2 fits in 4 bits (< 16)
    if (*num2 >= 16) {
      return std::nullopt;
    }

    // Check constraints on num1:
    // - Bits 63 and 62 must be 0 (highest 3 bits are "001")
    if ((*num1 >> 62) != 0) {
      return std::nullopt;
    }
    // - Bit 61 must be 1
    if (((*num1 >> 61) & 1) != 1) {
      return std::nullopt;
    }
    // - Bits [17, 32) must all be 0
    for (size_t bit = 17; bit < 32; ++bit) {
      if ((*num1 >> bit) & 1) {
        return std::nullopt;
      }
    }

    // Remove the constrained bits:
    // Extract bits [0, 17) (lower 17 bits)
    uint64_t lowerBits = *num1 & ad_utility::bitMaskForLowerBits(17);
    // Extract bits [32, 61) (29 bits)
    uint64_t middleBits = (*num1 >> 32) & ad_utility::bitMaskForLowerBits(29);
    // Combine: [middleBits:29][lowerBits:17][num2:4] = 50 bits total
    uint64_t compressed = (middleBits << 21) | (lowerBits << 4) | *num2;

    // Check that compressed value fits in NumBitsEncoding
    if (compressed >= (1ULL << NumBitsEncoding)) {
      return std::nullopt;
    }

    return Id::makeFromEncodedVal(compressed |
                                  (prefixIndex << NumBitsEncoding));
  }

  // Decode laneRef/roadRef/speedprofile/stopLoc pattern
  static void decodeRefPattern(std::string& result, uint64_t encoded) {
    // Extract num2 (lowest 4 bits)
    uint8_t num2 = encoded & 0xF;
    // Extract lowerBits (next 17 bits)
    uint64_t lowerBits = (encoded >> 4) & ad_utility::bitMaskForLowerBits(17);
    // Extract middleBits (next 29 bits)
    uint64_t middleBits = (encoded >> 21) & ad_utility::bitMaskForLowerBits(29);

    // Reconstruct num1:
    // [0][0][1][middleBits:29][zeros:15][lowerBits:17]
    uint64_t num1 = (1ULL << 61) |  // Set bit 61
                    (middleBits << 32) | lowerBits;

    result.append(std::to_string(num1));
    result.push_back('_');
    result.append(std::to_string(num2));
  }
};

// The default encoder for IRIs in QLever: 60 bits are used for the complete
// encodingr, 8 bits are used for the prefixes (which allows up to 256
// prefixes). This leaves 52 bits for the digits, so up to 13 digits can be
// encoded.
using EncodedIriManager = EncodedIriManagerImpl<Id::numDataBits, 8>;

#endif  // QLEVER_SRC_INDEX_ENCODEDVALUES_H
