#pragma once

#include "../global/Id.h"
#include "../util/File.h"

/**
 * This allows iterating over one of the permutations of the index once.
 **/
template <typename MetaDataType>
class MetaDataIterator {
 public:
  MetaDataIterator(const MetaDataType& meta, ad_utility::File file)
      : meta_(meta),
        _iterator(meta.data().begin()),
        _buffer_offset(0),
        _file(file) {
    scanCurrentPos();
  }

  // prefix increment
  MetaDataIterator& operator++() {
    if (empty()) {
      // don't do anything if we have already reached the end
      return *this;
    }
    ++_buffer_offset;
    if (_buffer_offset >= _bufferIds.size()) {
      ++_iterator;
      scanCurrentPos();
      _buffer_offset = 0;
    }
    return *this;
  }

  std::array<IdWithDatatype, 3> operator*() {
    auto t1 = uniqueFirstColumnType ? uniqueFirstColumnType.value() :  _bufferDatatypesCol1[_buffer_offset];
    auto t2 = uniqueSecondColumnType ? uniqueSecondColumnType.value() :  _bufferDatatypesCol2[_buffer_offset];
    return {currentRmd_._relId, {_bufferIds[_buffer_offset][0], t1},
            {_bufferIds[_buffer_offset][1], t2}};
  }

  bool empty() { return _iterator == meta_.data().end(); }

 private:
  void scanCurrentPos() {
    const FullRelationMetaData& rmd = itToRmd(_iterator);
    currentRmd_ = rmd;
    uniqueFirstColumnType = rmd._firstColumnUniqueDatatype;
    uniqueSecondColumnType = rmd._secondColumnUniqueDatatype;
    _bufferIds.resize(rmd.getNofElements());
    _file.read(_bufferIds.data(), rmd.getNofElements() * 2 * sizeof(Id),
               rmd._startFullIndex);
    size_t c = 0;
    if (!uniqueFirstColumnType) {
      _bufferDatatypesCol1.resize(rmd.getNofElements());
        _file.read(_bufferDatatypesCol1.data(),
                   rmd.getNofElements() * sizeof(Datatype), rmd.getStartOfTypeData());
        c = 1;
    }
    if (!uniqueSecondColumnType) {
      _bufferDatatypesCol2.resize(rmd.getNofElements());
      _file.read(_bufferDatatypesCol2.data(),
                 rmd.getNofElements() * sizeof(Datatype), rmd.getStartOfTypeData() + c * rmd.getNofElements() * sizeof(Datatype));
    }
  }

  const MetaDataType& meta_;
  typename MetaDataType::MapType::ConstIterator _iterator;
  FullRelationMetaData currentRmd_;

  // This buffers the results of the scans we need to use to read the relations
  std::vector<std::array<Id, 2>> _bufferIds;
  std::vector<Datatype> _bufferDatatypesCol1;
  std::vector<Datatype> _bufferDatatypesCol2;
  std::optional<Datatype> uniqueFirstColumnType;
  std::optional<Datatype> uniqueSecondColumnType;
  size_t _buffer_offset;

  ad_utility::File _file;
};
