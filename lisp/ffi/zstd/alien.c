//! zstd/alien.c --- ZSTD C wrapper

// frankly, I'm embarassed..

// build with:
/*
  cc -g -O2 -Wall -Wno-unused-value -lzstd -shared lisp/ffi/zstd/alien.c -o .stash/libzstd-alien.so
*/

/// Code:
#include <zdict.h>
size_t ZDICT_finalizeDictionaryWithParams(void* dstDictBuffer, size_t maxDictSize,
                                          const void* dictContent, size_t dictContentSize,
                                          const void* samplesBuffer, const size_t* samplesSizes,
                                          unsigned nbSamples, ZDICT_params_t* parameters) {
  return ZDICT_finalizeDictionary(dstDictBuffer, maxDictSize, dictContent, dictContentSize,
                                  samplesBuffer, samplesSizes, nbSamples, *parameters);}
