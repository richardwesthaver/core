;; TODO 2025-08-10: 
(in-package :c)
(include <zdict.h>)
(function ZDICT_finalizeDictionaryWithParams size_t
    ((dstDictBuffer (* void))
     (maxDictSize size_t)
     (dictContent const (* void))
     (dictContentSize size_t)
     (samplesBuffer const (* void))
     (samplesSizes const (* size_t))
     (nbSamples unsigned)
     (parameters (* ZDICT_params_t)))
  (ZDICT_finalizeDictionary 
   dstDictBuffer maxDictSize dictContent dictContentSize samplesBuffer samplesSizes nbSamples (* parameters)))
