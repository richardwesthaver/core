;; TODO 2025-08-10: 
(include <zdict.h>)
(function ZDICT_finalizeDictionaryWithParams size_t
    ((dstDictBuffer (* void))
     (maxDictSize size_t)
     (dictContent (const (* void)))
     (dictContentSize size_t)
     (samplesBuffer (const (* void)))
     (samplesSizes (const (* size_t)))
     (nbSamples unsigned)
     (parameters (* ZDICT_params_t)))
  (return 
    (ZDICT_finalizeDictionary 
     dstDictBuffer maxDictSize dictContent dictContentSize samplesBuffer samplesSizes nbSamples (deref parameters))))
