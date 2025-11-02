(include <zdict.h>)
(fn ZDICT_finalizeDictionaryWithParams size_t
    ((dstDictBuffer (* t))
     (maxDictSize size_t)
     (const dictContent (* t))
     (dictContentSize size_t)
     (const samplesBuffer (* t))
     (const samplesSizes (* size_t))
     (nbSamples unsigned)
     (params (* ZDICT_params_t)))
  (return
   (ZDICT_finalizeDictionary
    dstDictBuffer maxDictSize dictContent dictContentSize samplesBuffer samplesSizes nbSamples (deref params))))
