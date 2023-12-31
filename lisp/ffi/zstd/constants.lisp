("stddef.h" "zstd_errors.h" "zstd.h")

((:integer zstd-clevel-default "ZSTD_CLEVEL_DEFAULT" nil t)
 (:integer zstd-magicnumber "ZSTD_MAGICNUMBER" nil t)
 (:integer zstd-magic-skippable-start "ZSTD_MAGIC_SKIPPABLE_START" nil t)
 (:integer zstd-blocksizelog-max "ZSTD_BLOCKSIZELOG_MAX" nil t)
 (:integer zstd-blocksize-max "ZSTD_BLOCKSIZE_MAX" nil t)
 (:integer zstd-contentsize-unknown "ZSTD_CONTENTSIZE_UNKNOWN" nil t)
 (:integer zstd-contentsize-error "ZSTD_CONTENTSIZE_ERROR" nil t)
 (:integer zstd-max-input-size "ZSTD_MAX_INPUT_SIZE" nil t)

 (:enum zstd-strategy
        ((zstd-fast "ZSTD_fast")
         (zstd-dfast "ZSTD_dfast")
         (zstd-greedy "ZSTD_greedy")
         (zstd-lazy "ZSTD_lazy")
         (zstd-lazy2 "ZSTD_lazy2")
         (zstd-btlazy2 "ZSTD_btlazy2")
         (zstd_btopt "ZSTD_btopt")
         (zstd_btultra "ZSTD_btultra")
         (zstd_btultra2 "ZSTD_btultra2")))

 ;; (:enum zstd-cparameter)

 (:enum zstd-errorcode
        ((zstd-error-no-error "ZSTD_error_no_error")
         (zstd-error-generic "ZSTD_error_GENERIC")
         (zstd-error-prefix-unknown "ZSTD_error_prefix_unknown")
         (zstd-error-version-unsupported "ZSTD_error_version_unsupported")
         (zstd-error-frameparameter-unsupported "ZSTD_error_frameParameter_unsupported")
         (zstd-error-frameparameter-windowtoolarge "ZSTD_error_frameParameter_windowTooLarge")
         (zstd-error-corruption-detected "ZSTD_error_corruption_detected")
         (zstd-error-checksum-wrong "ZSTD_error_checksum_wrong")
         (zstd-error-literals-headerwrong "ZSTD_error_literals_headerWrong")
         (zstd-error-dictionary-corrupted "ZSTD_error_dictionary_corrupted")
         (zstd-error-dictionary-wrong "ZSTD_error_dictionary_wrong")
         (zstd-error-dictionarycreation-failed "ZSTD_error_dictionaryCreation_failed")
         (zstd-error-parameter-unsupported "ZSTD_error_parameter_unsupported")
         (zstd-error-parameter-combination-unsupported "ZSTD_error_parameter_combination_unsupported")
         (zstd-error-parameter-outofbound "ZSTD_error_parameter_outOfBound")
         (zstd-error-tablelog-toolarge "ZSTD_error_tableLog_tooLarge")
         (zstd-error-maxsymbolvalue-toolarge "ZSTD_error_maxSymbolValue_tooLarge")
         (zstd-error-maxsymbolvalue-toosmall "ZSTD_error_maxSymbolValue_tooSmall")
         (zstd-error-stabilitycondition-notrespected "ZSTD_error_stabilityCondition_notRespected")
         (zstd-error-stage-wrong "ZSTD_error_stage_wrong")
         (zstd-error-init-missing "ZSTD_error_init_missing")
         (zstd-error-memory-allocation "ZSTD_error_memory_allocation")
         (zstd-error-workspace-toosmall "ZSTD_error_workSpace_tooSmall")
         (zstd-error-dstsize-toosmall "ZSTD_error_dstSize_tooSmall")
         (zstd-error-srcsize-wrong "ZSTD_error_srcSize_wrong")
         (zstd-error-dstbuffer-null "ZSTD_error_dstBuffer_null")
         (zstd-error-noforwardprogress-destfull "ZSTD_error_noForwardProgress_destFull")
         (zstd-error-noforwardprogress-inputempty "ZSTD_error_noForwardProgress_inputEmpty")
         ;; UNSTABLE
         (zstd-error-frameindex-toolarge "ZSTD_error_frameIndex_tooLarge")
         (zstd-error-seekableio "ZSTD_error_seekableIO")
         (zstd-error-dstbuffer-wrong "ZSTD_error_dstBuffer_wrong")
         (zstd-error-srcbuffer-wrong "ZSTD_error_srcBuffer_wrong")
         (zstd-error-sequenceproducer-failed "ZSTD_error_sequenceProducer_failed")
         (zstd-error-externalsequences-invalid "ZSTD_error_externalSequences_invalid")
         (zstd-error-maxcode "ZSTD_error_maxCode")) 
        nil t))
