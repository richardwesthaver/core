("/usr/include/jpeglib.h" "/usr/include/jconfig.h")
(;; jconfig.h
 (:integer +jpeg-lib-version+ "JPEG_LIB_VERSION")
 ;; not in quotes in jconfig.h so sb-grovel parses as integer instead of string :(
 ;; (:string +libjpeg-turbo-version+ "LIBJPEG_TURBO_VERSION")
 (:integer +libjpeg-turbo-version-number+ "LIBJPEG_TURBO_VERSION_NUMBER")
 (:integer +c-arith-coding-supported+ "C_ARITH_CODING_SUPPORTED")
 (:integer +d-arith-coding-supported+ "D_ARITH_CODING_SUPPORTED")
 (:integer +mem-srcdst-supported+ "MEM_SRCDST_SUPPORTED")
 (:integer +with-simd+ "WITH_SIMD")
 (:integer +bits-in-jsample+ "BITS_IN_JSAMPLE")
 ;; +have-boolean+ +xmd-h+
 ;; jpeglib.h
 (:integer +dctsize+ "DCTSIZE")
 (:integer +dctsize2+ "DCTSIZE2")
 (:integer +max-quant-tbls+ "NUM_QUANT_TBLS")
 (:integer +num-huff-tbls+ "NUM_HUFF_TBLS")
 (:integer +num-arith-tbls+ "NUM_ARITH_TBLS")
 (:integer +max-comps-in-scan+ "MAX_COMPS_IN_SCAN")
 (:integer +max-samp-factor+ "MAX_SAMP_FACTOR")
 (:integer +c-max-blocks-in-mcu+ "C_MAX_BLOCKS_IN_MCU")
 (:integer +d-max-blocks-in-mcu+ "D_MAX_BLOCKS_IN_MCU")
 (:integer +jpool-permanent+ "JPOOL_PERMANENT")
 (:integer +jpool-image+ "JPOOL_IMAGE")
 (:integer +jpool-numpools+ "JPOOL_NUMPOOLS"))

