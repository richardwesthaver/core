;;; /home/ellis/comp/core/lisp/lib/dat/parquet/thrift.lisp --- Parquet Thrift Definitions -*- buffer-read-only:t -*-

;; input = /home/ellis/comp/core/.stash/parquet.json

;; This file was generated automatically by
;; DAT/PARQUET/GEN:PARSE-PARQUET-THRIFT-DEFINITIONS

;; Do not modify.

;;; Code:
(in-package :dat/parquet)

(defvar *parquet-json-types*
  '(:boolean :int32 :int64 :int96 :float :double :byte-array
    :fixed-len-byte-array))
(defvar *parquet-json-converted-types*
  '(:utf8 :map :map-key-value :list :enum :decimal :date :time-millis
    :time-micros :timestamp-millis :timestamp-micros :uint-8 :uint-16 :uint-32
    :uint-64 :int-8 :int-16 :int-32 :int-64 :json :bson :interval))
(defvar *parquet-json-field-repetition-types* '(:required :optional :repeated))
(defvar *parquet-json-encodings*
  '(:plain :plain-dictionary :rle :bit-packed :delta-binary-packed
    :delta-length-byte-array :delta-byte-array :rle-dictionary
    :byte-stream-split))
(defvar *parquet-json-compression-codecs*
  '(:uncompressed :snappy :gzip :lzo :brotli :lz4 :zstd :lz4-raw))
(defvar *parquet-json-page-types*
  '(:data-page :index-page :dictionary-page :data-page-v2))
(defvar *parquet-json-boundary-orders* '(:unordered :ascending :descending))
(deftype parquet-boolean () 'boolean)
(deftype parquet-int32 () '(signed-byte 32))
(deftype parquet-int64 () '(signed-byte 64))
(deftype parquet-int96 () '(signed-byte 96))
(deftype parquet-float () 'float)
(deftype parquet-double () 'double-float)
(deftype parquet-byte-array (&optional dat/parquet/gen::size)
  `(octet-vector ,dat/parquet/gen::size))
(deftype parquet-fixed-len-byte-array (dat/parquet/gen::size)
  `(octet-vector ,dat/parquet/gen::size))
(defclass parquet-size-statistics (parquet-object)
          ((unencoded-byte-array-data-bytes :documentation
            "The number of physical bytes stored for BYTE_ARRAY data values assuming
no encoding. This is exclusive of the bytes needed to store the length of
each byte array. In other words, this field is equivalent to the `(size
of PLAIN-ENCODING the byte array values) - (4 bytes * number of values
written)`. To determine unencoded sizes of other types readers can use
schema information multiplied by the number of non-null and null values.
The number of null\\non-null values can be inferred from the histograms
below.

For example, if a column chunk is dictionary-encoded with dictionary
[\\a\\, \\bc\\, \\cde\\], and a data page contains the indices [0, 0, 1, 2],
then this value for that data page should be 7 (1 + 1 + 2 + 3).

This field should only be set for types that use BYTE_ARRAY as their
physical type.
"
            :initarg :unencoded-byte-array-data-bytes :initform nil :type
            (or null (signed-byte 64)))
           (repetition-level-histogram :documentation
            "When present, there is expected to be one element corresponding to each
repetition (i.e. size=max repetition_level+1) where each element
represents the number of times the repetition level was observed in the
data.

This field may be omitted if max_repetition_level is 0 without loss
of information.

"
            :initarg :repetition-level-histogram :initform nil :type
            (or null (vector (signed-byte 64))))
           (definition-level-histogram :documentation
            "Same as repetition_level_histogram except for definition levels.

This field may be omitted if max_definition_level is 0 or 1 without
loss of information.

"
            :initarg :definition-level-histogram :initform nil :type
            (or null (vector (signed-byte 64)))))
          (:documentation
           "A structure for capturing metadata for estimating the unencoded,
uncompressed size of data written. This is useful for readers to estimate
how much memory is needed to reconstruct data in their memory model and for
fine grained filter pushdown on nested structures (the histograms contained
in this structure can help determine the number of nulls at a particular
nesting level and maximum length of lists).
"))
(defclass parquet-statistics (parquet-object)
          ((max :documentation
                "DEPRECATED: min and max value of the column. Use min_value and max_value.

Values are encoded using PLAIN encoding, except that variable-length byte
arrays do not include a length prefix.

These fields encode min and max values determined by signed comparison
only. New files should use the correct order for a column's logical type
and store the values in the min_value and max_value fields.

To support older readers, these may be set when the column order is
signed.
"
                :initarg :max :initform nil :type (or null octet-vector))
           (min :initarg :min :initform nil :type (or null octet-vector))
           (null-count :documentation "count of null value in the column
"
            :initarg :null-count :initform nil :type
            (or null (signed-byte 64)))
           (distinct-count :documentation "count of distinct values occurring
"
            :initarg :distinct-count :initform nil :type
            (or null (signed-byte 64)))
           (max-value :documentation
            "Lower and upper bound values for the column, determined by its ColumnOrder.

These may be the actual minimum and maximum values found on a page or column
chunk, but can also be (more compact) values that do not exist on a page or
column chunk. For example, instead of storing \\Blart Versenwald III\\, a writer
may set min_value=\\B\\, max_value=\\C\\. Such more compact values must still be
valid values within the column's logical type.

Values are encoded using PLAIN encoding, except that variable-length byte
arrays do not include a length prefix.
"
            :initarg :max-value :initform nil :type (or null octet-vector))
           (min-value :initarg :min-value :initform nil :type
            (or null octet-vector))
           (is-max-value-exact :documentation
            "If true, max_value is the actual maximum value for a column
"
            :initarg :is-max-value-exact :initform nil :type (or null boolean))
           (is-min-value-exact :documentation
            "If true, min_value is the actual minimum value for a column
"
            :initarg :is-min-value-exact :initform nil :type
            (or null boolean)))
          (:documentation "Statistics per row group and per page
All fields are optional.
"))
(defclass parquet-string-type (parquet-object) nil
          (:documentation "Empty structs to use as logical type annotations
"))
(defclass parquet-uuid-type (parquet-object) nil)
(defclass parquet-map-type (parquet-object) nil)
(defclass parquet-list-type (parquet-object) nil)
(defclass parquet-enum-type (parquet-object) nil)
(defclass parquet-date-type (parquet-object) nil)
(defclass parquet-float16-type (parquet-object) nil)
(defclass parquet-null-type (parquet-object) nil
          (:documentation
           "Logical type to annotate a column that is always null.

Sometimes when discovering the schema of existing data, values are always
null and the physical type can't be determined. This annotation signals
the case where the physical type was guessed from all null values.
"))
(defclass parquet-decimal-type (parquet-object)
          ((scale :initarg :scale :type (signed-byte 32))
           (precision :initarg :precision :type (signed-byte 32)))
          (:documentation "Decimal logical type annotation

Scale must be zero or a positive integer less than or equal to the precision.
Precision must be a non-zero positive integer.

To maintain forward-compatibility in v1, implementations using this logical
type must also set scale and precision on the annotated SchemaElement.

Allowed for physical types: INT32, INT64, FIXED_LEN_BYTE_ARRAY, and BYTE_ARRAY.
"))
(defclass parquet-milli-seconds (parquet-object) nil
          (:documentation "Time units for logical types
"))
(defclass parquet-micro-seconds (parquet-object) nil)
(defclass parquet-nano-seconds (parquet-object) nil)
(defclass parquet-time-unit (parquet-object)
          ((millis :initarg :millis :initform nil :type
            (or null parquet-milli-seconds))
           (micros :initarg :micros :initform nil :type
            (or null parquet-micro-seconds))
           (nanos :initarg :nanos :initform nil :type
            (or null parquet-nano-seconds))))
(defclass parquet-timestamp-type (parquet-object)
          ((isadjustedtoutc :initarg :isadjustedtoutc :type boolean)
           (unit :initarg :unit :type parquet-time-unit))
          (:documentation "Timestamp logical type annotation

Allowed for physical types: INT64
"))
(defclass parquet-time-type (parquet-object)
          ((isadjustedtoutc :initarg :isadjustedtoutc :type boolean)
           (unit :initarg :unit :type parquet-time-unit))
          (:documentation "Time logical type annotation

Allowed for physical types: INT32 (millis), INT64 (micros, nanos)
"))
(defclass parquet-int-type (parquet-object)
          ((bitwidth :initarg :bitwidth)
           (issigned :initarg :issigned :type boolean))
          (:documentation "Integer logical type annotation

bitWidth must be 8, 16, 32, or 64.

Allowed for physical types: INT32, INT64
"))
(defclass parquet-json-type (parquet-object) nil
          (:documentation "Embedded JSON logical type annotation

Allowed for physical types: BYTE_ARRAY
"))
(defclass parquet-bson-type (parquet-object) nil
          (:documentation "Embedded BSON logical type annotation

Allowed for physical types: BYTE_ARRAY
"))
(defclass parquet-logical-type (parquet-object)
          ((string :initarg :string :initform nil :type
                   (or null parquet-string-type))
           (map :initarg :map :initform nil :type (or null parquet-map-type))
           (list :initarg :list :initform nil :type
                 (or null parquet-list-type))
           (enum :initarg :enum :initform nil :type
            (or null parquet-enum-type))
           (decimal :initarg :decimal :initform nil :type
            (or null parquet-decimal-type))
           (date :initarg :date :initform nil :type
            (or null parquet-date-type))
           (time
            :initarg
            :time
            :initform
            nil
            :type
            (or null parquet-time-type))
           (timestamp :initarg :timestamp :initform nil :type
            (or null parquet-timestamp-type))
           (integer :initarg :integer :initform nil :type
            (or null parquet-int-type))
           (unknown :initarg :unknown :initform nil :type
            (or null parquet-null-type))
           (json :initarg :json :initform nil :type
            (or null parquet-json-type))
           (bson :initarg :bson :initform nil :type
            (or null parquet-bson-type))
           (uuid :initarg :uuid :initform nil :type
            (or null parquet-uuid-type))
           (float16 :initarg :float16 :initform nil :type
            (or null parquet-float16-type)))
          (:documentation "LogicalType annotations to replace ConvertedType.

To maintain compatibility, implementations using LogicalType for a
SchemaElement must also set the corresponding ConvertedType (if any)
from the following table.
"))
(defclass parquet-schema-element (parquet-object)
          ((type :documentation
            "Data type for this field. Not set if the current element is a non-leaf node
"
            :initarg :type :initform nil :type (or null parquet-type))
           (type-length :documentation
            "If type is FIXED_LEN_BYTE_ARRAY, this is the byte length of the values.
Otherwise, if specified, this is the maximum bit length to store any of the values.
(e.g. a low cardinality INT col could have this set to 3).  Note that this is
in the schema, and therefore fixed for the entire file.
"
            :initarg :type-length :initform nil :type
            (or null (signed-byte 32)))
           (repetition-type :documentation
            "repetition of the field. The root of the schema does not have a repetition_type.
All other nodes must have one
"
            :initarg :repetition-type :initform nil :type
            (or null parquet-field-repetition-type))
           (name :documentation "Name of the field in the schema
"
            :initarg :name :type string)
           (num-children :documentation
            "Nested fields.  Since thrift does not support nested fields,
the nesting is flattened to a single list by a depth-first traversal.
The children count is used to construct the nested relationship.
This field is not set when the element is a primitive type
"
            :initarg :num-children :initform nil :type
            (or null (signed-byte 32)))
           (converted-type :documentation
            "DEPRECATED: When the schema is the result of a conversion from another model.
Used to record the original type to help with cross conversion.

This is superseded by logicalType.
"
            :initarg :converted-type :initform nil :type
            (or null parquet-converted-type))
           (scale :documentation
            "DEPRECATED: Used when this column contains decimal data.
See the DECIMAL converted type for more details.

This is superseded by using the DecimalType annotation in logicalType.
"
            :initarg :scale :initform nil :type (or null (signed-byte 32)))
           (precision :initarg :precision :initform nil :type
            (or null (signed-byte 32)))
           (field-id :documentation
            "When the original schema supports field ids, this will save the
original field id in the parquet schema
"
            :initarg :field-id :initform nil :type (or null (signed-byte 32)))
           (logicaltype :documentation "The logical type of this SchemaElement

LogicalType replaces ConvertedType, but ConvertedType is still required
for some logical types to ensure forward-compatibility in format v1.
"
            :initarg :logicaltype :initform nil :type
            (or null parquet-logical-type)))
          (:documentation "Represents a element inside a schema definition.
 - if it is a group (inner node) then type is undefined and num_children is defined
 - if it is a primitive type (leaf) then type is defined and num_children is undefined
the nodes are listed in depth first traversal order.
"))
(defclass parquet-data-page-header (parquet-object)
          ((num-values :documentation
            "Number of values, including NULLs, in this data page.

If a OffsetIndex is present, a page must begin at a row
boundary (repetition_level = 0). Otherwise, pages may begin
within a row (repetition_level > 0).

"
            :initarg :num-values :type (signed-byte 32))
           (encoding :documentation "Encoding used for this data page *
"
            :initarg :encoding :type parquet-encoding)
           (definition-level-encoding :documentation
            "Encoding used for definition levels *
"
            :initarg :definition-level-encoding :type parquet-encoding)
           (repetition-level-encoding :documentation
            "Encoding used for repetition levels *
"
            :initarg :repetition-level-encoding :type parquet-encoding)
           (statistics :documentation
            "Optional statistics for the data in this page *
"
            :initarg :statistics :initform nil :type
            (or null parquet-statistics)))
          (:documentation "Data page header
"))
(defclass parquet-index-page-header (parquet-object) nil)
(defclass parquet-dictionary-page-header (parquet-object)
          ((num-values :documentation "Number of values in the dictionary *
"
            :initarg :num-values :type (signed-byte 32))
           (encoding :documentation "Encoding using this dictionary page *
"
            :initarg :encoding :type parquet-encoding)
           (is-sorted :documentation
            "If true, the entries in the dictionary are sorted in ascending order *
"
            :initarg :is-sorted :initform nil :type (or null boolean)))
          (:documentation
           "The dictionary page must be placed at the first position of the column chunk
if it is partly or completely dictionary encoded. At most one dictionary page
can be placed in a column chunk.

"))
(defclass parquet-data-page-header-v2 (parquet-object)
          ((num-values :documentation
            "Number of values, including NULLs, in this data page. *
"
            :initarg :num-values :type (signed-byte 32))
           (num-nulls :documentation "Number of NULL values, in this data page.
Number of non-null = num_values - num_nulls which is also the number of values in the data section *
"
            :initarg :num-nulls :type (signed-byte 32))
           (num-rows :documentation
            "Number of rows in this data page. Every page must begin at a
row boundary (repetition_level = 0): rows must **not** be
split across page boundaries when using V2 data pages.

"
            :initarg :num-rows :type (signed-byte 32))
           (encoding :documentation "Encoding used for data in this page *
"
            :initarg :encoding :type parquet-encoding)
           (definition-levels-byte-length :documentation
            "Length of the definition levels
"
            :initarg :definition-levels-byte-length :type (signed-byte 32))
           (repetition-levels-byte-length :documentation
            "Length of the repetition levels
"
            :initarg :repetition-levels-byte-length :type (signed-byte 32))
           (is-compressed :documentation "Whether the values are compressed.
Which means the section of the page between
definition_levels_byte_length + repetition_levels_byte_length + 1 and compressed_page_size (included)
is compressed with the compression_codec.
If missing it is considered compressed
"
            :initarg :is-compressed :initform nil :type (or null boolean))
           (statistics :documentation
            "Optional statistics for the data in this page *
"
            :initarg :statistics :initform nil :type
            (or null parquet-statistics)))
          (:documentation
           "New page format allowing reading levels without decompressing the data
Repetition and definition levels are uncompressed
The remaining section containing the data is compressed if is_compressed is true

"))
(defclass parquet-split-block-algorithm (parquet-object) nil
          (:documentation "Block-based algorithm type annotation. *
"))
(defclass parquet-bloom-filter-algorithm (parquet-object)
          ((block :documentation
             "Block-based Bloom filter. *
"
             :initarg
             :block
             :initform
             nil
             :type
             (or null parquet-split-block-algorithm)))
          (:documentation "The algorithm used in Bloom filter. *
"))
(defclass parquet-xx-hash (parquet-object) nil
          (:documentation
           "Hash strategy type annotation. xxHash is an extremely fast non-cryptographic hash
algorithm. It uses 64 bits version of xxHash.

"))
(defclass parquet-bloom-filter-hash (parquet-object)
          ((xxhash :documentation "xxHash Strategy. *
"
            :initarg :xxhash :initform nil :type (or null parquet-xx-hash)))
          (:documentation
           "The hash function used in Bloom filter. This function takes the hash of a column value
using plain encoding.

"))
(defclass parquet-uncompressed (parquet-object) nil
          (:documentation "The compression used in the Bloom filter.

"))
(defclass parquet-bloom-filter-compression (parquet-object)
          ((uncompressed :initarg :uncompressed :initform nil :type
            (or null parquet-uncompressed))))
(defclass parquet-bloom-filter-header (parquet-object)
          ((numbytes :documentation "The size of bitset in bytes *
"
            :initarg :numbytes :type (signed-byte 32))
           (algorithm :documentation "The algorithm for setting bits. *
"
            :initarg :algorithm :type parquet-bloom-filter-algorithm)
           (hash :documentation "The hash function used for Bloom filter. *
"
            :initarg :hash :type parquet-bloom-filter-hash)
           (compression :documentation
            "The compression used in the Bloom filter *
"
            :initarg :compression :type parquet-bloom-filter-compression))
          (:documentation
           "Bloom filter header is stored at beginning of Bloom filter data of each column
and followed by its bitset.

"))
(defclass parquet-page-header (parquet-object)
          ((type :documentation
            "the type of the page: indicates which of the *_header fields is set *
"
            :initarg :type :type parquet-page-type)
           (uncompressed-page-size :documentation
            "Uncompressed page size in bytes (not including this header) *
"
            :initarg :uncompressed-page-size :type (signed-byte 32))
           (compressed-page-size :documentation
            "Compressed (and potentially encrypted) page size in bytes, not including this header *
"
            :initarg :compressed-page-size :type (signed-byte 32))
           (crc :documentation
            "The 32-bit CRC checksum for the page, to be be calculated as follows:

- The standard CRC32 algorithm is used (with polynomial 0x04C11DB7,
  the same as in e.g. GZip).
- All page types can have a CRC (v1 and v2 data pages, dictionary pages,
  etc.).
- The CRC is computed on the serialization binary representation of the page
  (as written to disk), excluding the page header. For example, for v1
  data pages, the CRC is computed on the concatenation of repetition levels,
  definition levels and column values (optionally compressed, optionally
  encrypted).
- The CRC computation therefore takes place after any compression
  and encryption steps, if any.

If enabled, this allows for disabling checksumming in HDFS if only a few
pages need to be read.
"
            :initarg :crc :initform nil :type (or null (signed-byte 32)))
           (data-page-header :initarg :data-page-header :initform nil :type
            (or null parquet-data-page-header))
           (index-page-header :initarg :index-page-header :initform nil :type
            (or null parquet-index-page-header))
           (dictionary-page-header :initarg :dictionary-page-header :initform
            nil :type (or null parquet-dictionary-page-header))
           (data-page-header-v2 :initarg :data-page-header-v2 :initform nil
            :type (or null parquet-data-page-header-v2))))
(defclass parquet-key-value (parquet-object)
          ((key :initarg :key :type string)
           (value :initarg :value :initform nil :type (or null string)))
          (:documentation "Wrapper struct to store key values
"))
(defclass parquet-sorting-column (parquet-object)
          ((column-idx :documentation
            "The ordinal position of the column (in this row group) *
"
            :initarg :column-idx :type (signed-byte 32))
           (descending :documentation
            "If true, indicates this column is sorted in descending order. *
"
            :initarg :descending :type boolean)
           (nulls-first :documentation
            "If true, nulls will come before non-null values, otherwise,
nulls go at the end.
"
            :initarg :nulls-first :type boolean))
          (:documentation "Sort order within a RowGroup of a leaf column
"))
(defclass parquet-page-encoding-stats (parquet-object)
          ((page-type :documentation "the page type (data\\dic\\...) *
"
            :initarg :page-type :type parquet-page-type)
           (encoding :documentation "encoding of the page *
"
            :initarg :encoding :type parquet-encoding)
           (count :documentation
                  "number of pages of this type with this encoding *
"
                  :initarg :count :type (signed-byte 32)))
          (:documentation "statistics of a given page type and encoding
"))
(defclass parquet-column-meta-data (parquet-object)
          ((type :documentation "Type of this column *
"
            :initarg :type :type parquet-type)
           (encodings :documentation
            "Set of all encodings used for this column. The purpose is to validate
whether we can decode those pages. *
"
            :initarg :encodings :type (vector parquet-encoding))
           (path-in-schema :documentation "Path in schema *
"
            :initarg :path-in-schema :type (vector string))
           (codec :documentation "Compression codec *
"
            :initarg :codec :type parquet-compression-codec)
           (num-values :documentation "Number of values in this column *
"
            :initarg :num-values :type (signed-byte 64))
           (total-uncompressed-size :documentation
            "total byte size of all uncompressed pages in this column chunk (including the headers) *
"
            :initarg :total-uncompressed-size :type (signed-byte 64))
           (total-compressed-size :documentation
            "total byte size of all compressed, and potentially encrypted, pages
in this column chunk (including the headers) *
"
            :initarg :total-compressed-size :type (signed-byte 64))
           (key-value-metadata :documentation "Optional key\\value metadata *
"
            :initarg :key-value-metadata :initform nil :type
            (or null (vector parquet-key-value)))
           (data-page-offset :documentation
            "Byte offset from beginning of file to first data page *
"
            :initarg :data-page-offset :type (signed-byte 64))
           (index-page-offset :documentation
            "Byte offset from beginning of file to root index page *
"
            :initarg :index-page-offset :initform nil :type
            (or null (signed-byte 64)))
           (dictionary-page-offset :documentation
            "Byte offset from the beginning of file to first (only) dictionary page *
"
            :initarg :dictionary-page-offset :initform nil :type
            (or null (signed-byte 64)))
           (statistics :documentation "optional statistics for this column chunk
"
            :initarg :statistics :initform nil :type
            (or null parquet-statistics))
           (encoding-stats :documentation
            "Set of all encodings used for pages in this column chunk.
This information can be used to determine if all data pages are
dictionary encoded for example *
"
            :initarg :encoding-stats :initform nil :type
            (or null (vector parquet-page-encoding-stats)))
           (bloom-filter-offset :documentation
            "Byte offset from beginning of file to Bloom filter data. *
"
            :initarg :bloom-filter-offset :initform nil :type
            (or null (signed-byte 64)))
           (bloom-filter-length :documentation
            "Size of Bloom filter data including the serialized header, in bytes.
Added in 2.10 so readers may not read this field from old files and
it can be obtained after the BloomFilterHeader has been deserialized.
Writers should write this field so readers can read the bloom filter
in a single I\\O.
"
            :initarg :bloom-filter-length :initform nil :type
            (or null (signed-byte 32)))
           (size-statistics :documentation
            "Optional statistics to help estimate total memory when converted to in-memory
representations. The histograms contained in these statistics can
also be useful in some cases for more fine-grained nullability\\list length
filter pushdown.
"
            :initarg :size-statistics :initform nil :type
            (or null parquet-size-statistics)))
          (:documentation "Description for column metadata
"))
(defclass parquet-encryption-with-footer-key (parquet-object) nil)
(defclass parquet-encryption-with-column-key (parquet-object)
          ((path-in-schema :documentation "Column path in schema *
"
            :initarg :path-in-schema :type (vector string))
           (key-metadata :documentation
            "Retrieval metadata of column encryption key *
"
            :initarg :key-metadata :initform nil :type (or null octet-vector))))
(defclass parquet-column-crypto-meta-data (parquet-object)
          ((encryption-with-footer-key :initarg :encryption-with-footer-key
            :initform nil :type (or null parquet-encryption-with-footer-key))
           (encryption-with-column-key :initarg :encryption-with-column-key
            :initform nil :type (or null parquet-encryption-with-column-key))))
(defclass parquet-column-chunk (parquet-object)
          ((file-path :documentation
            "File where column data is stored.  If not set, assumed to be same file as
metadata.  This path is relative to the current file.

"
            :initarg :file-path :initform nil :type (or null string))
           (file-offset :documentation
            "Deprecated: Byte offset in file_path to the ColumnMetaData

Past use of this field has been inconsistent, with some implementations
using it to point to the ColumnMetaData and some using it to point to
the first page in the column chunk. In many cases, the ColumnMetaData at this
location is wrong. This field is now deprecated and should not be used.
Writers should set this field to 0 if no ColumnMetaData has been written outside
the footer.
"
            :initarg :file-offset :type (signed-byte 64))
           (meta-data :documentation
            "Column metadata for this chunk. Some writers may also replicate this at the
location pointed to by file_path\\file_offset.
Note: while marked as optional, this field is in fact required by most major
Parquet implementations. As such, writers MUST populate this field.

"
            :initarg :meta-data :initform nil :type
            (or null parquet-column-meta-data))
           (offset-index-offset :documentation
            "File offset of ColumnChunk's OffsetIndex *
"
            :initarg :offset-index-offset :initform nil :type
            (or null (signed-byte 64)))
           (offset-index-length :documentation
            "Size of ColumnChunk's OffsetIndex, in bytes *
"
            :initarg :offset-index-length :initform nil :type
            (or null (signed-byte 32)))
           (column-index-offset :documentation
            "File offset of ColumnChunk's ColumnIndex *
"
            :initarg :column-index-offset :initform nil :type
            (or null (signed-byte 64)))
           (column-index-length :documentation
            "Size of ColumnChunk's ColumnIndex, in bytes *
"
            :initarg :column-index-length :initform nil :type
            (or null (signed-byte 32)))
           (crypto-metadata :documentation
            "Crypto metadata of encrypted columns *
"
            :initarg :crypto-metadata :initform nil :type
            (or null parquet-column-crypto-meta-data))
           (encrypted-column-metadata :documentation
            "Encrypted column metadata for this chunk *
"
            :initarg :encrypted-column-metadata :initform nil :type
            (or null octet-vector))))
(defclass parquet-row-group (parquet-object)
          ((columns :documentation
            "Metadata for each column chunk in this row group.
This list must have the same order as the SchemaElement list in FileMetaData.

"
            :initarg :columns :type (vector parquet-column-chunk))
           (total-byte-size :documentation
            "Total byte size of all the uncompressed column data in this row group *
"
            :initarg :total-byte-size :type (signed-byte 64))
           (num-rows :documentation "Number of rows in this row group *
"
            :initarg :num-rows :type (signed-byte 64))
           (sorting-columns :documentation
            "If set, specifies a sort ordering of the rows in this RowGroup.
The sorting columns can be a subset of all the columns.
"
            :initarg :sorting-columns :initform nil :type
            (or null (vector parquet-sorting-column)))
           (file-offset :documentation
            "Byte offset from beginning of file to first page (data or dictionary)
in this row group *
"
            :initarg :file-offset :initform nil :type
            (or null (signed-byte 64)))
           (total-compressed-size :documentation
            "Total byte size of all compressed (and potentially encrypted) column data
in this row group *
"
            :initarg :total-compressed-size :initform nil :type
            (or null (signed-byte 64)))
           (ordinal :documentation "Row group ordinal in the file *
"
            :initarg :ordinal :initform nil :type (or null (signed-byte 16)))))
(defclass parquet-type-defined-order (parquet-object) nil
          (:documentation
           "Empty struct to signal the order defined by the physical or logical type
"))
(defclass parquet-column-order (parquet-object)
          ((type-order :documentation "The sort orders for logical types are:
  UTF8 - unsigned byte-wise comparison
  INT8 - signed comparison
  INT16 - signed comparison
  INT32 - signed comparison
  INT64 - signed comparison
  UINT8 - unsigned comparison
  UINT16 - unsigned comparison
  UINT32 - unsigned comparison
  UINT64 - unsigned comparison
  DECIMAL - signed comparison of the represented value
  DATE - signed comparison
  TIME_MILLIS - signed comparison
  TIME_MICROS - signed comparison
  TIMESTAMP_MILLIS - signed comparison
  TIMESTAMP_MICROS - signed comparison
  INTERVAL - undefined
  JSON - unsigned byte-wise comparison
  BSON - unsigned byte-wise comparison
  ENUM - unsigned byte-wise comparison
  LIST - undefined
  MAP - undefined

In the absence of logical types, the sort order is determined by the physical type:
  BOOLEAN - false, true
  INT32 - signed comparison
  INT64 - signed comparison
  INT96 (only used for legacy timestamps) - undefined
  FLOAT - signed comparison of the represented value (*)
  DOUBLE - signed comparison of the represented value (*)
  BYTE_ARRAY - unsigned byte-wise comparison
  FIXED_LEN_BYTE_ARRAY - unsigned byte-wise comparison

(*) Because the sorting order is not specified properly for floating
    point values (relations vs. total ordering) the following
    compatibility rules should be applied when reading statistics:
    - If the min is a NaN, it should be ignored.
    - If the max is a NaN, it should be ignored.
    - If the min is +0, the row group may contain -0 values as well.
    - If the max is -0, the row group may contain +0 values as well.
    - When looking for NaN values, min and max should be ignored.

    When writing statistics the following rules should be followed:
    - NaNs should not be written to min or max statistics fields.
    - If the computed max value is zero (whether negative or positive),
      `+0.0` should be written into the max statistics field.
    - If the computed min value is zero (whether negative or positive),
      `-0.0` should be written into the min statistics field.
"
            :initarg :type-order :initform nil :type
            (or null parquet-type-defined-order)))
          (:documentation
           "Union to specify the order used for the min_value and max_value fields for a
column. This union takes the role of an enhanced enum that allows rich
elements (which will be needed for a collation-based ordering in the future).

Possible values are:
* TypeDefinedOrder - the column uses the order defined by its logical or
                     physical type (if there is no logical type).

If the reader does not support the value of this union, min and max stats
for this column should be ignored.
"))
(defclass parquet-page-location (parquet-object)
          ((offset :documentation "Offset of the page in the file *
"
            :initarg :offset :type (signed-byte 64))
           (compressed-page-size :documentation
            "Size of the page, including header. Sum of compressed_page_size and header
length
"
            :initarg :compressed-page-size :type (signed-byte 32))
           (first-row-index :documentation
            "Index within the RowGroup of the first row of the page. When an
OffsetIndex is present, pages must begin on row boundaries
(repetition_level = 0).
"
            :initarg :first-row-index :type (signed-byte 64))))
(defclass parquet-offset-index (parquet-object)
          ((page-locations :documentation
            "PageLocations, ordered by increasing PageLocation.offset. It is required
that page_locations[i].first_row_index < page_locations[i+1].first_row_index.
"
            :initarg :page-locations :type (vector parquet-page-location))
           (unencoded-byte-array-data-bytes :documentation
            "Unencoded\\uncompressed size for BYTE_ARRAY types.

See documention for unencoded_byte_array_data_bytes in SizeStatistics for
more details on this field.
"
            :initarg :unencoded-byte-array-data-bytes :initform nil :type
            (or null (vector (signed-byte 64)))))
          (:documentation "Optional offsets for each data page in a ColumnChunk.

Forms part of the page index, along with ColumnIndex.

OffsetIndex may be present even if ColumnIndex is not.
"))
(defclass parquet-column-index (parquet-object)
          ((null-pages :documentation
            "A list of Boolean values to determine the validity of the corresponding
min and max values. If true, a page contains only null values, and writers
have to set the corresponding entries in min_values and max_values to
byte[0], so that all lists have the same length. If false, the
corresponding entries in min_values and max_values must be valid.
"
            :initarg :null-pages :type (vector boolean))
           (min-values :documentation
            "Two lists containing lower and upper bounds for the values of each page
determined by the ColumnOrder of the column. These may be the actual
minimum and maximum values found on a page, but can also be (more compact)
values that do not exist on a page. For example, instead of storing \\\\Blart
Versenwald III\\, a writer may set min_values[i]=\\B\\, max_values[i]=\\C\\.
Such more compact values must still be valid values within the column's
logical type. Readers must make sure that list entries are populated before
using them by inspecting null_pages.
"
            :initarg :min-values :type (vector octet-vector))
           (max-values :initarg :max-values :type (vector octet-vector))
           (boundary-order :documentation
            "Stores whether both min_values and max_values are ordered and if so, in
which direction. This allows readers to perform binary searches in both
lists. Readers cannot assume that max_values[i] <= min_values[i+1], even
if the lists are ordered.
"
            :initarg :boundary-order :type parquet-boundary-order)
           (null-counts :documentation
            "A list containing the number of null values for each page *
"
            :initarg :null-counts :initform nil :type
            (or null (vector (signed-byte 64))))
           (repetition-level-histograms :documentation
            "Contains repetition level histograms for each page
concatenated together.  The repetition_level_histogram field on
SizeStatistics contains more details.

When present the length should always be (number of pages *
(max_repetition_level + 1)) elements.

Element 0 is the first element of the histogram for the first page.
Element (max_repetition_level + 1) is the first element of the histogram
for the second page.

"
            :initarg :repetition-level-histograms :initform nil :type
            (or null (vector (signed-byte 64))))
           (definition-level-histograms :documentation
            "Same as repetition_level_histograms except for definitions levels.

"
            :initarg :definition-level-histograms :initform nil :type
            (or null (vector (signed-byte 64)))))
          (:documentation
           "Optional statistics for each data page in a ColumnChunk.

Forms part the page index, along with OffsetIndex.

If this structure is present, OffsetIndex must also be present.

For each field in this structure, <field>[i] refers to the page at
OffsetIndex.page_locations[i]
"))
(defclass parquet-aes-gcm-v1 (parquet-object)
          ((aad-prefix :documentation "AAD prefix *
"
            :initarg :aad-prefix :initform nil :type (or null octet-vector))
           (aad-file-unique :documentation
            "Unique file identifier part of AAD suffix *
"
            :initarg :aad-file-unique :initform nil :type
            (or null octet-vector))
           (supply-aad-prefix :documentation
            "In files encrypted with AAD prefix without storing it,
readers must supply the prefix *
"
            :initarg :supply-aad-prefix :initform nil :type (or null boolean))))
(defclass parquet-aes-gcm-ctr-v1 (parquet-object)
          ((aad-prefix :documentation "AAD prefix *
"
            :initarg :aad-prefix :initform nil :type (or null octet-vector))
           (aad-file-unique :documentation
            "Unique file identifier part of AAD suffix *
"
            :initarg :aad-file-unique :initform nil :type
            (or null octet-vector))
           (supply-aad-prefix :documentation
            "In files encrypted with AAD prefix without storing it,
readers must supply the prefix *
"
            :initarg :supply-aad-prefix :initform nil :type (or null boolean))))
(defclass parquet-encryption-algorithm (parquet-object)
          ((aes-gcm-v1 :initarg :aes-gcm-v1 :initform nil :type
            (or null parquet-aes-gcm-v1))
           (aes-gcm-ctr-v1 :initarg :aes-gcm-ctr-v1 :initform nil :type
            (or null parquet-aes-gcm-ctr-v1))))
(defclass parquet-file-meta-data (parquet-object)
          ((version :documentation "Version of this file *
"
            :initarg :version :type (signed-byte 32))
           (schema :documentation
            "Parquet schema for this file.  This schema contains metadata for all the columns.
The schema is represented as a tree with a single root.  The nodes of the tree
are flattened to a list by doing a depth-first traversal.
The column metadata contains the path in the schema for that column which can be
used to map columns to nodes in the schema.
The first element is the root *
"
            :initarg :schema :type (vector parquet-schema-element))
           (num-rows :documentation "Number of rows in this file *
"
            :initarg :num-rows :type (signed-byte 64))
           (row-groups :documentation "Row groups in this file *
"
            :initarg :row-groups :type (vector parquet-row-group))
           (key-value-metadata :documentation "Optional key\\value metadata *
"
            :initarg :key-value-metadata :initform nil :type
            (or null (vector parquet-key-value)))
           (created-by :documentation
            "String for application that wrote this file.  This should be in the format
<Application> version <App Version> (build <App Build Hash>).
e.g. impala version 1.0 (build 6cf94d29b2b7115df4de2c06e2ab4326d721eb55)

"
            :initarg :created-by :initform nil :type (or null string))
           (column-orders :documentation
            "Sort order used for the min_value and max_value fields in the Statistics
objects and the min_values and max_values fields in the ColumnIndex
objects of each column in this file. Sort orders are listed in the order
matching the columns in the schema. The indexes are not necessary the same
though, because only leaf nodes of the schema are represented in the list
of sort orders.

Without column_orders, the meaning of the min_value and max_value fields
in the Statistics object and the ColumnIndex object is undefined. To ensure
well-defined behaviour, if these fields are written to a Parquet file,
column_orders must be written as well.

The obsolete min and max fields in the Statistics object are always sorted
by signed comparison regardless of column_orders.
"
            :initarg :column-orders :initform nil :type
            (or null (vector parquet-column-order)))
           (encryption-algorithm :documentation
            "Encryption algorithm. This field is set only in encrypted files
with plaintext footer. Files with encrypted footer store algorithm id
in FileCryptoMetaData structure.
"
            :initarg :encryption-algorithm :initform nil :type
            (or null parquet-encryption-algorithm))
           (footer-signing-key-metadata :documentation
            "Retrieval metadata of key used for signing the footer.
Used only in encrypted files with plaintext footer.
"
            :initarg :footer-signing-key-metadata :initform nil :type
            (or null octet-vector)))
          (:documentation "Description for file metadata
"))
(defclass parquet-file-crypto-meta-data (parquet-object)
          ((encryption-algorithm :documentation
            "Encryption algorithm. This field is only used for files
with encrypted footer. Files with plaintext footer store algorithm id
inside footer (FileMetaData structure).
"
            :initarg :encryption-algorithm :type parquet-encryption-algorithm)
           (key-metadata :documentation
            "Retrieval metadata of key used for encryption of footer,
and (possibly) columns *
"
            :initarg :key-metadata :initform nil :type (or null octet-vector)))
          (:documentation "Crypto metadata for files with encrypted footer *
"))
