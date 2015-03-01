## Erlang CBOR encoder/decoder

### Requirements

 - Erlang 17.0 or higher: cbor-erlang uses erlang maps to represent map, which
   introduced from 17.0.

### Highlights

 - Optimized: no sub-binary is created while decoding, hot code path is built
   with single jump table built by pattern matching.
 - Tested: contains all testcases on standard documents.
 - Correct floating-point implemntation: can decode floating points that are
   not supported by language: half-presition floating points, NaN,
   (+-)Infinity.

### Decoder mappings

#### Genral

 - Both byte string and UTF-8 string is mapped to binary.
 - All numbers(integers, bignums, floating points) are mapped to number.
 - Lists are mapped to plain lists, maps are mapped to maps.

#### Tags

 - Bignums are decoded to numbers.
 - Date/time string (tag 0) is mapped to {timetext, Binary}
 - Epoch-based date/time (tag 1) is mapped to {timeepoch, Number}
 - Other tags are mapped to {tag, TagId, Value}


### Encoder mappings

Encoder encodes erlang terms to CBOR-encoded iolist. `iolist_to_binary` could
be used if you need single binary output.

 - Numbers (fixed-size numbers, big numbers, floating-point numbers) are supported.
 - All binaries are encoded to byte strings.
 - Atoms `true`, `false`, `null`, `undefined` are suported.
 - All lists/maps are encoded to break-terminating format.


### TODO

 - cbor-erlang does not distingiush byte string and UTF-8 string while
   decoding. Some invalid CBOR tagged values could be decoded without problem,
   for example date/time string (tag 0) with byte string data items. The bug
   should be fixed later.
 - No error reporting on encoding/decoding: no detailed error information is
   provided to caller, for example why encoding/decoding is failed or which
   byte decoder failed to decode.
 - Decimal fraction (tag 3) and bigfloat (tag 4) is not supported.
 - Add faster encoder which generates non-canonical CBOR data.
