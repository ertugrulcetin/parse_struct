# parse_struct

Parse C struct dumps in clojure.

```
(:require [parse_struct.core :refer [parse]
          [parse_struct.common_types :as ct)

(parse ct/i16 byte-seq)
; short integer

(parse {:type    :array
        :len     20
        :element ct/u32}
       byte-seq)
; lazyseq of unsigned integers (long actually) since java doesn't have unsigned

(parse {:type        :struct
         :definition [[:a ct/i32]
                      [:b {:type       :string
                           :bytes      8
                           :trim_nulls true}]]})
; a struct

; strings are parsed like this:
(new String <bytes seq> "ASCII")
; :trim_nulls = true returns characters upto first null character,
; :trim_nulls = false keeps them in string 
```

Structs and arrays can be arbitrarily nested.

### TODO:

* Configurable endianness
* serializing

### Note:

You'll need to have `rustc` installed in order to run tests.