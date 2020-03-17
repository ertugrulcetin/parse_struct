(ns parse_struct.common-types)

(def i8 {:type   :int
         :bytes  1
         :signed true})

(def u8 {:type   :int
         :bytes  1
         :signed false})

(def i16 {:type   :int
          :bytes  2
          :signed true})

(def u16 {:type   :int
          :bytes  2
          :signed false})

(def i32 {:type   :int
          :bytes  4
          :signed true})

(def u32 {:type   :int
          :bytes  4
          :signed false})

(def name8 {:type       :string
            :bytes      8
            :trim_nulls true})

