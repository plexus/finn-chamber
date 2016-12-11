(ns finn-chamber.util)

(defn map-keys [f m]
  (zipmap
   (map f (keys m))
   (vals m)))

(defn map-vals [f m]
  (zipmap
   (keys m)
   (map f (vals m))))

(defn deep-merge
  "Like merge, but merges maps recursively."
  [& maps]
  (if (every? map? maps)
    (apply merge-with deep-merge maps)
    (last maps)))
