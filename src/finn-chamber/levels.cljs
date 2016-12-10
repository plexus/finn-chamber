(ns finn-chamber.levels)


(def lever? #{:A :B :C :D :E :F :G :H :I :J :K})
(def item-chars {\| :wall \A :A \B :B \C :C \D :D \E :E \F \F \G :G \H :H \I :I \J :J \K :K})

(defn parse-floor [s]
  (map {\# true \  false} s))

(defn parse-items [s]
  (->> s
       (map-indexed (fn [idx x]
              [idx (get item-chars x)]))
       (remove #(nil? (second %)))
       (into {})))

(defn parse-levers [item-rows]
  (->> (for [y (range (count item-rows))
             x (range (count (first item-rows)))]
         (let [item-row (nth item-rows y)
               item-char (nth item-row x)
               item (get item-chars item-char)]
           (if (lever? item)
             [item {:x x :y y}])))
       (remove nil?)
       (into {})))

(defn parse-walls [item-rows]
  (->> (for [y (range (count item-rows))
             x (range (count (first item-rows)))]
         (let [item-row (nth item-rows y)
               item-char (nth item-row x)
               item (get item-chars item-char)]
           (if (= :wall item)
             [x y])))
       (remove nil?)))

(defn parse-level-spec [spec]
  (let [pairs (partition 2 2 [(repeat 80 \ )] spec)
        floor-rows (map first pairs)
        item-rows (map last pairs)]
    {:floors (map parse-floor floor-rows)
     :levers (parse-levers item-rows)
     :walls (parse-walls item-rows)}))



(def level-1 (parse-level-spec ["################################################################################"
                                "|                         A                                                    |"
                                "#######          ###############################################################"
                                "|                                                                    B         |"
                                "##################################################          ####################"
                                "|                                                                              |"
                                "###        ########################        #############################     ###"
                                "|     C               |                                                        |"
                                "##############     ################        #####################################"
                                "|     D               |                                                        |"
                                "#################################################################        #######"
                                "|                                                                              |"
                                "################################################################################"]))
