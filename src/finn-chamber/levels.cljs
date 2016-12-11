(ns finn-chamber.levels
  (:require [finn-chamber.util :refer [deep-merge]]))


(def lever? #{:A :B :C :D :E :F :G :H :I :J :K})
(def item-chars {\| :wall \A :A \B :B \C :C \D :D \E :E \F \F \G :G \H :H \I :I \J :J \K :K})

(defn map-grid
  "map over a seq of equally sized strings. Yields [x y char]."
  [f grid]
  (for [y (range (count grid))
        x (range (count (first grid)))]
    (let [row (nth grid y)
          char (nth row x)]
      (f x y char))))

(defn parse-items [s]
  (->> s
       (map-indexed (fn [idx x]
              [idx (get item-chars x)]))
       (remove #(nil? (second %)))
       (into {})))

(defn parse-floors [floor-rows]
  (->> (map-grid (fn [x y char]
                   (when-not (= char \ )
                     {:x x
                      :y y
                      :group (keyword (str char))})
                   ) floor-rows)
       (remove nil?)
       (into [])))

(defn parse-levers [item-rows]
  (->> (map-grid (fn [x y char]
                   (let [item (get item-chars char)]
                     (if (lever? item)
                       [item {:x x :y y :state :left :name item :hides [#{} #{}]}])))
                 item-rows)
       (remove nil?)
       (into {})))

(defn parse-walls [item-rows]
  (->>

   (for [y (range (count item-rows))
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
    {:floors (parse-floors floor-rows)
     :levers (parse-levers item-rows)
     :walls (parse-walls item-rows)}))

(defn level [spec & {:as props}]
  (deep-merge (parse-level-spec spec)
              props))

(def level-1
  (level
   ["################################################################################"
    "|                         A                    |||            E                |"
    "###########################################4444##########################11111##"
    "|                                                                    B         |"
    "####22222#################33333#################################################"
    "|                     ||||                                                     |"
    "####                  ##################################################     ###"
    "|     C               ||||                       |                             |"
    "##############        #############        #####################################"
    "|     D               ||||                                                     |"
    "#################################################################        #######"
    "|              F                 |                   G                         |"
    "################################################################################"]
   :visible #{:# :1 :4 :3}
   :levers {:E {:state :right
                :hides [#{} #{:1}]}
            :B {:hides [#{} #{}]}
            :C {:hides [#{:4} #{}]}
            :A {:hides [#{:3} #{}]}
            :D {:hides [#{} #{}]}}))
