(ns finn-chamber.levels
  (:require [finn-chamber.util :refer [deep-merge]]))


(def lever? #{:A :B :C :D :E :F :G :H :I :J :K})
(def barrier? #{:$ :| :1 :2 :3 :4 :5 :6 :7 :8 :9 :0 :#})
(def item-chars {\$ :finn \| :wall \A :A \B :B \C :C \D :D \E :E \F \F \G :G \H :H \I :I \J :J \K :K})

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

(defn parse-barriers [rows]
  (->> (map-grid (fn [x y char]
                   (let [group (keyword char)]
                     (when (barrier? group)
                       {:x x
                        :y y
                        :group group}))
                   ) rows)
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

(defn parse-level-spec [spec]
  (let [pairs (partition 2 2 [(repeat 80 \ )] spec)
        floor-rows (map first pairs)
        item-rows (map last pairs)]
    {:floors (parse-barriers floor-rows)
     :walls (parse-barriers item-rows)
     :levers (parse-levers item-rows)}))

(defn level [spec & {:as props}]
  (deep-merge (parse-level-spec spec)
              props))

(def levels
  [(level
    ["################################################################################"
     "|                                                                      A       $"
     "#                                                               ################"
     "|                                                                              |"
     "#                                                 ####111111####               #"
     "|                                                                              |"
     "#                                                                              #"
     "|                                                                              |"
     "#                                                                              #"
     "|                                                                              |"
     "#                                                                              #"
     "1                                                                              |"
     "################################################################################"]
    :visible #{:# :| :1 :4 :3 :5}
    :levers {:A {:hides [#{:1} #{}]}})

   (level
    ["################################################################################"
     "|                                                                    A         $"
     "#                                                                              |"
     "|                                                                              |"
     "#                                                 ####111111####               #"
     "|                                                                              |"
     "#                                                                              #"
     "|                                                                              |"
     "#      #####                                                                   #"
     "|                                                                              |"
     "#                                                                              #"
     "1                                                                              |"
     "################################################################################"]
    :visible #{:# :| :1 :4 :3 :5}
    :levers {:A {:hides [#{:1} #{}]}})])

["################################################################################"
    "|                         A                    |||            E                |"
    "###########################################4444##########################11111##"
    "|                                                                    B         |"
    "####22222#################33333#################################################"
    "|                     ||||                                                     |"
    "####                  ##########################################################"
    "|     C               5555                       |                             |"
    "##############        ##########################################################"
    "|     D               ||||                                                     |"
    "#################################################################        #######"
    "|              F                 |                   G                         |"
    "################################################################################"]
