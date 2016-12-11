(ns finn-chamber.levels
  (:require [finn-chamber.util :refer [deep-merge]]))


(def lever? #{:A :B :C :D :E :F :G :H :I :J :K})
(def barrier? #{:$ :| :1 :2 :3 :4 :5 :6 :7 :8 :9 :0 :# :a :b})
(def item-chars {\$ :finn \| :wall \A :A \B :B \C :C \D :D \E :E \F :F \G :G \H :H \I :I \J :J \K :K})

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
     "|     C   6                                                     1     A        $"
     "###########                                                     ###########3333#"
     "|         |                                                     2     |        |"
     "#22222#########                                   ##############222222#333333###"
     "|             |                                   2                            |"
     "#22222222###########               #############2222222222222222222222#####3333#"
     "|                                  2                                           |"
     "#                   #############2222222222222222222222222222222222222#3333#####"
     "|                   2222                                              |        |"
     "#    444444444444   2222                                              ###333333#"
     "|    4     B    42222222                                              |        4"
     "################################################################################"]
    :visible #{:# :| :1 :4 :3 :5}
    :levers {:A {:hides [#{:1} #{:9}]}
             :B {:hides [#{:3} #{:6 :2}]}
             :C {:hides [#{:4} #{}]}})

   (level
    ["################################################################################"
     "5         5                  C           D                      4              #"
     "###########                77777       77777                    ###########    #"
     "|         0                                                F    |              |"
     "##############                  7777777           #####################33333####"
     "|            0                                    b               9            |"
     "##################                 ########################################4444#"
     "|                0                 a                              0            |"
     "#7777777777777777777#####################11111#########################3333#####"
     "|                   ||||                                          0   |        |"
     "#                   ####                 66666                    0   ###    99#"
     "$          A     |||||||                                       B  0   3    E   #"
     "################################################################################"]

    :visible #{:# :| :1 :2 :3 :4 :5 :a}
    :levers {:A {:hides [#{:1} #{:6}]}
             :B {:hides [#{:2} #{:7}]}
             :C {:hides [#{:3} #{:8}]}
             :D {:hides [#{:4} #{:9}]}
             :E {:hides [#{:5} #{:0}]}
             :F {:hides [#{:a} #{:b}]}
             }
    )
   ]
  )
