(ns passel.time.parser)

(defn custom-datetime [dt]
  (let [mx (re-matches #"^([0-9]{4})-(0[1-9]|1[0-2])-(0[1-9]|[1-2][0-9]|3[0-1]) (2[0-3]|[01][0-9]):([0-5][0-9]):([0-5][0-9])$" dt)]
    (->> (rest mx) (map #(Integer/parseInt %)))))
