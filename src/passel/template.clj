(ns passel.template)

(defn format-template
  "A cheap mustache like template formatter, e.g.
  (format-template \"Hello {name}, welcome to {place}\"
  {:name \"foo\" :place \"Earth\"})
  => \"Hello foo, welcome to Earth\""
  [^String text replacements & {:keys [pattern]}]
  (let [pattern (or pattern #"\{([^}]*)\}")
        matcher (re-matcher pattern text)]
    (loop [i 0
           s ""
           m (.find matcher)]
      (if m
        (recur (.end matcher)
               (str s
                    (.substring text i (.start matcher))
                    (let [key (.group matcher 1)]
                      (or (get replacements (keyword key))
                          (get replacements key))))
               (.find matcher))
        (str s (.substring text i (count text)))))))
