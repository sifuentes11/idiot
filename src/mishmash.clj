(ns mishmash)

(defn pascal [rowData endRow currentRow]

  (if (< endRow 0)
    (println "invalid input")
    (if (= endRow currentRow)
      (do
        (dotimes [u (- (count rowData) 1)]
          (print (get rowData u) ""))
        (println (get rowData (- (count rowData) 1))))
      (if (= currentRow 0)
        (pascal [1 1] endRow (inc currentRow))
        (do
          (def newRowData [1])
          (dotimes [i currentRow]
            (def newRowData (conj newRowData (+ (get rowData i) (get rowData (+ i 1))))))
          (def newRowData (conj newRowData 1))
          (recur newRowData endRow (inc currentRow))
          ;(pascal newRowData endRow (inc currentRow))
          )))))

(defn write-roman [num]

  (if (or (<= (Integer/parseInt num) 0) (>= (Integer/parseInt num) 4000))
    (println "invalid input")
    (do
      (def val1 (Integer/parseInt num))
      (while (>= val1 1000)
        (print "M")
        (def val1 (- val1 1000)))
      (if (>= val1 900)
        (do
          (print "CM")
          (def val1 (- val1 900)))
        ())
      (if (>= val1 500)
        (do
          (print "D")
          (def val1 (- val1 500)))
        ())
      (if (>= val1 400)
        (do
          (print "CD")
          (def val1 (- val1 400)))
        ())
      (while (>= val1 100)
        (do
          (print "C")
          (def val1 (- val1 100))))

      (if (>= val1 90)
        (do
          (print "XC")
          (def val1 (- val1 90)))
        ())
      (if (>= val1 50)
        (do
          (print "L")
          (def val1 (- val1 50)))
        ())
      (if (>= val1 40)
        (do
          (print "XL")
          (def val1 (- val1 40)))
        ())
      (while (>= val1 10)
        (do
          (print "X")
          (def val1 (- val1 10))))

      (if (>= val1 9)
        (do
          (print "IX")
          (def val1 (- val1 9)))
        ())
      (if (>= val1 5)
        (do
          (print "V")
          (def val1 (- val1 5)))
        ())
      (if (>= val1 4)
        (do
          (print "IV")
          (def val1 (- val1 4)))
        ())
      (while (>= val1 1)
        (do
          (print "I")
          (def val1 (- val1 1))))

      (println ""))))

(defn read-roman [numerals]
  (def start 0)
  (def end 2)
  (def total 0)
  (def found 0)

  (while (or (< found (count numerals)))
    (if (= (count numerals) (+ found 1))
      (do
        (def end (- end 1)))
      ())
    (def current (subs numerals start end))
    (if (= current "CM")
      (do
        (def total (+ total 900))
        (def start (+ start 2))
        (def end (+ end 2))
        (def found (+ found 2)))
      (if (= current "CD")
        (do
          (def total (+ total 400))
          (def start (+ start 2))
          (def end (+ end 2))
          (def found (+ found 2)))

        (if (= current "XC")
          (do
            (def total (+ total 90))
            (def start (+ start 2))
            (def end (+ end 2))
            (def found (+ found 2)))

          (if (= current "XL")
            (do
              (def total (+ total 40))
              (def start (+ start 2))
              (def end (+ end 2))
              (def found (+ found 2)))

            (if (= current "IX")
              (do
                (def total (+ total 9))
                (def start (+ start 2))
                (def end (+ end 2))
                (def found (+ found 2)))
              (if (= current "IV")
                (do
                  (def total (+ total 4))
                  (def start (+ start 2))
                  (def end (+ end 2))
                  (def found (+ found 2)))
                (do
                  (def current (subs current 0 1))
                  (if (= current "M")
                    (do
                      (def total (+ total 1000))
                      (def start (+ start 1))
                      (def end (+ end 1))
                      (def found (+ found 1)))
                    (if (= current "D")
                      (do
                        (def total (+ total 500))
                        (def start (+ start 1))
                        (def end (+ end 1))
                        (def found (+ found 1)))
                      (if (= current "C")
                        (do
                          (def total (+ total 100))
                          (def start (+ start 1))
                          (def end (+ end 1))
                          (def found (+ found 1)))
                        (if (= current "L")
                          (do
                            (def total (+ total 50))
                            (def start (+ start 1))
                            (def end (+ end 1))
                            (def found (+ found 1)))
                          (if (= current "X")
                            (do
                              (def total (+ total 10))
                              (def start (+ start 1))
                              (def end (+ end 1))
                              (def found (+ found 1)))
                            (if (= current "V")
                              (do
                                (def total (+ total 5))
                                (def start (+ start 1))
                                (def end (+ end 1))
                                (def found (+ found 1)))
                              (if (= current "I")
                                (do
                                  (def total (+ total 1))
                                  (def start (+ start 1))
                                  (def end (+ end 1))
                                  (def found (+ found 1)))
                                (do
                                  (println "invalid input")
                                  (System/exit 0)))))))))))))))))

  (println total))

(defn -main [& args]
  (if (or (> (count args) 2) (< (count args) 2))
    ((println "invalid input")
     (System/exit 0))
    ())

  (if (= (nth args 0) "pascal")
    (try
      (do
        (Integer/parseInt (nth args 1))
        (pascal [1] (Integer/parseInt (nth args 1)) 0))
      (catch Exception e
        (println "invalid input")
        (System/exit 0)))

    (if (= (nth args 0) "write-roman")
      (try
        (do
          (Integer/parseInt (nth args 1))
          (write-roman (nth args 1)))
        (catch Exception e
          (println "invalid input")
          (System/exit 0)))

      (if (= (nth args 0) "read-roman")
        (read-roman (nth args 1))
        (println "invalid input")))))


