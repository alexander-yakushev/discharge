(ns discharge.utils
  (:require [clojure.tools.logging :refer [error]])
  (:import java.text.SimpleDateFormat))

(defn fail [msg]
  (error msg)
  (System/exit 1))

(defn format-date [date & [from? to?]]
  (let [[date to] (if (instance? String date)
                   [(.parse (SimpleDateFormat. from?) date) to?]
                   [date from?])]
    (if to
      (.format (SimpleDateFormat. to) date)
      date)))
