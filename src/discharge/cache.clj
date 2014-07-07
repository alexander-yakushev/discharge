(ns discharge.cache
  (:refer-clojure :exclude [drop])
  (:require [clojure.core.cache :as cache]))

(def ^:private doc-cache (atom nil))

(defn clear []
  (reset! doc-cache (cache/fifo-cache-factory {} :threshold 100)))

(clear)

(defn lookup [f]
  (cache/lookup @doc-cache (.getCanonicalFile f)))

(defn add [f doc]
  (swap! doc-cache cache/miss
         (.getCanonicalFile f) doc))

(defn drop [f]
  (swap! doc-cache cache/evict (.getCanonicalFile f)))
